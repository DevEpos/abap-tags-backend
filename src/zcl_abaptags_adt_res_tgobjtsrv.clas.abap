"! <p class="shorttext synchronized" lang="en">Resource for Tagged object tree services</p>
CLASS zcl_abaptags_adt_res_tgobjtsrv DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES BEGIN OF ty_tag_infos.
    INCLUDE TYPE zabaptags_tag_data.
    TYPES has_children TYPE abap_bool.
    TYPES END OF ty_tag_infos.

    TYPES:
      ty_tag_data_sorted TYPE SORTED TABLE OF zabaptags_tag_data WITH UNIQUE KEY tag_id.

    DATA:
      object_name_range        TYPE RANGE OF sobj_name,
      object_type_range        TYPE RANGE OF trobjtype,
      parent_object_name_range TYPE RANGE OF sobj_name,
      parent_object_type_range TYPE RANGE OF trobjtype,
      search_params            TYPE zabaptags_tgobj_search_params,
      owner_range              TYPE RANGE OF sy-uname,
      tree_result              TYPE zabaptags_tgobj_tree_result,
      matching_tags            TYPE zabaptags_tag_data_t,
      tag_infos                TYPE ty_tag_infos,
      tag_id_range             TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    METHODS:
      determine_obj_name_range
        IMPORTING
          query TYPE string
        RAISING
          cx_adt_rest,
      get_parameters
        IMPORTING
          io_request TYPE REF TO if_adt_rest_request
        RAISING
          cx_adt_rest,
      get_response_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_request_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_matching_tags,
      get_matching_objects,
      fill_descriptions,
      get_shared_tags
        RETURNING
          VALUE(result) TYPE zif_abaptags_ty_global=>ty_tag_id_range,
      read_first_level_tags,
      read_sub_level_tags,
      retrieve_addtnl_input_infos,
      enhance_first_level_tags,
      read_sub_level_objs_with_tags,
      get_tags_in_hierarchy
        RETURNING
          VALUE(result) TYPE ty_tag_data_sorted.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobjtsrv IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = search_params ).

    get_parameters( request ).
    retrieve_addtnl_input_infos( ).
    " retrieves tags at current level that have object or child tags
    get_matching_tags( ).
    " retrieves objects at the current level
    get_matching_objects( ).

    response->set_body_data(
      content_handler = get_response_content_handler( )
      data            = tree_result ).
  ENDMETHOD.


  METHOD get_parameters.
    DATA(scope) = COND #(
      WHEN search_params-search_scope IS INITIAL THEN zif_abaptags_c_global=>scopes-all
      ELSE search_params-search_scope ).
    owner_range = SWITCH #(  scope
      WHEN zif_abaptags_c_global=>scopes-global THEN VALUE #( ( sign = 'I' option = 'EQ' low = space ) )
      WHEN zif_abaptags_c_global=>scopes-user   THEN VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ) ).

    IF search_params-query IS NOT INITIAL.
      determine_obj_name_range( search_params-query ).
    ENDIF.

    tag_id_range = VALUE #( FOR tag_id IN search_params-tag_id ( sign = 'I' option = 'EQ' low = tag_id ) ).
  ENDMETHOD.


  METHOD determine_obj_name_range.
    DATA: obj_name_type  TYPE TABLE OF string,
          obj_name_range LIKE object_name_range,
          obj_type_range LIKE object_type_range.

    IF search_params-query_type = zif_abaptags_c_global=>tag_query_types-object_uri.
      zcl_abaptags_adt_util=>map_uri_to_wb_object(
        EXPORTING
          uri         = query
        IMPORTING
          object_name = DATA(object_name)
          tadir_type  = DATA(object_type) ).
      obj_name_range = VALUE #( ( sign = 'I' option = 'EQ' low = object_name ) ).
      obj_type_range = VALUE #( ( sign = 'I' option = 'EQ' low = object_type ) ).
    ELSEIF search_params-query_type = zif_abaptags_c_global=>tag_query_types-object_name_type_combo.
      SPLIT query AT ':' INTO TABLE obj_name_type.
      IF lines( obj_name_type ) = 2.
        obj_name_range = VALUE #( ( sign = 'I' option = 'EQ' low = obj_name_type[ 1 ] ) ).
        obj_type_range = VALUE #( ( sign = 'I' option = 'EQ' low = obj_name_type[ 2 ] ) ).
      ENDIF.
    ELSE.
      DATA(l_query) = to_upper( query ).
      DATA(length) = strlen( query ).
      DATA(last_char_offset) = length - 1.

      DATA(option) = 'EQ'.
      DATA(sign) = 'I'.

      IF l_query+last_char_offset(1) = '<'.
        l_query = l_query(last_char_offset).
      ELSEIF l_query+last_char_offset(1) <> '*'.
        l_query = |{ l_query }*|.
      ENDIF.

      IF l_query CA '+*'.
        option = 'CP'.
      ENDIF.

      obj_name_range = VALUE #( ( sign = sign option = option low = l_query ) ).
    ENDIF.

    IF search_params-query_focus = zif_abaptags_c_global=>tag_query_focus-parent_object.
      parent_object_name_range = obj_name_range.
      parent_object_type_range = obj_type_range.
    ELSE.
      object_name_range = obj_name_range.
      object_type_range = obj_type_range.
    ENDIF.

  ENDMETHOD.


  METHOD get_request_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TGOBJ_SEARCH_PARAMS'
      root_name    = 'SEARCH_PARAMS'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.


  METHOD get_response_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TGOBJ_TREE_RESULT'
      root_name    = 'TREE_RESULT'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.


  METHOD get_matching_tags.
    IF tag_id_range IS INITIAL.
      read_first_level_tags( ).
      enhance_first_level_tags( ).
    ELSE.
      IF parent_object_name_range IS NOT INITIAL.
        read_sub_level_objs_with_tags( ).
      ELSE.
        " [1st draft] retrieve all sub tags
        " [final solution]  only retrieve sub tags if there tagged objects without parent objects for that tags
        read_sub_level_tags( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_matching_objects.
    TYPES:
      BEGIN OF ty_tgobj_with_count,
        tag_id        TYPE zabaptags_tgobjn-tag_id,
        name          TYPE zabaptags_tgobjn-object_name,
        type          TYPE zabaptags_tgobjn-object_type,
        sub_obj_count TYPE i,
      END OF ty_tgobj_with_count.

    DATA: matching_objects TYPE STANDARD TABLE OF ty_tgobj_with_count.

    " root level only has tags, but no objects
    IF tag_id_range IS INITIAL OR parent_object_name_range IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF tag_infos-has_children = abap_true.
      SELECT tgobj~tag_id,
             tgobj~object_name AS name,
             tgobj~object_type AS type,
             coalesce( COUNT( sub_tgobj~tag_id ), 0 ) AS sub_obj_count
        FROM zabaptags_tgobjn AS tgobj
          LEFT OUTER JOIN zabaptags_tgobjn AS sub_tgobj
            ON  tgobj~object_name = sub_tgobj~parent_object_name
            AND tgobj~object_type = sub_tgobj~parent_object_type
            AND tgobj~tag_id      = sub_tgobj~parent_tag_id
        WHERE tgobj~tag_id IN @tag_id_range
          AND tgobj~parent_object_name IS INITIAL
        GROUP BY tgobj~tag_id,
                 tgobj~object_name,
                 tgobj~object_type
        INTO CORRESPONDING FIELDS OF TABLE @matching_objects.
    ELSE.
      SELECT DISTINCT
             tgobj~tag_id,
             tgobj~object_name AS name,
             tgobj~object_type AS type,
             0 AS sub_obj_count
        FROM zabaptags_tgobjn AS tgobj
        WHERE tgobj~tag_id IN @tag_id_range
          AND tgobj~parent_object_name IS INITIAL
        INTO CORRESPONDING FIELDS OF TABLE @matching_objects.
    ENDIF.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(tadir_info_reader) = NEW zcl_abaptags_tadir( CORRESPONDING #( matching_objects ) )->determine_tadir_entries( ).

    LOOP AT matching_objects ASSIGNING FIELD-SYMBOL(<matching_obj>).
      TRY.
          DATA(tadir_info) = tadir_info_reader->get_tadir_info(
            name = <matching_obj>-name
            type = <matching_obj>-type ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      DATA(adt_object_ref) = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
        tadir_type = <matching_obj>-type
        name       = <matching_obj>-name ).

      DATA(tagged_object) = VALUE zabaptags_tgobj_tree_object(
        expandable = xsdbool( <matching_obj>-sub_obj_count > 0 )
        object_ref = VALUE #(
          name         = <matching_obj>-name
          type         = adt_object_ref-type
          tadir_type   = <matching_obj>-type
          uri          = adt_object_ref-uri
          package_name = tadir_info-package_name
          owner        = tadir_info-author ) ).

      tree_result-objects = VALUE #( BASE tree_result-objects ( tagged_object ) ).
    ENDLOOP.

    fill_descriptions( ).

  ENDMETHOD.


  METHOD fill_descriptions.
    DATA: texts TYPE STANDARD TABLE OF seu_objtxt.

    texts = VALUE #(
      FOR tagged_obj IN tree_result-objects
      ( object = tagged_obj-object_ref-tadir_type obj_name = tagged_obj-object_ref-name ) ).

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab = texts.

    LOOP AT tree_result-objects ASSIGNING FIELD-SYMBOL(<object>).
      ASSIGN texts[ obj_name = <object>-object_ref-name object = <object>-object_ref-tadir_type ] TO FIELD-SYMBOL(<text>).
      IF sy-subrc = 0.
        <object>-object_ref-description = <text>-stext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_shared_tags.
    SELECT 'I' AS sign,
           'EQ' AS option,
            shared~tag_id AS low
      FROM zabaptags_shtags AS shared
        INNER JOIN zabaptags_tags AS tags
          ON shared~tag_id = tags~tag_id
      WHERE shared_user = @sy-uname
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD read_first_level_tags.
    SELECT tag~tag_id,
           tag~parent_tag_id,
           tag~name,
           tag~name_upper,
           tag~owner,
           tag~description,
           tag~is_shared,
           COUNT(*) AS tagged_object_count
      FROM zabaptags_i_taggedobjaggr AS aggr
        INNER JOIN zabaptags_tags AS tag
          ON aggr~tag_id = tag~tag_id
      WHERE tag~parent_tag_id IS INITIAL
      GROUP BY tag~tag_id,
               tag~parent_tag_id,
               tag~name,
               tag~name_upper,
               tag~owner,
               tag~description,
               tag~is_shared
      INTO CORRESPONDING FIELDS OF TABLE @tree_result-tags.
  ENDMETHOD.


  METHOD read_sub_level_tags.
    DATA: tags_to_keep TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    " collect all the child tags in the hierarchy
    DATA(tags_in_hierarchy) = get_tags_in_hierarchy( ).
    DATA(relevant_tag_ids) = VALUE zif_abaptags_ty_global=>ty_tag_id_range(
      FOR <t> IN tags_in_hierarchy ( sign = 'I' option = 'EQ' low = <t>-tag_id )
      ( LINES OF tag_id_range ) ).

    " determine tagged object counts for tags in the hierarchy
    SELECT tag~tag_id,
           coalesce( COUNT( tgobj~tag_id ),0 ) AS tagged_object_count
      FROM zabaptags_tags AS tag
        LEFT OUTER JOIN zabaptags_tgobjn AS tgobj
          ON tgobj~tag_id = tag~tag_id
      WHERE tag~parent_tag_id IN @relevant_tag_ids
        AND tgobj~parent_object_name IS INITIAL
      GROUP BY tag~tag_id,
               tag~parent_tag_id,
               tag~name,
               tag~name_upper,
               tag~owner,
               tag~description,
               tag~is_shared
      INTO TABLE @DATA(possible_tags).

    " determine all tags that need to be kept because of assigned objects
    LOOP AT possible_tags ASSIGNING FIELD-SYMBOL(<tag>) WHERE tagged_object_count > 0.
      ASSIGN tags_in_hierarchy[ tag_id = <tag>-tag_id ] TO FIELD-SYMBOL(<tag_in_hier>).
      IF sy-subrc = 0.
        <tag_in_hier>-tagged_object_count = <tag>-tagged_object_count.

        tags_to_keep = VALUE #( BASE tags_to_keep ( sign = 'I' option = 'EQ' low = <tag_in_hier>-tag_id ) ).

        DATA(parent_tag_id) = <tag_in_hier>-parent_tag_id.

        WHILE parent_tag_id IS NOT INITIAL.
          ASSIGN tags_in_hierarchy[ tag_id = parent_tag_id ] TO <tag_in_hier>.
          IF sy-subrc = 0.
            tags_to_keep = VALUE #( BASE tags_to_keep ( sign = 'I' option = 'EQ' low = parent_tag_id ) ).
            parent_tag_id = <tag_in_hier>-parent_tag_id.
          ELSE.
            EXIT.
          ENDIF.
        ENDWHILE.

      ENDIF.
    ENDLOOP.

    IF sy-subrc = 0.
      " We only keep the tags that have tagged objects
      DELETE tags_in_hierarchy WHERE tag_id NOT IN tags_to_keep.
      " Only the direct child tags are returned
      DELETE tags_in_hierarchy WHERE parent_tag_id NOT IN tag_id_range.
      tree_result-tags = tags_in_hierarchy.
    ENDIF.

  ENDMETHOD.


  METHOD retrieve_addtnl_input_infos.
    CHECK lines( tag_id_range ) = 1.

    DATA(tag_id) = tag_id_range[ 1 ]-low.

    SELECT SINGLE *
      FROM zabaptags_tags
      WHERE tag_id = @tag_id
      INTO CORRESPONDING FIELDS OF @tag_infos.

    " read flag to mark tagged objects as expandable or not
    SELECT SINGLE @abap_true
      FROM zabaptags_tags
      WHERE parent_tag_id = @tag_id
      INTO @tag_infos-has_children.

    IF tag_infos-is_shared = abap_true.
      IF tag_infos-parent_tag_id IS INITIAL.
        " Really necessary to verify if it is still shared for the current user???
        SELECT SINGLE @abap_true
          FROM zabaptags_shtags
          WHERE tag_id = @tag_id
          INTO @tag_infos-is_shared_for_me.
      ELSE.
        " hierarchical sharing is not possible, so to be truly sure the complete
        " hierarchy up to the root would need to be checked to see if the root is shared
        " for the current user
        tag_infos-is_shared_for_me = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD enhance_first_level_tags.
    CHECK tree_result-tags IS NOT INITIAL.

    DATA(shared_tags) = get_shared_tags( ).
    IF shared_tags IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT tree_result-tags ASSIGNING FIELD-SYMBOL(<tag>) WHERE is_shared = abap_true.
      IF <tag>-tag_id IN shared_tags.
        <tag>-is_shared_for_me = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_sub_level_objs_with_tags.

    SELECT child_tgobj~tag_id,
           child_tgobj~parent_tag_id,
           child_tgobj~object_name,
           child_tgobj~object_type,
           child_tag~name,
           child_tag~name_upper,
           child_tag~description,
           child_tag~owner,
           coalesce( COUNT( grand_child_obj~tag_id ), 0 ) AS grand_child_obj_count
      FROM zabaptags_tgobjn AS child_tgobj
        INNER JOIN zabaptags_tags AS child_tag
          ON child_tgobj~tag_id = child_tag~tag_id
        LEFT OUTER JOIN zabaptags_tgobjn AS grand_child_obj
          ON  child_tgobj~object_name = grand_child_obj~parent_object_name
          AND child_tgobj~object_type = grand_child_obj~parent_object_type
          AND child_tgobj~tag_id      = grand_child_obj~parent_tag_id
      WHERE child_tgobj~parent_tag_id IN @tag_id_range
        AND child_tgobj~parent_object_type IN @parent_object_type_range
        AND child_tgobj~parent_object_name IN @parent_object_name_range
      group by child_tgobj~tag_id,
               child_tgobj~parent_tag_id,
               child_tgobj~object_name,
               child_tgobj~object_type,
               child_tag~name,
               child_tag~name_upper,
               child_tag~description,
               child_tag~owner
      INTO TABLE @DATA(objects_with_tags).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " determine tadir info for found objects
    DATA(tadir_access) = NEW zcl_abaptags_tadir( VALUE #(
      FOR <obj> IN objects_with_tags
      ( name = <obj>-object_name type = <obj>-object_type ) ) )->determine_tadir_entries( ).

    LOOP AT objects_with_tags ASSIGNING FIELD-SYMBOL(<obj_with_tag>)
        GROUP BY ( tag_id = <obj_with_tag>-tag_id ) ASSIGNING FIELD-SYMBOL(<obj_with_tag_group>).

      DATA(tagged_count) = 0.

      LOOP AT GROUP <obj_with_tag_group> ASSIGNING FIELD-SYMBOL(<obj_with_tag_group_entry>).
        TRY.
            DATA(tadir_info) = tadir_access->get_tadir_info( name = <obj_with_tag_group_entry>-object_name
                                                             type = <obj_with_tag_group_entry>-object_type ).
          CATCH cx_sy_itab_line_not_found.
            CONTINUE.
        ENDTRY.

        DATA(adt_object_ref) = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
          tadir_type = tadir_info-type
          name       = <obj_with_tag_group_entry>-object_name ).

        tree_result-objects = VALUE #( BASE tree_result-objects
          ( parent_tag_id = <obj_with_tag_group>-tag_id
            " TODO: do another select with grouping to determine the count of child elements
*            expandable    = abap_true
            expandable    = xsdbool( <obj_with_tag_group_entry>-grand_child_obj_count > 0 )
            " tagged_object_count
            object_ref    = VALUE #(
              name         = <obj_with_tag_group_entry>-object_name
              type         = adt_object_ref-type
              tadir_type   = <obj_with_tag_group_entry>-object_type
              uri          = adt_object_ref-uri
              package_name = tadir_info-package_name
              owner        = tadir_info-author ) ) ).
        tagged_count = tagged_count + 1.
      ENDLOOP.

      IF sy-subrc = 0 AND <obj_with_tag_group_entry> IS ASSIGNED.
        tree_result-tags = VALUE #( BASE tree_result-tags
          ( tag_id              = <obj_with_tag_group_entry>-tag_id
            name                = <obj_with_tag_group_entry>-name
            name_upper          = <obj_with_tag_group_entry>-name_upper
            description         = <obj_with_tag_group_entry>-description
            parent_tag_id       = <obj_with_tag_group_entry>-parent_tag_id
            tagged_object_count = tagged_count ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_tags_in_hierarchy.
    DATA: temp_ids TYPE zif_abaptags_ty_global=>ty_tag_id_range,
          new_ids  LIKE result.

    temp_ids = tag_id_range.

    WHILE temp_ids IS NOT INITIAL.
      SELECT tag_id,
             parent_tag_id,
             name,
             name_upper,
             description,
             owner
        FROM zabaptags_tags
        WHERE parent_tag_id IN @temp_ids
        INTO CORRESPONDING FIELDS OF TABLE @new_ids.

      LOOP AT new_ids ASSIGNING FIELD-SYMBOL(<new_id>).
        INSERT <new_id> INTO TABLE result.
      ENDLOOP.

      temp_ids = VALUE #( FOR <child_tag> IN new_ids ( sign = 'I' option = 'EQ' low = <child_tag>-tag_id ) ).
    ENDWHILE.

  ENDMETHOD.

ENDCLASS.
