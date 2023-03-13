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
      BEGIN OF ty_tag_to_root_tag,
        tag_id      TYPE zabaptags_tag_id,
        root_tag_id TYPE zabaptags_tag_id,
        has_objects TYPE abap_bool,
      END OF ty_tag_to_root_tag,

      ty_tag_to_root_tag_ind TYPE SORTED TABLE OF ty_tag_to_root_tag WITH UNIQUE KEY tag_id root_tag_id,
      ty_tag_data_sorted     TYPE SORTED TABLE OF zabaptags_tag_data WITH UNIQUE KEY tag_id.

    DATA:
      parent_object_name_range TYPE RANGE OF sobj_name,
      parent_object_type_range TYPE RANGE OF trobjtype,
      request_data             TYPE zabaptags_tgobj_tree_request,
      tree_result              TYPE zabaptags_tgobj_tree_result,
      tag_infos                TYPE ty_tag_infos,
      tag_id_range             TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    METHODS:
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
      collect_child_info
        IMPORTING
          root_tag_id         TYPE zabaptags_tag_id
          tags                TYPE zabaptags_tag_data_t
        CHANGING
          tag_to_root_tag_ind TYPE ty_tag_to_root_tag_ind
          child_tag_range     TYPE zif_abaptags_ty_global=>ty_tag_id_range,
      get_tags_in_hierarchy
        RETURNING
          VALUE(result) TYPE ty_tag_data_sorted.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobjtsrv IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = request_data ).

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
    IF request_data-tag_id IS NOT INITIAL.
      tag_id_range = VALUE #( ( sign = 'I' option = 'EQ' low = request_data-tag_id ) ).
    ENDIF.

    IF request_data-parent_object_name IS NOT INITIAL AND
        request_data-parent_object_type IS NOT INITIAL.
      parent_object_name_range = VALUE #( ( sign = 'I' option = 'EQ' low = request_data-parent_object_name ) ).
      parent_object_type_range = VALUE #( ( sign = 'I' option = 'EQ' low = request_data-parent_object_type ) ).
    ENDIF.
  ENDMETHOD.


  METHOD get_request_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TGOBJ_TREE_REQUEST'
      root_name    = 'REQUEST_DATA'
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
    DATA: tag_to_root_tag_ind TYPE ty_tag_to_root_tag_ind,
          all_child_tag_range TYPE zif_abaptags_ty_global=>ty_tag_id_range,
          all_tags            TYPE zabaptags_tag_data_t.

    FIELD-SYMBOLS: <child_tags> TYPE zabaptags_tag_data_t.

    " 1) Select all tags with corresponding information
    SELECT tag~tag_id,
           tag~parent_tag_id,
           tag~name,
           tag~name_upper,
           tag~owner,
           tag~description,
           tag~is_shared
      FROM zabaptags_tags AS tag
      INTO CORRESPONDING FIELDS OF TABLE @all_tags.

    " 2) build hierarchical tags
    DATA(hier_tags) = zcl_abaptags_tag_util=>build_hierarchical_tags( tags_flat = all_tags ).

    " 3) map child tags to root tags
    LOOP AT hier_tags ASSIGNING FIELD-SYMBOL(<hier_tag>) WHERE child_tags IS NOT INITIAL.
      ASSIGN <hier_tag>-child_tags->* TO <child_tags>.
      collect_child_info( EXPORTING root_tag_id         = <hier_tag>-tag_id
                                    tags                = <child_tags>
                          CHANGING  tag_to_root_tag_ind = tag_to_root_tag_ind
                                    child_tag_range     = all_child_tag_range ).
    ENDLOOP.

    " 4) get object counts on root/flat tags
    SELECT tag~tag_id, COUNT(*) AS tagged_object_count
      FROM zabaptags_i_taggedobjaggr AS aggr
        INNER JOIN zabaptags_tags AS tag
          ON aggr~tag_id = tag~tag_id
      WHERE tag~parent_tag_id IS INITIAL
      GROUP BY tag~tag_id
      INTO TABLE @DATA(root_tags_w_counts).

    " 5) find tags in hierarchy with at least 1 object assigned
    IF all_child_tag_range IS NOT INITIAL.
      SELECT DISTINCT tag~tag_id
        FROM zabaptags_i_taggedobjaggr AS aggr
          INNER JOIN zabaptags_tags AS tag
            ON aggr~tag_id = tag~tag_id
        WHERE tag~tag_id IN @all_child_tag_range
        INTO TABLE @DATA(child_tags_w_objects).

      IF sy-subrc = 0.
        " fill map table
        LOOP AT child_tags_w_objects ASSIGNING FIELD-SYMBOL(<child_tag_w_objects>).
          tag_to_root_tag_ind[ tag_id = <child_tag_w_objects>-tag_id ]-has_objects = abap_true.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF root_tags_w_counts IS INITIAL AND
        child_tags_w_objects IS INITIAL.
      RETURN.
    ENDIF.

    " 6) collect only tags with object counts
    LOOP AT hier_tags ASSIGNING <hier_tag> WHERE parent_tag_id IS INITIAL.
      DATA(tag_info) = CORRESPONDING zabaptags_tag_data( <hier_tag> EXCEPT child_tags ).
      ASSIGN root_tags_w_counts[ tag_id = <hier_tag>-tag_id ] TO FIELD-SYMBOL(<obj_count>).
      IF sy-subrc = 0.
        tag_info-tagged_object_count = <obj_count>-tagged_object_count.
      ELSEIF line_exists( tag_to_root_tag_ind[ root_tag_id = <hier_tag>-tag_id
                                               has_objects = abap_true ] ).
      ELSE.
        CONTINUE.
      ENDIF.

      tree_result-tags = VALUE #( BASE tree_result-tags ( tag_info ) ).
    ENDLOOP.

    " 7) enhance root tags
    DATA(shared_tags) = get_shared_tags( ).
    IF shared_tags IS INITIAL.
      DELETE tree_result-tags WHERE owner <> space
                                AND owner <> sy-uname.
    ELSE.
      LOOP AT tree_result-tags ASSIGNING FIELD-SYMBOL(<tag>) WHERE owner <> space
                                                               AND owner <> sy-uname.
        IF <tag>-tag_id IN shared_tags.
          <tag>-is_shared_for_me = abap_true.
        ELSE.
          DELETE tree_result-tags.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD collect_child_info.

    FIELD-SYMBOLS: <child_tags> TYPE zabaptags_tag_data_t.

    LOOP AT tags ASSIGNING FIELD-SYMBOL(<tag>).
      INSERT VALUE #(
        tag_id = <tag>-tag_id
        root_tag_id = root_tag_id ) INTO TABLE tag_to_root_tag_ind.
      child_tag_range = VALUE #(
        BASE child_tag_range ( sign = 'I' option = 'EQ' low = <tag>-tag_id ) ).

      IF <tag>-child_tags IS NOT INITIAL.
        ASSIGN <tag>-child_tags->* TO <child_tags>.
        collect_child_info( EXPORTING root_tag_id         = root_tag_id
                                      tags                = <child_tags>
                            CHANGING  tag_to_root_tag_ind = tag_to_root_tag_ind
                                      child_tag_range     = child_tag_range ).
      ENDIF.
    ENDLOOP.

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
      GROUP BY child_tgobj~tag_id,
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
            expandable    = xsdbool( <obj_with_tag_group_entry>-grand_child_obj_count > 0 )
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
