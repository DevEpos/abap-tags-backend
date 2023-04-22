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
    CONSTANTS:
      c_empty_uuid TYPE sysuuid_x16 VALUE '00000000000000000000000000000000'.

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

      BEGIN OF ty_tag_with_obj_count,
        tag_id              TYPE zabaptags_tags-tag_id,
        tagged_object_count TYPE i,
      END OF ty_tag_with_obj_count,

      BEGIN OF ty_sub_lvl_obj_with_tag,
        tag_id             TYPE zabaptags_tag_id,
        parent_tag_id      TYPE zabaptags_tag_id,
        object_name        TYPE sobj_name,
        object_type        TYPE trobjtype,
        component_name     TYPE zabaptags_obj_comp_name,
        component_type     TYPE swo_objtyp,
        name               TYPE zabaptags_tag_name,
        name_upper         TYPE zabaptags_tag_name,
        description        TYPE zabaptags_tags-description,
        owner              TYPE responsibl,
        has_grand_children TYPE abap_bool,
      END OF ty_sub_lvl_obj_with_tag,

      ty_sub_lvl_objs_with_tag TYPE STANDARD TABLE OF ty_sub_lvl_obj_with_tag WITH EMPTY KEY,
      ty_tags_with_obj_counts  TYPE STANDARD TABLE OF ty_tag_with_obj_count WITH EMPTY KEY,
      ty_tag_data_sorted       TYPE SORTED TABLE OF zabaptags_tag_data WITH UNIQUE KEY tag_id.

    DATA:
      parent_object_name_range TYPE RANGE OF sobj_name,
      parent_object_type_range TYPE RANGE OF trobjtype,
      request_data             TYPE zabaptags_tgobj_tree_request,
      tree_result              TYPE zabaptags_tgobj_tree_result,
      tag_infos                TYPE ty_tag_infos,
      read_full_tree_count     TYPE abap_bool,
      is_shared_tags_read      TYPE abap_bool,
      shared_tags_range        TYPE zif_abaptags_ty_global=>ty_tag_id_range,
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
      post_process_root_tags,
      find_sub_level_objs_with_tags
        RETURNING
          VALUE(result) TYPE ty_sub_lvl_objs_with_tag,
      find_sub_level_comps_with_tags
        RETURNING
          VALUE(result) TYPE ty_sub_lvl_objs_with_tag,
      read_sub_level_objs_with_tags,
      get_tags_in_hierarchy
        RETURNING
          VALUE(result) TYPE ty_tag_data_sorted,
      get_direct_root_tag_counts
        RETURNING
          VALUE(result) TYPE ty_tags_with_obj_counts,
      get_deep_root_tag_counts
        RETURNING
          VALUE(result) TYPE ty_tags_with_obj_counts,
      fetch_full_tree_count.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobjtsrv IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = request_data ).

    get_parameters( request ).
    retrieve_addtnl_input_infos( ).
    IF read_full_tree_count = abap_true.
      fetch_full_tree_count( ).
    ENDIF.

    get_matching_tags( ).
    get_matching_objects( ).
    fill_descriptions( ).

    response->set_body_data(
      content_handler = get_response_content_handler( )
      data            = tree_result ).
  ENDMETHOD.


  METHOD get_parameters.
    IF request_data-tag_id IS NOT INITIAL.
      tag_id_range = VALUE #( ( sign = 'I' option = 'EQ' low = request_data-tag_id ) ).
    ELSE.
      read_full_tree_count = abap_true.
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
      post_process_root_tags( ).
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
        tag_id       TYPE zabaptags_tgobjn-tag_id,
        name         TYPE zabaptags_tgobjn-object_name,
        type         TYPE zabaptags_tgobjn-object_type,
        comp_name    TYPE zabaptags_obj_comp_name,
        comp_type    TYPE swo_objtyp,
        has_children TYPE abap_bool,
      END OF ty_tgobj_with_count.

    DATA: matching_objects TYPE STANDARD TABLE OF ty_tgobj_with_count,
          adt_object_ref   TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.

    " root level only has tags, but no objects
    IF tag_id_range IS INITIAL OR parent_object_name_range IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF tag_infos-has_children = abap_true.
      SELECT tgobj~tagid AS tag_id,
             tgobj~objectname AS name,
             tgobj~objecttype AS type,
             tgobj~componentname AS comp_name,
             tgobj~componenttype AS comp_type,
             coalesce( sub_tgobj~dummy, @abap_false ) AS has_children
        FROM zabaptags_i_tgobjn AS tgobj
          LEFT OUTER JOIN zabaptags_i_tgobjn AS sub_tgobj
            ON  tgobj~objectname    = sub_tgobj~parentobjectname
            AND tgobj~objecttype    = sub_tgobj~parentobjecttype
            AND tgobj~tagid         = sub_tgobj~parenttagid
            " Hint: Components can not have children at this time
            AND tgobj~componentname = @space
        WHERE tgobj~tagid IN @tag_id_range
          AND tgobj~parentobjectname = @space
        GROUP BY tgobj~tagid,
                 tgobj~objectname,
                 tgobj~objecttype,
                 tgobj~componentname,
                 tgobj~componenttype,
                 sub_tgobj~dummy
        INTO CORRESPONDING FIELDS OF TABLE @matching_objects.
    ELSE.
      SELECT DISTINCT
             tgobj~tagid AS tag_id,
             tgobj~objectname AS name,
             tgobj~objecttype AS type,
             tgobj~componentname AS comp_name,
             tgobj~componenttype AS comp_type,
             @abap_false AS has_children
        FROM zabaptags_i_tgobjn AS tgobj
        WHERE tgobj~tagid IN @tag_id_range
          AND tgobj~parentobjectname = @space
        INTO CORRESPONDING FIELDS OF TABLE @matching_objects.
    ENDIF.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(tadir_info_reader) = NEW zcl_abaptags_tadir( CORRESPONDING #( matching_objects ) )->determine_tadir_entries( ).

    DATA(comp_adt_mapper) = NEW zcl_abaptags_comp_adt_mapper( ).
    comp_adt_mapper->add_components( VALUE #(
      FOR <mo> IN matching_objects WHERE ( comp_name IS NOT INITIAL ) ( VALUE #(
        component_name = <mo>-comp_name
        component_type = <mo>-comp_type
        object_name    = <mo>-name
        object_type    = <mo>-type ) ) ) ).
    comp_adt_mapper->determine_components( ).

    LOOP AT matching_objects ASSIGNING FIELD-SYMBOL(<matching_obj>).
      TRY.
          DATA(tadir_info) = tadir_info_reader->get_tadir_info(
            name = <matching_obj>-name
            type = <matching_obj>-type ).
        CATCH cx_sy_itab_line_not_found.
          " TODO: handle some edge cases, like $-packages
          CONTINUE.
      ENDTRY.

      DATA(object_name) = ``.
      DATA(parent_object_name) = ``.
      DATA(adt_type) = ``.

      IF <matching_obj>-comp_name IS NOT INITIAL.
        adt_object_ref = comp_adt_mapper->get_adt_object( VALUE #(
          object_name    = <matching_obj>-name
          object_type    = <matching_obj>-type
          component_name = <matching_obj>-comp_name
          component_type = <matching_obj>-comp_type ) ).
        adt_type = <matching_obj>-comp_type.
        object_name = <matching_obj>-comp_name.
        parent_object_name = <matching_obj>-name.
      ELSE.
        adt_object_ref = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
          tadir_type = <matching_obj>-type
          name       = <matching_obj>-name ).
        adt_type = adt_object_ref-type.
        object_name = <matching_obj>-name.
      ENDIF.

      IF adt_object_ref-uri IS INITIAL.
        " No URI means that the TADIR object or the component does not longer exist
        CONTINUE.
      ENDIF.

      DATA(tagged_object) = VALUE zabaptags_tgobj_tree_object(
        expandable = <matching_obj>-has_children
        object_ref = VALUE #(
          name         = object_name
          parent_name  = parent_object_name
          type         = adt_type
          tadir_type   = <matching_obj>-type
          uri          = adt_object_ref-uri
          package_name = tadir_info-package_name
          owner        = tadir_info-author ) ).

      tree_result-objects = VALUE #( BASE tree_result-objects ( tagged_object ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD fill_descriptions.
    DATA: texts TYPE STANDARD TABLE OF seu_objtxt.

    CHECK tree_result-objects IS NOT INITIAL.

    texts = VALUE #(
      FOR tagged_obj IN tree_result-objects WHERE ( object_ref-parent_name IS INITIAL )
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
    IF is_shared_tags_read = abap_false.
      SELECT 'I' AS sign,
             'EQ' AS option,
              shared~tag_id AS low
        FROM zabaptags_shtags AS shared
          INNER JOIN zabaptags_tags AS tags
            ON shared~tag_id = tags~tag_id
        WHERE shared_user = @sy-uname
        INTO CORRESPONDING FIELDS OF TABLE @shared_tags_range.

      is_shared_tags_read = abap_true.
    ENDIF.

    result = shared_tags_range.
  ENDMETHOD.


  METHOD read_first_level_tags.
    SELECT tag~tag_id,
           tag~parent_tag_id,
           tag~name,
           tag~name_upper,
           tag~owner,
           tag~description,
           tag~is_shared
      FROM zabaptags_tags AS tag
      WHERE parent_tag_id = @c_empty_uuid
      INTO TABLE @DATA(root_tags).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(root_tags_w_counts) = VALUE ty_tags_with_obj_counts(
      ( LINES OF get_direct_root_tag_counts( ) )
      ( LINES OF get_deep_root_tag_counts( ) ) ).

    IF root_tags_w_counts IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT root_tags ASSIGNING FIELD-SYMBOL(<root_tag>).
      IF line_exists( root_tags_w_counts[ tag_id = <root_tag>-tag_id ] ).
        tree_result-tags = VALUE #( BASE tree_result-tags ( CORRESPONDING #( <root_tag> ) ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_sub_level_tags.
    DATA: tags_to_keep TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    SELECT map~tag_id,
           COUNT( * ) AS tagged_object_count
      FROM zabaptags_tagsrm AS map
        INNER JOIN zabaptags_tgobjn AS tgobj
          ON tgobj~tag_id = map~tag_id
      WHERE map~root_tag_id IN @tag_id_range
        AND tgobj~parent_object_name = @space
      GROUP BY map~tag_id
      INTO TABLE @DATA(sub_tags_with_obj_counts).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " find tags in hierarchy with more information
    DATA(tags_in_hierarchy) = get_tags_in_hierarchy( ).

    " determine all tags that need to be kept because of assigned objects
    LOOP AT sub_tags_with_obj_counts ASSIGNING FIELD-SYMBOL(<tag>) WHERE tagged_object_count > 0.
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


  METHOD post_process_root_tags.
    CHECK tree_result-tags IS NOT INITIAL.

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


  METHOD read_sub_level_objs_with_tags.
    DATA(objects_with_tags) = VALUE ty_sub_lvl_objs_with_tag(
      ( LINES OF find_sub_level_comps_with_tags( ) )
      ( LINES OF find_sub_level_objs_with_tags( ) ) ).
    IF objects_with_tags IS INITIAL.
      RETURN.
    ENDIF.

    " determine tadir info for found objects
    DATA(tadir_access) = NEW zcl_abaptags_tadir( VALUE #(
      FOR <obj> IN objects_with_tags
      ( name = <obj>-object_name type = <obj>-object_type ) ) )->determine_tadir_entries( ).
    DATA: adt_object_ref TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.

    " determine ADT info for components of repository objects
    DATA(cmp_adt_mapper) = NEW zcl_abaptags_comp_adt_mapper( ).
    cmp_adt_mapper->add_components( VALUE #(
      FOR <owt> IN objects_with_tags WHERE ( component_name IS NOT INITIAL )
      ( component_name = <owt>-component_name
        component_type = <owt>-component_type
        object_name    = <owt>-object_name
        object_type    = <owt>-object_type ) ) ).

    cmp_adt_mapper->determine_components( ).

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

        DATA(object_name) = ``.
        DATA(parent_object_name) = ``.
        DATA(adt_type) = ``.

        IF <obj_with_tag_group_entry>-component_name IS NOT INITIAL.
          adt_object_ref = cmp_adt_mapper->get_adt_object( VALUE #(
            object_name    = <obj_with_tag_group_entry>-object_name
            object_type    = <obj_with_tag_group_entry>-object_type
            component_name = <obj_with_tag_group_entry>-component_name
            component_type = <obj_with_tag_group_entry>-component_type ) ).
          adt_type = <obj_with_tag_group_entry>-component_type.
          object_name = <obj_with_tag_group_entry>-component_name.
          parent_object_name = <obj_with_tag_group_entry>-object_name.
        ELSE.
          adt_object_ref = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
            tadir_type = <obj_with_tag_group_entry>-object_type
            name       = <obj_with_tag_group_entry>-object_name ).
          adt_type = adt_object_ref-type.
          object_name = <obj_with_tag_group_entry>-object_name.
        ENDIF.

        " skip deleted objects
        IF adt_object_ref-uri IS INITIAL.
          UNASSIGN <obj_with_tag_group_entry>.
          CONTINUE.
        ENDIF.

        tree_result-objects = VALUE #( BASE tree_result-objects
          ( parent_tag_id = <obj_with_tag_group>-tag_id
            expandable    = <obj_with_tag_group_entry>-has_grand_children
            object_ref    = VALUE #(
              name         = object_name
              parent_name  = parent_object_name
              type         = adt_type
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


  METHOD find_sub_level_comps_with_tags.
    SELECT childtagid AS tag_id,
           childparenttagid AS parent_tag_id,
           childobjectname AS object_name,
           childobjecttype AS object_type,
           childcomponentname AS component_name,
           childcomponenttype AS component_type,
           childtagname AS name,
           childtagnameupper AS name_upper,
           childtagdescription AS description,
           childtagowner AS owner
      FROM zabaptags_i_sublvlcmpwtag
      WHERE childparenttagid IN @tag_id_range
        AND childparentobjectname IN @parent_object_name_range
        AND childparentobjecttype IN @parent_object_type_range
      GROUP BY childtagid,
               childparenttagid,
               childobjectname,
               childobjecttype,
               childcomponentname,
               childcomponenttype,
               childtagname,
               childtagnameupper,
               childtagdescription,
               childtagowner
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD find_sub_level_objs_with_tags.
    SELECT childtagid AS tag_id,
           childparenttagid AS parent_tag_id,
           childobjectname AS object_name,
           childobjecttype AS object_type,
           childtagname AS name,
           childtagnameupper AS name_upper,
           childtagdescription AS description,
           childtagowner AS owner,
           hasgrandchildren AS has_grand_children
      FROM zabaptags_i_sublvlobjwtag
      WHERE childparenttagid IN @tag_id_range
        AND childparentobjectname IN @parent_object_name_range
        AND childparentobjecttype IN @parent_object_type_range
      GROUP BY childtagid,
               childparenttagid,
               childobjectname,
               childobjecttype,
               childtagname,
               childtagnameupper,
               childtagdescription,
               childtagowner,
               hasgrandchildren
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD get_tags_in_hierarchy.
    SELECT tag~tag_id,
          tag~parent_tag_id,
          tag~name,
          tag~name_upper,
          tag~description,
          tag~owner
      FROM zabaptags_tagsrm AS map
        INNER JOIN zabaptags_tags AS tag
          ON map~tag_id = tag~tag_id
      WHERE map~root_tag_id IN @tag_id_range
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD get_direct_root_tag_counts.
    DATA(dyn_from) = |{ zcl_abaptags_ddls_id=>view_i_tagged_obj_aggr } AS aggr | &&
      |INNER JOIN zabaptags_tags AS tag| &&
      |  ON aggr~tag_id = tag~tag_id|.

    SELECT tag~tag_id, COUNT(*) AS tagged_object_count
      FROM (dyn_from)
      WHERE tag~parent_tag_id = @c_empty_uuid
      GROUP BY tag~tag_id
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD get_deep_root_tag_counts.
    DATA(dyn_from) = | { zcl_abaptags_ddls_id=>view_i_root_tags_with_counts } AS aggr | &&
      |INNER JOIN zabaptags_tags AS tag| &&
      |  ON  aggr~tagid = tag~tag_id| &&
      |  AND tag~parent_tag_id = @c_empty_uuid|.

    SELECT tagid AS tag_id,
           objectcount AS tagged_object_count
      FROM (dyn_from)
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD fetch_full_tree_count.
    SELECT COUNT(*)
      FROM zabaptags_tgobjn AS tgobj
        INNER JOIN zabaptags_tags AS tag
          ON tgobj~tag_id = tag~tag_id
      WHERE tag~owner = @sy-uname
         OR tag~owner = @space
      INTO @tree_result-tagged_object_count.

    DATA(shared_tags) = get_shared_tags( ).
    IF shared_tags IS NOT INITIAL.
      SELECT COUNT(*) FROM zabaptags_tgobjn
        WHERE tag_id IN @shared_tags
        INTO @DATA(temp_count).
      tree_result-tagged_object_count = tree_result-tagged_object_count + temp_count.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
