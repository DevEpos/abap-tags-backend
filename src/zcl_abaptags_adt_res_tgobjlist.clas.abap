"! <p class="shorttext synchronized" lang="en">Resource for retrieving a list of tagged objects</p>
CLASS zcl_abaptags_adt_res_tgobjlist DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tagged_object,
        id                 TYPE sysuuid_x16,
        tag_id             TYPE zabaptags_tag_id,
        tag_name           TYPE zabaptags_tag_name,
        owner              TYPE zabaptags_tags-owner,
        object_name        TYPE sobj_name,
        object_type        TYPE trobjtype,
        component_name     TYPE zabaptags_obj_comp_name,
        component_type     TYPE swo_objtyp,
        parent_tag_id      TYPE zabaptags_tag_id,
        parent_tag_name    TYPE zabaptags_tag_name,
        parent_object_name TYPE sobj_name,
        parent_object_type TYPE trobjtype,
      END OF ty_tagged_object,

      ty_tagged_objects TYPE STANDARD TABLE OF ty_tagged_object WITH EMPTY KEY.

    DATA: list_request        TYPE zabaptags_tgobj_list_request,
          found_objects       TYPE ty_tagged_objects,
          tagged_object_infos TYPE zabaptags_tgobj_info_t,

          BEGIN OF select_keys,
            tag_id        TYPE TABLE OF zabaptags_tag_id,
            full_semantic TYPE ty_tagged_objects,
            parent_obj    TYPE ty_tagged_objects,
            comp_obj      TYPE ty_tagged_objects,
            tag_and_obj   TYPE ty_tagged_objects,
          END OF select_keys.

    METHODS: get_request_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_response_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_tagged_objects,
      get_adjusted_types
        IMPORTING
          tagged_object      TYPE ty_tagged_object
        EXPORTING
          object_type        TYPE swo_objtyp
          parent_object_type TYPE swo_objtyp,
      get_tgobj_infos_by_sem_keys,
      post_process_found_objects,
      fill_semantic_key_tables,
      find_obj_by_tag_n_obj,
      find_obj_by_parent_obj,
      find_obj_by_obj_n_comp,
      find_obj_by_full_key,
      find_obj_by_tag_id,
      load_assigned_child_objects.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobjlist IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_handler( )
                            IMPORTING data            = list_request ).

    get_tagged_objects( ).
    IF list_request-load_child_objects = abap_true.
      load_assigned_child_objects( ).
    ENDIF.
    post_process_found_objects( ).

    IF tagged_object_infos IS NOT INITIAL.
      response->set_body_data(
        content_handler = get_response_handler( )
        data            = tagged_object_infos ).
    ENDIF.
  ENDMETHOD.


  METHOD get_request_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name   = 'ZABAPTAGS_TGOBJ_LIST_REQUEST'
      root_name = 'REQUEST' ).
  ENDMETHOD.


  METHOD get_response_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name   = 'ZABAPTAGS_TGOBJ_INFOS'
      root_name = 'TGOBJ_INFOS' ).
  ENDMETHOD.


  METHOD get_tagged_objects.
    IF list_request-tag_ids IS NOT INITIAL.
      select_keys-tag_id = list_request-tag_ids.
    ENDIF.

    fill_semantic_key_tables( ).
    get_tgobj_infos_by_sem_keys( ).

  ENDMETHOD.


  METHOD get_adjusted_types.
    DATA(wb_object_type) = cl_wb_object_type=>create_from_exttype( p_external_id = tagged_object-object_type ).
    DATA(main_global_type) = wb_object_type->get_main_global_type( ).
    IF main_global_type-subtype_wb IS NOT INITIAL.
      object_type = |{ main_global_type-objtype_tr }/{ main_global_type-subtype_wb }|.
    ELSE.
      object_type = main_global_type-objtype_tr.
    ENDIF.

    IF tagged_object-parent_object_type IS NOT INITIAL AND
        tagged_object-parent_object_name IS NOT INITIAL.
      wb_object_type = cl_wb_object_type=>create_from_exttype( p_external_id = tagged_object-parent_object_type ).
      main_global_type = wb_object_type->get_main_global_type( ).
      IF main_global_type-subtype_wb IS NOT INITIAL.
        parent_object_type = |{ main_global_type-objtype_tr }/{ main_global_type-subtype_wb }|.
      ELSE.
        parent_object_type = main_global_type-objtype_tr.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_tgobj_infos_by_sem_keys.
    find_obj_by_tag_n_obj( ).
    find_obj_by_parent_obj( ).
    find_obj_by_obj_n_comp( ).
    find_obj_by_full_key( ).
    find_obj_by_tag_id( ).
  ENDMETHOD.


  METHOD fill_semantic_key_tables.

    LOOP AT list_request-tagged_object_infos ASSIGNING FIELD-SYMBOL(<tgobj>).
      IF <tgobj>-parent_object_name IS NOT INITIAL AND
          <tgobj>-parent_object_type IS NOT INITIAL AND
          <tgobj>-parent_tag_id IS NOT INITIAL AND
          <tgobj>-component_name IS NOT INITIAL AND
          <tgobj>-component_type IS NOT INITIAL.
        select_keys-full_semantic = VALUE #( BASE select_keys-full_semantic ( CORRESPONDING #( <tgobj> ) ) ).
      ELSEIF <tgobj>-parent_object_name IS NOT INITIAL AND
          <tgobj>-parent_object_type IS NOT INITIAL AND
          <tgobj>-parent_tag_id IS NOT INITIAL.
        select_keys-parent_obj = VALUE #( BASE select_keys-parent_obj ( CORRESPONDING #( <tgobj> ) ) ).
      ELSEIF <tgobj>-object_type IS NOT INITIAL AND
          <tgobj>-object_name IS NOT INITIAL AND
          <tgobj>-component_name IS NOT INITIAL AND
          <tgobj>-component_type IS NOT INITIAL.
        select_keys-comp_obj = VALUE #( BASE select_keys-comp_obj ( CORRESPONDING #( <tgobj> ) ) ).
      ELSEIF <tgobj>-object_type IS NOT INITIAL AND
          <tgobj>-object_name IS NOT INITIAL.
        select_keys-tag_and_obj = VALUE #( BASE select_keys-tag_and_obj ( CORRESPONDING #( <tgobj> ) ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_obj_by_full_key.
    CHECK select_keys-full_semantic IS NOT INITIAL.

    SELECT tgobj~id,
           tgobj~tagid AS tag_id,
           tag~name AS tag_name,
           tag~owner AS owner,
           tgobj~objectname AS object_name,
           tgobj~objecttype AS object_type,
           tgobj~componentname AS component_name,
           tgobj~componenttype AS component_type,
           tgobj~parenttagid AS parent_tag_id,
           parent_tag~name AS parent_tag_name,
           tgobj~parentobjectname AS parent_object_name,
           tgobj~parentobjecttype AS parent_object_type
      FROM zabaptags_i_tgobjn AS tgobj
        INNER JOIN zabaptags_i_tag AS tag
          ON tgobj~tagid = tag~tagid
        LEFT OUTER JOIN zabaptags_i_tag AS parent_tag
          ON tgobj~parenttagid = parent_tag~tagid
        FOR ALL ENTRIES IN @select_keys-full_semantic
        WHERE tgobj~objectname       = @select_keys-full_semantic-object_name
          AND tgobj~objecttype       = @select_keys-full_semantic-object_type
          AND tgobj~componentname    = @select_keys-full_semantic-component_name
          AND tgobj~componenttype    = @select_keys-full_semantic-component_type
          AND tgobj~tagid            = @select_keys-full_semantic-tag_id
          AND tgobj~parentobjectname = @select_keys-full_semantic-parent_object_name
          AND tgobj~parentobjecttype = @select_keys-full_semantic-parent_object_type
          AND tgobj~parenttagid      = @select_keys-full_semantic-parent_tag_id
      APPENDING CORRESPONDING FIELDS OF TABLE @found_objects.
  ENDMETHOD.


  METHOD find_obj_by_parent_obj.
    CHECK select_keys-parent_obj IS NOT INITIAL.

    SELECT tgobj~id,
           tgobj~tagid AS tag_id,
           tag~name AS tag_name,
           tag~owner AS owner,
           tgobj~objectname AS object_name,
           tgobj~objecttype AS object_type,
           tgobj~componentname AS component_name,
           tgobj~componenttype AS component_type,
           tgobj~parenttagid AS parent_tag_id,
           parent_tag~name AS parent_tag_name,
           tgobj~parentobjectname AS parent_object_name,
           tgobj~parentobjecttype AS parent_object_type
      FROM zabaptags_i_tgobjn AS tgobj
        INNER JOIN zabaptags_i_tag AS tag
          ON tgobj~tagid = tag~tagid
        LEFT OUTER JOIN zabaptags_i_tag AS parent_tag
          ON tgobj~parenttagid = parent_tag~tagid
        FOR ALL ENTRIES IN @select_keys-parent_obj
        WHERE tgobj~objectname       = @select_keys-parent_obj-object_name
          AND tgobj~objecttype       = @select_keys-parent_obj-object_type
          AND tgobj~componentname    = @space
          AND tgobj~componenttype    = @space
          AND tgobj~tagid            = @select_keys-parent_obj-tag_id
          AND tgobj~parentobjectname = @select_keys-parent_obj-parent_object_name
          AND tgobj~parentobjecttype = @select_keys-parent_obj-parent_object_type
          AND tgobj~parenttagid      = @select_keys-parent_obj-parent_tag_id
      APPENDING CORRESPONDING FIELDS OF TABLE @found_objects.
  ENDMETHOD.


  METHOD find_obj_by_obj_n_comp.
    CHECK select_keys-comp_obj IS NOT INITIAL.

    SELECT tgobj~id,
           tgobj~tagid AS tag_id,
           tag~name AS tag_name,
           tag~owner AS owner,
           tgobj~objectname AS object_name,
           tgobj~objecttype AS object_type,
           tgobj~componentname AS component_name,
           tgobj~componenttype AS component_type,
           tgobj~parenttagid AS parent_tag_id,
           parent_tag~name AS parent_tag_name,
           tgobj~parentobjectname AS parent_object_name,
           tgobj~parentobjecttype AS parent_object_type
      FROM zabaptags_i_tgobjn AS tgobj
        INNER JOIN zabaptags_i_tag AS tag
          ON tgobj~tagid = tag~tagid
        LEFT OUTER JOIN zabaptags_i_tag AS parent_tag
          ON tgobj~parenttagid = parent_tag~tagid
        FOR ALL ENTRIES IN @select_keys-comp_obj
        WHERE tgobj~objectname    = @select_keys-comp_obj-object_name
          AND tgobj~objecttype    = @select_keys-comp_obj-object_type
          AND tgobj~componentname = @select_keys-comp_obj-component_name
          AND tgobj~componenttype = @select_keys-comp_obj-component_type
          AND tgobj~tagid         = @select_keys-comp_obj-tag_id
          AND tgobj~parenttagid   = @select_keys-comp_obj-parent_tag_id
      APPENDING CORRESPONDING FIELDS OF TABLE @found_objects.
  ENDMETHOD.


  METHOD find_obj_by_tag_id.
    CHECK select_keys-tag_id IS NOT INITIAL.

    SELECT tgobj~id,
           tgobj~tagid AS tag_id,
           tag~name AS tag_name,
           tag~owner AS owner,
           tgobj~objectname AS object_name,
           tgobj~objecttype AS object_type,
           tgobj~componentname AS component_name,
           tgobj~componenttype AS component_type,
           tgobj~parenttagid AS parent_tag_id,
           parent_tag~name AS parent_tag_name,
           tgobj~parentobjectname AS parent_object_name,
           tgobj~parentobjecttype AS parent_object_type
      FROM zabaptags_i_tgobjn AS tgobj
        INNER JOIN zabaptags_i_tag AS tag
          ON tgobj~tagid = tag~tagid
        LEFT OUTER JOIN zabaptags_i_tag AS parent_tag
          ON tgobj~parenttagid = parent_tag~tagid
      FOR ALL ENTRIES IN @list_request-tag_ids
      WHERE tgobj~tagid = @list_request-tag_ids-table_line
      APPENDING CORRESPONDING FIELDS OF TABLE @found_objects.
  ENDMETHOD.


  METHOD find_obj_by_tag_n_obj.
    CHECK select_keys-tag_and_obj IS NOT INITIAL.

    SELECT tgobj~id,
           tgobj~tagid AS tag_id,
           tag~name AS tag_name,
           tag~owner AS owner,
           tgobj~objectname AS object_name,
           tgobj~objecttype AS object_type,
           tgobj~componentname AS component_name,
           tgobj~componenttype AS component_type,
           tgobj~parenttagid AS parent_tag_id,
           parent_tag~name AS parent_tag_name,
           tgobj~parentobjectname AS parent_object_name,
           tgobj~parentobjecttype AS parent_object_type
      FROM zabaptags_i_tgobjn AS tgobj
        INNER JOIN zabaptags_i_tag AS tag
          ON tgobj~tagid = tag~tagid
        LEFT OUTER JOIN zabaptags_i_tag AS parent_tag
          ON tgobj~parenttagid = parent_tag~tagid
        FOR ALL ENTRIES IN @select_keys-tag_and_obj
        WHERE tgobj~objectname    = @select_keys-tag_and_obj-object_name
          AND tgobj~objecttype    = @select_keys-tag_and_obj-object_type
          AND tgobj~componentname = @space
          AND tgobj~componenttype = @space
          AND tgobj~tagid         = @select_keys-tag_and_obj-tag_id
      APPENDING CORRESPONDING FIELDS OF TABLE @found_objects.
  ENDMETHOD.


  METHOD load_assigned_child_objects.
    DATA(parent_objects) = found_objects.

    WHILE parent_objects IS NOT INITIAL.
      SELECT tgobj~id,
             tgobj~tagid AS tag_id,
             tag~name AS tag_name,
             tag~owner AS owner,
             tgobj~objectname AS object_name,
             tgobj~objecttype AS object_type,
             tgobj~componentname AS component_name,
             tgobj~componenttype AS component_type,
             tgobj~parenttagid AS parent_tag_id,
             parent_tag~name AS parent_tag_name,
             tgobj~parentobjectname AS parent_object_name,
             tgobj~parentobjecttype AS parent_object_type
        FROM zabaptags_i_tgobjn AS tgobj
          INNER JOIN zabaptags_i_tag AS tag
            ON tgobj~tagid = tag~tagid
          LEFT OUTER JOIN zabaptags_i_tag AS parent_tag
            ON tgobj~parenttagid = parent_tag~tagid
        FOR ALL ENTRIES IN @parent_objects
        WHERE tgobj~parentobjectname = @parent_objects-object_name
          AND tgobj~parentobjecttype = @parent_objects-object_type
          AND tgobj~parenttagid = @parent_objects-tag_id
        INTO TABLE @DATA(temp).

      IF sy-subrc = 0.
        found_objects = VALUE #( BASE found_objects ( LINES OF temp ) ).
        parent_objects = temp.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.


  METHOD post_process_found_objects.

    SORT found_objects BY id.
    DELETE ADJACENT DUPLICATES FROM found_objects COMPARING id.

    LOOP AT found_objects REFERENCE INTO DATA(found_obj).

      DATA(new_tgobj_info) = VALUE zabaptags_tgobj_info(
        id                 = found_obj->id
        tag_id             = found_obj->tag_id
        tag_type           = COND #(
          WHEN found_obj->owner IS INITIAL THEN zif_abaptags_c_global=>tag_type-global
          WHEN found_obj->owner = sy-uname THEN zif_abaptags_c_global=>tag_type-user
          ELSE zif_abaptags_c_global=>tag_type-shared )
        tag_name           = found_obj->tag_name
        object_name        = found_obj->object_name
        component_name     = found_obj->component_name
        component_type     = found_obj->component_type
        parent_tag_id      = found_obj->parent_tag_id
        parent_tag_name    = found_obj->parent_tag_name
        parent_object_name = found_obj->parent_object_name ).

      get_adjusted_types( EXPORTING tagged_object      = found_obj->*
                          IMPORTING object_type        = new_tgobj_info-object_type
                                    parent_object_type = new_tgobj_info-parent_object_type ).

      tagged_object_infos = VALUE #( BASE tagged_object_infos ( new_tgobj_info ) ).
    ENDLOOP.

    SORT tagged_object_infos BY tag_type object_type tag_name object_name component_type component_name parent_tag_name parent_object_type parent_object_name.
  ENDMETHOD.



ENDCLASS.
