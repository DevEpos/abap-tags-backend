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
        object_type        TYPE swo_objtyp,
        component_name     TYPE zabaptags_obj_comp_name,
        component_type     TYPE swo_objtyp,
        parent_tag_id      TYPE zabaptags_tag_id,
        parent_tag_name    TYPE zabaptags_tag_name,
        parent_object_name TYPE sobj_name,
        parent_object_type TYPE swo_objtyp,
      END OF ty_tagged_object,

      ty_tagged_objects TYPE STANDARD TABLE OF ty_tagged_object WITH EMPTY KEY.

    DATA: list_request        TYPE zabaptags_tgobj_list_request,
          tagged_object_infos TYPE zabaptags_tgobj_info_t,
          shared_tags_range   TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    METHODS:
      get_request_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_response_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_tagged_objects,
      get_tgobj_infos_by_tag_id,
      get_adjusted_types
        IMPORTING
          tagged_object      TYPE ty_tagged_object
        EXPORTING
          object_type        TYPE swo_objtyp
          parent_object_type TYPE swo_objtyp.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobjlist IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_handler( )
                            IMPORTING data            = list_request ).

    get_tagged_objects( ).

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
      get_tgobj_infos_by_tag_id( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_tgobj_infos_by_tag_id.
    DATA: found_objects TYPE ty_tagged_objects.

    CHECK list_request-tag_ids IS NOT INITIAL.

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
      INTO CORRESPONDING FIELDS OF TABLE @found_objects.

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

    SORT tagged_object_infos BY tag_type object_type object_name component_type component_name.

  ENDMETHOD.


  METHOD get_adjusted_types.
    DATA(wb_object_type) = cl_wb_object_type=>create_from_exttype( p_external_id = tagged_object-object_type ).
    DATA(main_global_type) = wb_object_type->get_main_global_type( ).
    object_type = |{ main_global_type-objtype_tr }/{ main_global_type-subtype_wb }|.

    IF tagged_object-parent_object_type IS NOT INITIAL AND
        tagged_object-parent_object_name IS NOT INITIAL.
      wb_object_type = cl_wb_object_type=>create_from_exttype( p_external_id = tagged_object-parent_object_type ).
      main_global_type = wb_object_type->get_main_global_type( ).
      parent_object_type = |{ main_global_type-objtype_tr }/{ main_global_type-subtype_wb }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
