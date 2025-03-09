"! <p class="shorttext synchronized">Resource for ABAP Tags Export</p>
CLASS zcl_abaptags_adt_res_tagexport DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA export_request TYPE zabaptags_tag_export_request.
    DATA export_response TYPE zabaptags_data_export.
    DATA tag_id_range TYPE zif_abaptags_ty_global=>ty_tag_id_range.
    DATA flat_tags TYPE zabaptags_tag_data_t.

    METHODS get_request_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS get_tags.
    METHODS get_tagged_objects.
    METHODS build_tag_trees.

    METHODS get_response_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS get_shared_tags.

    METHODS validate_parent_object
      IMPORTING
        tadir_reader TYPE REF TO zcl_abaptags_tadir
      CHANGING
        tgobj_info   TYPE zabaptags_tgobj_info.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tagexport IMPLEMENTATION.
  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_handler( )
                            IMPORTING data            = export_request ).

    get_tags( ).
    get_tagged_objects( ).
    get_shared_tags( ).
    build_tag_trees( ).

    response->set_body_data( content_handler = get_response_handler( )
                             data            = export_response  ).
  ENDMETHOD.

  METHOD get_request_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                 st_name   = 'ZABAPTAGS_TAG_EXPORT_REQUEST'
                 root_name = 'REQUEST' ).
  ENDMETHOD.

  METHOD get_response_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                 st_name   = 'ZABAPTAGS_TAG_EXPORT_RESPONSE'
                 root_name = 'RESPONSE' ).
  ENDMETHOD.

  METHOD get_tags.
    tag_id_range = VALUE #( FOR <tag_id> IN export_request-tag_ids ( sign = 'I' option = 'EQ' low = <tag_id> ) ).
    flat_tags = zcl_abaptags_tags_dac=>get_instance( )->find_tags( owner_range  = VALUE #( sign   = 'I'
                                                                                           option = 'EQ'
                                                                                           ( low = space )
                                                                                           ( low = sy-uname ) )
                                                                   tag_id_range = tag_id_range ).
  ENDMETHOD.

  METHOD get_tagged_objects.
    DATA adt_object_ref TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.

    " 1) retrieve flat tagged objects
    SELECT tgobj_db~id,
           tgobj_db~objecttype       AS object_type,
           tgobj_db~objectname       AS object_name,
           tgobj_db~componentname    AS component_name,
           tgobj_db~componenttype    AS component_type,
           tgobj_db~tagid            AS tag_id,
           tag~name                  AS tag_name,
           tag~owner                 AS owner,
           tgobj_db~parenttagid      AS parent_tag_id,
           tgobj_db~parentobjecttype AS parent_object_type,
           tgobj_db~parentobjectname AS parent_object_name,
           parent_tag~name           AS parent_tag_name,
           orig_parent_tag~tagid     AS orig_parent_tag_id,
           orig_parent_tag~name      AS orig_parent_tag_name
      FROM zabaptags_i_tgobjn AS tgobj_db
           INNER JOIN zabaptags_i_tag AS tag
             ON tgobj_db~tagid = tag~tagid
           LEFT OUTER JOIN zabaptags_i_tag AS parent_tag
             ON tgobj_db~parenttagid = parent_tag~tagid
           LEFT OUTER JOIN zabaptags_i_tag AS orig_parent_tag
             ON orig_parent_tag~tagid = tag~parenttagid
      WHERE tgobj_db~tagid IN @tag_id_range
      INTO TABLE @DATA(tagged_objects).

    " 2) Validate the existence of the tagged objects
    DATA(tadir_info_reader) = NEW zcl_abaptags_tadir( CORRESPONDING #( tagged_objects MAPPING name = object_name type = object_type ) )->determine_tadir_entries( ).
    DATA(parent_obj_tadir_reader) = NEW zcl_abaptags_tadir( VALUE #( FOR <tgobj_db> IN tagged_objects
                                                                     WHERE ( parent_object_name IS NOT INITIAL AND parent_object_type IS NOT INITIAL )
                                                                     ( name = <tgobj_db>-parent_object_name
                                                                       type = <tgobj_db>-parent_object_type ) ) )->determine_tadir_entries( ).

    DATA(comp_adt_mapper) = NEW zcl_abaptags_comp_adt_mapper( ).
    comp_adt_mapper->add_components( VALUE #( FOR <mo> IN tagged_objects WHERE ( component_name IS NOT INITIAL )
                                              ( VALUE #( component_name = <mo>-component_name
                                                         component_type = <mo>-component_type
                                                         object_name    = <mo>-object_name
                                                         object_type    = <mo>-object_type ) ) ) ).
    comp_adt_mapper->determine_components( ).

    LOOP AT tagged_objects REFERENCE INTO DATA(tgobj).
      TRY.
          tadir_info_reader->get_tadir_info( name = tgobj->object_name
                                             type = tgobj->object_type  ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      IF tgobj->component_name IS NOT INITIAL.
        adt_object_ref = comp_adt_mapper->get_adt_object( VALUE #( object_name    = tgobj->object_name
                                                                   object_type    = tgobj->object_type
                                                                   component_name = tgobj->component_name
                                                                   component_type = tgobj->component_type ) ).

      ELSE.
        adt_object_ref = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type( tadir_type = tgobj->object_type
                                                                                name       = tgobj->object_name ).
      ENDIF.

      IF adt_object_ref-uri IS INITIAL.
        " No URI means that the TADIR object or the component does not longer exist
        CONTINUE.
      ENDIF.

      DATA(tgobj_info) = CORRESPONDING zabaptags_tgobj_info( tgobj->* ).
      tgobj_info-object_type = adt_object_ref-type.
      tgobj_info-tag_type    = COND #( WHEN tgobj->owner IS INITIAL THEN zif_abaptags_c_global=>tag_type-global
                                       WHEN tgobj->owner = sy-uname THEN zif_abaptags_c_global=>tag_type-user ).

      validate_parent_object( EXPORTING tadir_reader = parent_obj_tadir_reader
                              CHANGING  tgobj_info   = tgobj_info ).
      IF tgobj_info-parent_tag_id IS INITIAL.
        tgobj_info-parent_tag_id   = tgobj->orig_parent_tag_id.
        tgobj_info-parent_tag_name = tgobj->orig_parent_tag_name.
      ENDIF.

      export_response-tagged_objects = VALUE #( BASE export_response-tagged_objects ( tgobj_info ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_shared_tags.
    CHECK export_request-include_shared_tags_info = abap_true.

    DATA(shared_tags) = zcl_abaptags_tags_dac=>get_instance( )->find_shared_tags_db( tag_ids = tag_id_range ).

    LOOP AT shared_tags REFERENCE INTO DATA(shared_tag) GROUP BY shared_tag->tag_id.
      export_response-shared_tags = VALUE #( BASE export_response-shared_tags
                                             ( tag_id = shared_tag->tag_id
                                               users  = VALUE #( FOR user IN GROUP shared_tag
                                                                 ( name = user-shared_user ) ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD build_tag_trees.
    " Note: ZCL_ABAPTAGS_TAGS_DAC->GET_TAGGED_OBJ_COUNT is not used as we need the count of all 'non-deleted' objects
    LOOP AT export_response-tagged_objects REFERENCE INTO DATA(tgobj) GROUP BY ( tag_id = tgobj->tag_id
                                                                                 size   = GROUP SIZE )
         WITHOUT MEMBERS REFERENCE INTO DATA(tgobj_group).
      DATA(tag) = REF #( flat_tags[ KEY tag_id
                                    tag_id = tgobj_group->tag_id ] OPTIONAL ).
      IF tag IS NOT INITIAL.
        tag->tagged_object_count = tgobj_group->size.
      ENDIF.
    ENDLOOP.

    export_response-tags = zcl_abaptags_tag_util=>build_hierarchical_tags( tags_flat = flat_tags ).
  ENDMETHOD.

  METHOD validate_parent_object.
    DATA adt_object_ref TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.

    IF tgobj_info-parent_object_name IS INITIAL OR tgobj_info-parent_object_type IS INITIAL.
      CLEAR tgobj_info-parent_tag_id. " not required during import/export
      RETURN.
    ENDIF.

    DATA(clear_parent) = abap_false.
    TRY.
        tadir_reader->get_tadir_info( name = tgobj_info-parent_object_name
                                      type = CONV #( tgobj_info-parent_object_type ) ).
        adt_object_ref = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
                             tadir_type = CONV #( tgobj_info-parent_object_type )
                             name       = tgobj_info-parent_object_name ).
        IF adt_object_ref IS INITIAL.
          clear_parent = abap_true.
        ELSE.
          tgobj_info-parent_object_type = adt_object_ref-type.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        clear_parent = abap_true.
    ENDTRY.

    IF clear_parent = abap_true.
      CLEAR: tgobj_info-parent_object_name,
             tgobj_info-parent_object_type,
             tgobj_info-parent_tag_id,
             tgobj_info-parent_tag_name.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
