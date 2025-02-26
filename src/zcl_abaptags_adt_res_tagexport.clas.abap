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

    METHODS get_request_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS get_tags.
    METHODS get_tagged_objects.

    METHODS get_response_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS get_shared_tags.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tagexport IMPLEMENTATION.
  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_handler( )
                            IMPORTING data            = export_request ).

    get_tags( ).
    get_tagged_objects( ).
    get_shared_tags( ).

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
    DATA(tags) = zcl_abaptags_tags_dac=>get_instance( )->find_tags( owner_range  = VALUE #( sign   = 'I'
                                                                                            option = 'EQ'
                                                                                            ( low = space )
                                                                                            ( low = sy-uname ) )
                                                                    tag_id_range = tag_id_range ).

    export_response-tags = zcl_abaptags_tag_util=>build_hierarchical_tags( tags_flat = tags ).
  ENDMETHOD.

  METHOD get_tagged_objects.
    DATA adt_object_ref TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.

    " 1) retrieve flat tagged objects
    SELECT objecttype       AS object_type,
           objectname       AS object_name,
           componentname    AS component_name,
           componenttype    AS component_type,
           tagid            AS tag_id,
           parenttagid      AS parent_tag_id,
           parentobjecttype AS parent_object_type,
           parentobjectname AS parent_object_name
      FROM zabaptags_i_tgobjn
      WHERE tagid IN @tag_id_range
      INTO TABLE @DATA(tagged_objects).

    " 2) Validate the existence of the tagged objects
    DATA(tadir_info_reader) = NEW zcl_abaptags_tadir( CORRESPONDING #( tagged_objects MAPPING name = object_name type = object_type ) )->determine_tadir_entries( ).

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

      export_response-tagged_objects = VALUE #( BASE export_response-tagged_objects
                                                ( CORRESPONDING #( tgobj->* ) ) ).
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
ENDCLASS.
