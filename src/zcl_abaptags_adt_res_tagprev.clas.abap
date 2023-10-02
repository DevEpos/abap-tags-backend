"! <p class="shorttext synchronized">ADT Resource for handling Tagging Preview</p>
CLASS zcl_abaptags_adt_res_tagprev DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA tags_dac TYPE REF TO zcl_abaptags_tags_dac.
    DATA object_refs TYPE SORTED TABLE OF zabaptags_adt_obj_ref WITH UNIQUE KEY name tadir_type.
    DATA preview_info TYPE zabaptags_tag_preview_info.
    DATA tags_raw TYPE zabaptags_tag_data_t.
    DATA tagged_obj_counts TYPE zif_abaptags_ty_global=>ty_tag_counts.
    DATA obj_refs_for_mapping TYPE TABLE OF REF TO zabaptags_adt_obj_ref.
    DATA cds_name_mapper TYPE REF TO zcl_abaptags_cds_name_mapper.

    METHODS get_content_handler
      RETURNING
        VALUE(content_handler) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Collect object references from request body</p>
    METHODS collect_object_refs.
    METHODS read_tags_flat.
    METHODS fill_tagged_object_info.
    METHODS determine_tagged_object_count.
    METHODS create_response_data.
    METHODS fill_cds_display_names.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tagprev IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    tags_dac = zcl_abaptags_tags_dac=>get_instance( ).
    cds_name_mapper = NEW #( ).
  ENDMETHOD.

  METHOD post.
    DATA(content_handler) = get_content_handler( ).
    DATA(binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).

    IF binary_data IS NOT INITIAL.
      request->get_body_data( EXPORTING content_handler = content_handler
                              IMPORTING data            = preview_info ).
    ENDIF.

    collect_object_refs( ).
    read_tags_flat( ).
    determine_tagged_object_count( ).
    fill_tagged_object_info( ).
    fill_cds_display_names( ).
    create_response_data( ).

    response->set_body_data( content_handler = content_handler
                             data            = preview_info ).
  ENDMETHOD.

  METHOD get_content_handler.
    content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                          st_name      = 'ZABAPTAGS_TAG_PREVIEW_INFO'
                          root_name    = 'TAG_PREVIEW_INFO'
                          content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.

  METHOD collect_object_refs.
    LOOP AT preview_info-object_refs ASSIGNING FIELD-SYMBOL(<obj_ref>).
      DATA(obj_ref_int) = CORRESPONDING zabaptags_adt_obj_ref( <obj_ref> ).
      TRY.
          zcl_abaptags_adt_util=>map_uri_to_wb_object( EXPORTING uri         = <obj_ref>-uri
                                                       IMPORTING object_name = obj_ref_int-name
                                                                 object_type = DATA(object_type)
                                                                 tadir_type  = obj_ref_int-tadir_type ).

          IF    <obj_ref>-type = zif_abaptags_c_global=>wb_object_types-local_class
             OR <obj_ref>-type = zif_abaptags_c_global=>wb_object_types-local_interface.
            DATA(glob_class_name) = COND #(
              WHEN strlen( obj_ref_int-name ) > 30 THEN obj_ref_int-name(30) ELSE obj_ref_int-name ).
            obj_ref_int-parent_name = condense( translate( val = glob_class_name from = '=' to = space ) ).
            obj_ref_int-name        = <obj_ref>-name.
          ELSE.
            obj_ref_int-type = COND #( WHEN object_type-subtype_wb IS NOT INITIAL
                                       THEN object_type-objtype_tr && '/' && object_type-subtype_wb
                                       ELSE object_type-objtype_tr ).
            CONDENSE obj_ref_int-name.
            IF obj_ref_int-name CA ' '.
              DATA(whitespace_off) = find( val = obj_ref_int-name regex = '\s' ).
              IF whitespace_off <> -1.
                obj_ref_int-name = obj_ref_int-name(whitespace_off).
              ENDIF.

              obj_ref_int-uri = zcl_abaptags_adt_util=>get_adt_obj_ref( name    = |{ obj_ref_int-name }|
                                                                        wb_type = object_type )-uri.
            ENDIF.
          ENDIF.

          INSERT obj_ref_int INTO TABLE object_refs REFERENCE INTO DATA(added_obj_ref).
          IF cds_name_mapper->collect_entry( name = obj_ref_int-name
                                             type = obj_ref_int-tadir_type ).
            obj_refs_for_mapping = VALUE #( BASE obj_refs_for_mapping ( added_obj_ref  ) ).
          ENDIF.
        CATCH cx_adt_uri_mapping.
      ENDTRY.
      DELETE preview_info-object_refs.
    ENDLOOP.

    CLEAR preview_info.
  ENDMETHOD.

  METHOD read_tags_flat.
    tags_raw = VALUE #( ( LINES OF tags_dac->find_tags( owner_range = VALUE #( sign   = 'I'
                                                                               option = 'EQ'
                                                                               ( low = sy-uname )
                                                                               ( low = space  ) ) ) )
                        ( LINES OF zcl_abaptags_tag_util=>get_shared_tags( abap_true ) ) ).
  ENDMETHOD.

  METHOD determine_tagged_object_count.
    tagged_obj_counts = tags_dac->get_tagged_obj_count( object_refs = CORRESPONDING #( object_refs ) ).
  ENDMETHOD.

  METHOD fill_tagged_object_info.
    LOOP AT tags_raw ASSIGNING FIELD-SYMBOL(<tag>).
      ASSIGN tagged_obj_counts[ tag_id = <tag>-tag_id ] TO FIELD-SYMBOL(<tag_count>).
      IF sy-subrc = 0.
        <tag>-tagged_object_count = <tag_count>-count.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_response_data.
    preview_info-tags        = zcl_abaptags_tag_util=>build_hierarchical_tags( tags_raw ).
    preview_info-object_refs = CORRESPONDING #( object_refs ).
  ENDMETHOD.

  METHOD fill_cds_display_names.
    CHECK cds_name_mapper->map_entries( ).

    LOOP AT obj_refs_for_mapping INTO DATA(obj_ref).
      obj_ref->alt_name = cds_name_mapper->get_display_name( name = obj_ref->name
                                                             type = obj_ref->tadir_type ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
