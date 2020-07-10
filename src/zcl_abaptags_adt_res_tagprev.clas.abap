"! <p class="shorttext synchronized" lang="en">ADT Resource for handling Tagging Preview</p>
CLASS zcl_abaptags_adt_res_tagprev DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_tag_count,
        tag_id TYPE zabaptags_tag_id,
        count  TYPE sy-tabix,
      END OF ty_s_tag_count.

    DATA mt_object_refs TYPE SORTED TABLE OF zabaptags_adt_obj_ref WITH UNIQUE KEY name tadir_type.
    DATA ms_preview_info TYPE zabaptags_tag_preview_info.
    DATA mt_tags_raw TYPE zabaptags_tag_data_t.
    DATA mt_tagged_obj_count TYPE HASHED TABLE OF ty_s_tag_count WITH UNIQUE KEY tag_id.

    METHODS get_content_handler
      RETURNING
        VALUE(ro_content_handler) TYPE REF TO if_adt_rest_content_handler.
    "! <p class="shorttext synchronized" lang="en">Collect object references from request body</p>
    METHODS collect_object_refs.
    METHODS read_tags_flat.
    METHODS fill_tagged_object_info.
    METHODS determine_tagged_object_count.
    METHODS create_response_data.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tagprev IMPLEMENTATION.

  METHOD post.
    DATA(lo_content_handler) = get_content_handler( ).
    DATA(lv_binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).

    IF lv_binary_data IS NOT INITIAL.
      request->get_body_data(
         EXPORTING content_handler = lo_content_handler
         IMPORTING data            = ms_preview_info
      ).
    ENDIF.

    collect_object_refs( ).
    read_tags_flat( ).
    determine_tagged_object_count( ).
    fill_tagged_object_info( ).
    create_response_data( ).

    response->set_body_data(
        content_handler = lo_content_handler
        data            = ms_preview_info
    ).
  ENDMETHOD.

  METHOD get_content_handler.
    ro_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
       st_name      = 'ZABAPTAGS_TAG_PREVIEW_INFO'
       root_name    = 'TAG_PREVIEW_INFO'
       content_type = if_rest_media_type=>gc_appl_xml
    ).
  ENDMETHOD.


  METHOD collect_object_refs.

    LOOP AT ms_preview_info-object_refs ASSIGNING FIELD-SYMBOL(<ls_obj_ref>).
      DATA(ls_obj_ref_int) = CORRESPONDING zabaptags_adt_obj_ref( <ls_obj_ref> ).
      TRY.
          zcl_abaptags_adt_util=>map_uri_to_wb_object(
            EXPORTING iv_uri         = <ls_obj_ref>-uri
            IMPORTING ev_object_name = ls_obj_ref_int-name
                      es_object_type = DATA(ls_object_type)
                      ev_tadir_type  = ls_obj_ref_int-tadir_type
          ).
          ls_obj_ref_int-type = COND #( WHEN ls_object_type-subtype_wb IS NOT INITIAL THEN ls_object_type-objtype_tr && '/' && ls_object_type-subtype_wb
                                                                                      ELSE ls_object_type-objtype_tr ).
          CONDENSE ls_obj_ref_int-name.
          IF ls_obj_ref_int-name CA ' '.
            DATA(lv_whitespace) = find( val = ls_obj_ref_int-name regex = '\s' ).
            IF lv_whitespace <> -1.
              ls_obj_ref_int-name = ls_obj_ref_int-name(lv_whitespace).
            ENDIF.

            ls_obj_ref_int-uri = zcl_abaptags_adt_util=>get_adt_obj_ref( iv_name = |{ ls_obj_ref_int-name }| is_type = ls_object_type )-uri.
          ENDIF.
          INSERT ls_obj_ref_int INTO TABLE mt_object_refs.
        CATCH cx_adt_uri_mapping.
      ENDTRY.
      DELETE ms_preview_info-object_refs.
    ENDLOOP.

    CLEAR ms_preview_info.

  ENDMETHOD.


  METHOD read_tags_flat.
    SELECT *
      FROM zabaptags_tags
      WHERE owner = @sy-uname
         OR owner = @space
    INTO CORRESPONDING FIELDS OF TABLE @mt_tags_raw.
  ENDMETHOD.

  METHOD determine_tagged_object_count.
    DATA: lt_where TYPE TABLE OF string.

    LOOP AT mt_object_refs ASSIGNING FIELD-SYMBOL(<ls_obj_ref>).
      DATA(lv_operator) = COND #( WHEN sy-tabix <> 1 THEN ` OR ` ELSE `` ).

      lt_where = VALUE #( BASE lt_where
        ( |{ lv_operator }( OBJECT_TYPE = { cl_abap_dyn_prg=>quote( <ls_obj_ref>-tadir_type ) } | &&
          |AND OBJECT_NAME = { cl_abap_dyn_prg=>quote( <ls_obj_ref>-name ) } )| )
      ).
    ENDLOOP.

    SELECT tag_id, COUNT( * ) AS count
      FROM zabaptags_tgobj
      WHERE (lt_where)
      GROUP BY tag_id
    INTO CORRESPONDING FIELDS OF TABLE @mt_tagged_obj_count.
  ENDMETHOD.

  METHOD fill_tagged_object_info.
    LOOP AT mt_tags_raw ASSIGNING FIELD-SYMBOL(<ls_tag>).
      ASSIGN mt_tagged_obj_count[ tag_id = <ls_tag>-tag_id ] TO FIELD-SYMBOL(<ls_tag_count>).
      IF sy-subrc = 0.
        <ls_tag>-tagged_object_count = <ls_tag_count>-count.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_response_data.
    ms_preview_info-tags = zcl_abaptags_tag_util=>build_hierarchical_tags( mt_tags_raw ).
    ms_preview_info-object_refs = CORRESPONDING #( mt_object_refs ).
  ENDMETHOD.

ENDCLASS.
