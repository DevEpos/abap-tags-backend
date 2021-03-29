"! <p class="shorttext synchronized" lang="en">ADT Resource for handling Tagging Preview</p>
CLASS zcl_abaptags_adt_res_tagprev DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      post
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tag_count,
        tag_id TYPE zabaptags_tag_id,
        count  TYPE sy-tabix,
      END OF ty_tag_count.

    DATA:
      object_refs       TYPE SORTED TABLE OF zabaptags_adt_obj_ref WITH UNIQUE KEY name tadir_type,
      preview_info      TYPE zabaptags_tag_preview_info,
      tags_raw          TYPE zabaptags_tag_data_t,
      tagged_obj_counts TYPE HASHED TABLE OF ty_tag_count WITH UNIQUE KEY tag_id.

    METHODS:
      get_content_handler
        RETURNING
          VALUE(content_handler) TYPE REF TO if_adt_rest_content_handler,
      "! <p class="shorttext synchronized" lang="en">Collect object references from request body</p>
      collect_object_refs,
      read_tags_flat,
      fill_tagged_object_info,
      determine_tagged_object_count,
      create_response_data.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tagprev IMPLEMENTATION.

  METHOD post.
    DATA(content_handler) = get_content_handler( ).
    DATA(binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).

    IF binary_data IS NOT INITIAL.
      request->get_body_data(
        EXPORTING content_handler = content_handler
        IMPORTING data            = preview_info ).
    ENDIF.

    collect_object_refs( ).
    read_tags_flat( ).
    determine_tagged_object_count( ).
    fill_tagged_object_info( ).
    create_response_data( ).

    response->set_body_data(
      content_handler = content_handler
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
          zcl_abaptags_adt_util=>map_uri_to_wb_object(
            EXPORTING uri         = <obj_ref>-uri
            IMPORTING object_name = obj_ref_int-name
                      object_type = DATA(object_type)
                      tadir_type  = obj_ref_int-tadir_type ).
          obj_ref_int-type = COND #( WHEN object_type-subtype_wb IS NOT INITIAL THEN object_type-objtype_tr && '/' && object_type-subtype_wb
                                                                                ELSE object_type-objtype_tr ).
          CONDENSE obj_ref_int-name.
          IF obj_ref_int-name CA ' '.
            DATA(whitespace_off) = find( val = obj_ref_int-name regex = '\s' ).
            IF whitespace_off <> -1.
              obj_ref_int-name = obj_ref_int-name(whitespace_off).
            ENDIF.

            obj_ref_int-uri = zcl_abaptags_adt_util=>get_adt_obj_ref( name = |{ obj_ref_int-name }| wb_type = object_type )-uri.
          ENDIF.
          INSERT obj_ref_int INTO TABLE object_refs.
        CATCH cx_adt_uri_mapping.
      ENDTRY.
      DELETE preview_info-object_refs.
    ENDLOOP.

    CLEAR preview_info.

  ENDMETHOD.


  METHOD read_tags_flat.
    SELECT *
      FROM zabaptags_tags
      WHERE owner = @sy-uname
         OR owner = @space
      INTO CORRESPONDING FIELDS OF TABLE @tags_raw.
  ENDMETHOD.

  METHOD determine_tagged_object_count.
    DATA: where TYPE TABLE OF string.

    LOOP AT object_refs ASSIGNING FIELD-SYMBOL(<obj_ref>).
      DATA(operator) = COND #( WHEN sy-tabix <> 1 THEN ` OR ` ELSE `` ).

      where = VALUE #( BASE where
        ( |{ operator }( OBJECT_TYPE = { cl_abap_dyn_prg=>quote( <obj_ref>-tadir_type ) } | &&
          |AND OBJECT_NAME = { cl_abap_dyn_prg=>quote( <obj_ref>-name ) } )| ) ).
    ENDLOOP.

    SELECT tag_id, COUNT( * ) AS count
      FROM zabaptags_tgobj
      WHERE (where)
      GROUP BY tag_id
      INTO CORRESPONDING FIELDS OF TABLE @tagged_obj_counts.
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
    preview_info-tags = zcl_abaptags_tag_util=>build_hierarchical_tags( tags_raw ).
    preview_info-object_refs = CORRESPONDING #( object_refs ).
  ENDMETHOD.

ENDCLASS.
