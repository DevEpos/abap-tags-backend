"! <p class="shorttext synchronized" lang="en">Resource for Sharing Tags</p>
CLASS zcl_abaptags_adt_res_tagsshare DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        share   TYPE string VALUE 'share',
        unshare TYPE string VALUE 'unshare',
      END OF c_action.

    DATA shared_tags TYPE zabaptags_shared_tag_t.

    METHODS get_content_handler
      RETURNING
        VALUE(content_handler) TYPE REF TO if_adt_rest_content_handler.
    METHODS unshare_tags.
    METHODS share_tags.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tagsshare IMPLEMENTATION.

  METHOD post.
    DATA(binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).
    IF binary_data IS NOT INITIAL.
      request->get_body_data(
         EXPORTING content_handler = get_content_handler(  )
         IMPORTING data            = shared_tags
      ).
    ENDIF.

    CHECK shared_tags IS NOT INITIAL.

    DATA(unshare) = zcl_abaptags_adt_request_util=>get_boolean_req_param(
      param_name = 'unshare'
      request    = request
    ).

    IF unshare = abap_true.
      unshare_tags( ).
    ELSE.
      share_tags( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_content_handler.
    content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
       st_name      = 'ZABAPTAGS_SHARED_TAGS'
       root_name    = 'SHARED_TAGS'
       content_type = if_rest_media_type=>gc_appl_xml
    ).
  ENDMETHOD.


  METHOD unshare_tags.
    DATA: tag_ids TYPE RANGE OF zabaptags_tag_id.

    tag_ids = VALUE #( FOR shared_tag IN shared_tags ( sign = 'I' option = 'EQ' low = shared_tag-tag_id ) ).

    DELETE FROM zabaptags_shtags WHERE tag_id IN @tag_ids.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD share_tags.

  ENDMETHOD.

ENDCLASS.
