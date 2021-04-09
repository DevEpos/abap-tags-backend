"! <p class="shorttext synchronized" lang="en">Resource for Sharing Tags</p>
CLASS zcl_abaptags_adt_res_tagsshare DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,
      post
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        share   TYPE string VALUE 'share',
        unshare TYPE string VALUE 'unshare',
      END OF c_action.

    DATA:
      tag_user_map TYPE zabaptags_shared_tag_t,
      tag_repo     TYPE REF TO zcl_abaptags_tags_dac.

    METHODS:
      get_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      unshare_tags,
      share_tags.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tagsshare IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    tag_repo = zcl_abaptags_tags_dac=>get_instance( ).
  ENDMETHOD.


  METHOD post.
    DATA(binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).
    IF binary_data IS NOT INITIAL.
      request->get_body_data(
         EXPORTING content_handler = get_content_handler(  )
         IMPORTING data            = tag_user_map ).
    ENDIF.

    IF tag_user_map IS INITIAL.
      RETURN.
    ENDIF.

    DATA(unshare) = zcl_abaptags_adt_request_util=>get_boolean_req_param(
      param_name = c_action-unshare
      request    = request ).

    IF unshare = abap_true.
      unshare_tags( ).
    ELSE.
      share_tags( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_SHARED_TAGS'
      root_name    = 'SHARED_TAGS'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.


  METHOD unshare_tags.
    tag_repo->delete_shared_tags( VALUE #(
      FOR shared_tag IN tag_user_map ( sign = 'I' option = 'EQ' low = shared_tag-tag_id ) ) ).
  ENDMETHOD.


  METHOD share_tags.
    DATA: tags_to_be_shared    TYPE SORTED TABLE OF zabaptags_shtags WITH UNIQUE KEY tag_id shared_user,
          shared_tags          LIKE tags_to_be_shared,
          tag_ids_to_be_shared TYPE RANGE OF zabaptags_tag_id,
          update_relevant_tags TYPE abap_bool.

    LOOP AT tag_user_map ASSIGNING FIELD-SYMBOL(<tag_user_map_entry>).

      LOOP AT <tag_user_map_entry>-users ASSIGNING FIELD-SYMBOL(<user>) WHERE name <> sy-uname.
        INSERT VALUE #(
          tag_id = <tag_user_map_entry>-tag_id
          shared_user = <user>-name ) INTO TABLE tags_to_be_shared.
      ENDLOOP.

      IF sy-subrc = 0.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <tag_user_map_entry>-tag_id ) TO tag_ids_to_be_shared.
      ENDIF.
    ENDLOOP.

    IF tags_to_be_shared IS INITIAL.
      RETURN.
    ENDIF.

    shared_tags = tag_repo->find_shared_tags_db( tag_ids_to_be_shared ).

    IF shared_tags IS NOT INITIAL.

      LOOP AT tags_to_be_shared ASSIGNING FIELD-SYMBOL(<tag_to_be_shared>).
        IF line_exists( shared_tags[
             tag_id      = <tag_to_be_shared>-tag_id
             shared_user = <tag_to_be_shared>-shared_user ] ).
          DELETE tags_to_be_shared.
          update_relevant_tags = abap_true.
        ENDIF.
      ENDLOOP.

      IF update_relevant_tags = abap_true.
        tag_ids_to_be_shared = VALUE #(
          FOR <tag> IN tags_to_be_shared ( sign = 'I' option = 'EQ' low = <tag>-tag_id ) ).
        DELETE ADJACENT DUPLICATES FROM tag_ids_to_be_shared COMPARING low.
      ENDIF.

    ENDIF.

    tag_repo->share_tags(
      tag_ids       = tag_ids_to_be_shared
      tags_to_share = CORRESPONDING #( tags_to_be_shared ) ).

  ENDMETHOD.


ENDCLASS.
