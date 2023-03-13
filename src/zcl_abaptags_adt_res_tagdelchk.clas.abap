"! <p class="shorttext synchronized" lang="en">Resource to check Tag Deletion</p>
CLASS zcl_abaptags_adt_res_tagdelchk DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      tags_to_delete TYPE zabaptags_tag_data_t,
      check_result   TYPE zabaptags_tags_delchk_result.

    METHODS:
      get_response_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_request_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tagdelchk IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = tags_to_delete ).

    response->set_body_data(
      content_handler = get_response_content_handler( )
      data            = check_result ).
  ENDMETHOD.


  METHOD get_request_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TAGS'
      root_name    = 'TAGS'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.


  METHOD get_response_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TAGS_DELCHK_RES'
      root_name    = 'CHECK_RESULT'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.

ENDCLASS.
