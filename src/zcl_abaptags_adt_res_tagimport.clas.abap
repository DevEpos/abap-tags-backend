"! <p class="shorttext synchronized">Resource for ABAP Tags Import</p>
CLASS zcl_abaptags_adt_res_tagimport DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA import_request TYPE zabaptags_data_export.

    METHODS get_request_content_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS import_tags.
    METHODS import_tagged_objects.
    METHODS import_shared_tags.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tagimport IMPLEMENTATION.
  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = import_request ).

    import_tags( ).
    import_tagged_objects( ).
    import_shared_tags( ).
  ENDMETHOD.

  METHOD get_request_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                 st_name      = 'ZABAPTAGS_TAG_IMPORT_REQUEST'
                 root_name    = 'REQUEST'
                 content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.

  METHOD import_tags.
  ENDMETHOD.

  METHOD import_tagged_objects.
  ENDMETHOD.

  METHOD import_shared_tags.
  ENDMETHOD.
ENDCLASS.
