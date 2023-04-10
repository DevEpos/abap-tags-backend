"! <p class="shorttext synchronized" lang="en">Resource for deleting Tagged Objects</p>
CLASS zcl_abaptags_adt_res_tgobjdel DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: object_ids_for_deletion TYPE zabaptags_raw16_t.

    METHODS:
      get_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      delete_tagged_objects.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobjdel IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_content_handler( )
                            IMPORTING data            = object_ids_for_deletion ).

    IF object_ids_for_deletion IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>no_tgobj_id_supplied.
    ENDIF.

    delete_tagged_objects( ).
  ENDMETHOD.


  METHOD get_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TGOBJ_DEL_REQUEST'
      root_name    = 'REQUEST'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.


  METHOD delete_tagged_objects.
    DATA: id_tab TYPE TABLE OF zabaptags_tgobjn.

    id_tab = VALUE #( FOR id IN object_ids_for_deletion ( id = id ) ).

    IF id_tab IS NOT INITIAL.
      DELETE zabaptags_tgobjn FROM TABLE id_tab.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
