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
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      run_deletion_check.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tagdelchk IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = tags_to_delete ).


    IF tags_to_delete IS NOT INITIAL.
      run_deletion_check( ).
    ENDIF.

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


  METHOD run_deletion_check.
    DATA: tags_range TYPE RANGE OF zabaptags_tag_id.

    tags_range = VALUE #( FOR <t> IN tags_to_delete ( sign = 'I' option = 'EQ' low = <t>-tag_id ) ).

    " run check against selected nodes first
    SELECT aggr~tag_id,
           COUNT(*) AS count
      FROM zabaptags_i_taggedobjaggr AS aggr
      WHERE aggr~tag_id IN @tags_range
      GROUP BY aggr~tag_id
      INTO TABLE @DATA(counts).

    " no deeper checks necessary
    LOOP AT counts ASSIGNING FIELD-SYMBOL(<tag_obj_count>).
      check_result-tags = VALUE #( BASE check_result-tags
        ( tag_id       = <tag_obj_count>-tag_id
          message      = |Still { <tag_obj_count>-count } | && COND #(
            WHEN <tag_obj_count>-count > 1 THEN `objects` ELSE `object` ) && ` assigned`
          message_type = 'ERROR' ) ).
    ENDLOOP.

    IF lines( counts ) <> lines( tags_to_delete ).
      " check if there are child tags with tagged objects
      SELECT tag~tag_id,
             COUNT(*) AS count
        FROM zabaptags_i_childtgobj AS tag
        WHERE tag~tag_id IN @tags_range
        GROUP BY tag~tag_id
        INTO TABLE @DATA(root_counts).

      LOOP AT root_counts ASSIGNING FIELD-SYMBOL(<root_count>).
        check_result-tags = VALUE #( BASE check_result-tags
          ( tag_id       = <root_count>-tag_id
            message      = |Still { <root_count>-count } | && COND #(
              WHEN <root_count>-count > 1 THEN `objects` ELSE `object` ) && ` in lower tree levels assigned`
            message_type = 'ERROR' ) ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
