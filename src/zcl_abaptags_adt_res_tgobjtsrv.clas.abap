"! <p class="shorttext synchronized" lang="en">Resource for Tagged object tree services</p>
CLASS zcl_abaptags_adt_res_tgobjtsrv DEFINITION
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
      object_name_range        TYPE RANGE OF sobj_name,
      object_type_range        TYPE RANGE OF trobjtype,
      parent_object_name_range TYPE RANGE OF sobj_name,
      parent_object_type_range TYPE RANGE OF trobjtype,
      search_params            TYPE zabaptags_tgobj_search_params,
      owner_range              TYPE RANGE OF sy-uname,
      tree_result              TYPE zabaptags_tgobj_tree_result,
      tag_id_range             TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    METHODS:
      determine_obj_name_range
        IMPORTING
          query TYPE string
        RAISING
          cx_adt_rest,
      get_parameters
        IMPORTING
          io_request TYPE REF TO if_adt_rest_request
        RAISING
          cx_adt_rest,
      get_response_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_request_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_matching_tags,
      get_matching_objects.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobjtsrv IMPLEMENTATION.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = search_params ).

    get_parameters( request ).
    " retrieves tags at current level that have object or child tags
    get_matching_tags( ).
    " retrieves objects at the current level
    get_matching_objects( ).

    response->set_body_data(
      content_handler = get_response_content_handler( )
      data            = tree_result ).
  ENDMETHOD.


  METHOD get_parameters.
    DATA(scope) = COND #(
      WHEN search_params-search_scope IS INITIAL THEN zif_abaptags_c_global=>scopes-all
      ELSE search_params-search_scope ).
    owner_range = SWITCH #(  scope
      WHEN zif_abaptags_c_global=>scopes-global THEN VALUE #( ( sign = 'I' option = 'EQ' low = space ) )
      WHEN zif_abaptags_c_global=>scopes-user   THEN VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ) ).

    IF search_params-query IS NOT INITIAL.
      determine_obj_name_range( search_params-query ).
    ENDIF.
  ENDMETHOD.


  METHOD determine_obj_name_range.
    DATA: obj_name_type  TYPE TABLE OF string,
          obj_name_range LIKE object_name_range,
          obj_type_range LIKE object_type_range.

    IF search_params-query_type = zif_abaptags_c_global=>tag_query_types-object_uri.
      zcl_abaptags_adt_util=>map_uri_to_wb_object(
        EXPORTING
          uri         = query
        IMPORTING
          object_name = DATA(object_name)
          tadir_type  = DATA(object_type) ).
      obj_name_range = VALUE #( ( sign = 'I' option = 'EQ' low = object_name ) ).
      obj_type_range = VALUE #( ( sign = 'I' option = 'EQ' low = object_type ) ).
    ELSEIF search_params-query_type = zif_abaptags_c_global=>tag_query_types-object_name_type_combo.
      SPLIT query AT ':' INTO TABLE obj_name_type.
      IF lines( obj_name_type ) = 2.
        obj_name_range = VALUE #( ( sign = 'I' option = 'EQ' low = obj_name_type[ 1 ] ) ).
        obj_type_range = VALUE #( ( sign = 'I' option = 'EQ' low = obj_name_type[ 2 ] ) ).
      ENDIF.
    ELSE.
      DATA(l_query) = to_upper( query ).
      DATA(length) = strlen( query ).
      DATA(last_char_offset) = length - 1.

      DATA(option) = 'EQ'.
      DATA(sign) = 'I'.

      IF l_query+last_char_offset(1) = '<'.
        l_query = l_query(last_char_offset).
      ELSEIF l_query+last_char_offset(1) <> '*'.
        l_query = |{ l_query }*|.
      ENDIF.

      IF l_query CA '+*'.
        option = 'CP'.
      ENDIF.

      obj_name_range = VALUE #( ( sign = sign option = option low = l_query ) ).
    ENDIF.

    IF search_params-query_focus = zif_abaptags_c_global=>tag_query_focus-parent_object.
      parent_object_name_range = obj_name_range.
      parent_object_type_range = obj_type_range.
    ELSE.
      object_name_range = obj_name_range.
      object_type_range = obj_type_range.
    ENDIF.

  ENDMETHOD.


  METHOD get_request_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TGOBJ_SEARCH_PARAMS'
      root_name    = 'SEARCH_PARAMS'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.


  METHOD get_response_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TGOBJ_TREE_RESULT'
      root_name    = 'TREE_RESULT'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.


  METHOD get_matching_tags.
***   SELECT tag_id, COUNT( * ) AS count
***        FROM zabaptags_i_taggedobjaggr
***        WHERE (where)
***          AND tag_id IN @tag_ids
***        GROUP BY tag_id
***        INTO CORRESPONDING FIELDS OF TABLE @result.
***    SELECT tag~tag_id,
***           tag~parent_tag_id,
***      FROM zabaptags_tags AS tag
***        inner join zabaptags
***      INTO TABLE @DATA(matching_tags).

  ENDMETHOD.


  METHOD get_matching_objects.

  ENDMETHOD.

ENDCLASS.
