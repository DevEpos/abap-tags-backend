"! <p class="shorttext synchronized" lang="en">ADT Resource for abap tag search</p>
CLASS zcl_abaptags_adt_res_tagsearch DEFINITION
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
      BEGIN OF c_tadir_types,
        function       TYPE trobjtype VALUE 'FUNC',
        function_group TYPE trobjtype VALUE 'FUGR',
      END OF c_tadir_types.
    TYPES:
      BEGIN OF ty_s_tag_info,
        object_name TYPE sobj_name,
        object_type TYPE trobjtype,
        tag_id      TYPE zabaptags_tag_id,
        tag_name    TYPE zabaptags_tag_name,
        owner       TYPE responsibl,
      END OF ty_s_tag_info.

    TYPES:
      BEGIN OF ty_s_func_modules,
        function TYPE tfdir-funcname,
        group    TYPE tfdir-pname,
      END OF ty_s_func_modules.

    TYPES:
      BEGIN OF ty_s_tadir_info,
        object_name  TYPE tadir-obj_name,
        object_type  TYPE tadir-object,
        package_name TYPE tadir-devclass,
        author       TYPE tadir-author,
      END OF ty_s_tadir_info.

    DATA mt_func_modules TYPE HASHED TABLE OF ty_s_func_modules WITH UNIQUE KEY function.
    DATA mt_tadir_info TYPE HASHED TABLE OF ty_s_tadir_info WITH UNIQUE KEY object_name object_type.
    DATA mt_tag_info TYPE SORTED TABLE OF ty_s_tag_info WITH NON-UNIQUE KEY object_name object_type.
    DATA mt_tagged_objects_int TYPE STANDARD TABLE OF zabaptags_tagged_object.
    DATA mt_owner_range TYPE RANGE OF sy-uname.
    DATA mt_tags_for_search TYPE zabaptags_tag_data_t.
    DATA mv_query TYPE string.
    DATA mv_max_results TYPE i.
    DATA mt_object_name_range TYPE RANGE OF sobj_name.
    DATA mt_object_type_range TYPE RANGE OF trobjtype.
    DATA mt_parent_object_name_range TYPE RANGE OF sobj_name.
    DATA mt_parent_object_type_range TYPE RANGE OF trobjtype.
    DATA mt_tagged_objects_simple TYPE STANDARD TABLE OF zabaptags_tgobj.
    DATA ms_search_params TYPE zabaptags_tgobj_search_params.

    "! <p class="shorttext synchronized" lang="en">Retrieve Parameters</p>
    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.
    "! <p class="shorttext synchronized" lang="en">Get Content handler for response</p>
    METHODS get_response_content_handler
      RETURNING
        VALUE(ro_content_handler) TYPE REF TO if_adt_rest_content_handler.
    "! <p class="shorttext synchronized" lang="en">Get Content handler for request</p>
    METHODS get_request_content_handler
      RETURNING
        VALUE(ro_content_handler) TYPE REF TO if_adt_rest_content_handler.
    "! <p class="shorttext synchronized" lang="en">Search for tagged objects</p>
    METHODS search
      RETURNING
        VALUE(rf_results) TYPE abap_bool.
    METHODS determine_obj_name_range
      IMPORTING
        iv_query TYPE string
      RAISING
        cx_adt_uri_mapping.

    METHODS post_process_results.
    METHODS fill_descriptions.
    METHODS retrieve_tag_info.
    METHODS determine_tadir_info.

ENDCLASS.



CLASS zcl_abaptags_adt_res_tagsearch IMPLEMENTATION.

  METHOD post.
    request->get_body_data(
      EXPORTING content_handler = get_request_content_handler( )
      IMPORTING data            = ms_search_params
    ).

    IF ms_search_params-tag_id IS INITIAL AND
      ms_search_params-query IS INITIAL.
      RETURN.
    ENDIF.

    get_parameters( request ).

    CHECK search( ).
    determine_tadir_info( ).
    retrieve_tag_info( ).
    post_process_results( ).

    fill_descriptions( ).

    DATA(lt_result) = CORRESPONDING zabaptags_tagged_object_t( mt_tagged_objects_int ).

    response->set_body_data(
        content_handler = get_response_content_handler( )
        data            = lt_result
    ).
  ENDMETHOD.

  METHOD get_parameters.
    DATA(lv_scope) = COND #(
      WHEN ms_search_params-search_scope IS INITIAL THEN zif_abaptags_c_global=>c_scopes-all ELSE ms_search_params-search_scope
    ).
    mt_owner_range = SWITCH #(  lv_scope
      WHEN zif_abaptags_c_global=>c_scopes-global THEN VALUE #( ( sign = 'I' option = 'EQ' low = space ) )
      WHEN zif_abaptags_c_global=>c_scopes-user   THEN VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) )
    ).

    mv_max_results = ms_search_params-max_results.
    IF mv_max_results > 0.
      ADD 1 TO mv_max_results.
    ENDIF.

    IF ms_search_params-query IS NOT INITIAL.
      determine_obj_name_range( ms_search_params-query ).
    ENDIF.
  ENDMETHOD.

  METHOD determine_obj_name_range.
    DATA: lt_obj_name_type  TYPE TABLE OF string,
          lt_obj_name_range LIKE mt_object_name_range,
          lt_obj_type_range LIKE mt_object_type_range.

    DATA(lv_query) = iv_query.

    IF ms_search_params-query_type = zif_abaptags_c_global=>c_tag_query_types-object_uri.
      zcl_abaptags_adt_util=>map_uri_to_wb_object(
        EXPORTING iv_uri         = lv_query
        IMPORTING ev_object_name = DATA(lv_object_name)
                  ev_tadir_type  = DATA(lv_object_type)
      ).
      lt_obj_name_range = VALUE #( ( sign = 'I' option = 'EQ' low = lv_object_name ) ).
      lt_obj_type_range = VALUE #( ( sign = 'I' option = 'EQ' low = lv_object_type ) ).
    ELSEIF ms_search_params-query_type = zif_abaptags_c_global=>c_tag_query_types-object_name_type_combo.
      SPLIT lv_query AT ':' INTO TABLE lt_obj_name_type.
      IF lines( lt_obj_name_type ) = 2.
        lt_obj_name_range = VALUE #( ( sign = 'I' option = 'EQ' low = lt_obj_name_type[ 1 ] ) ).
        lt_obj_type_range = VALUE #( ( sign = 'I' option = 'EQ' low = lt_obj_name_type[ 2 ] ) ).
      ENDIF.
    ELSE.
      DATA(lv_length) = strlen( lv_query ).
      DATA(lv_last_char_offset) = lv_length - 1.

      DATA(lv_option) = 'EQ'.
      DATA(lv_sign) = 'I'.

      IF iv_query+lv_last_char_offset(1) = '<'.
        lv_query = lv_query(lv_last_char_offset).
      ELSEIF lv_query+lv_last_char_offset(1) <> '*'.
        lv_query = |{ lv_query }*|.
      ENDIF.

      IF lv_query CA '+*'.
        lv_option = 'CP'.
      ENDIF.

      lt_obj_name_range = VALUE #( ( sign = lv_sign option = lv_option low = to_upper( lv_query ) ) ).
    ENDIF.

    IF ms_search_params-query_focus = zif_abaptags_c_global=>c_tag_query_focus-parent_object.
      mt_parent_object_name_range = lt_obj_name_range.
      mt_parent_object_type_range = lt_obj_type_range.
    ELSE.
      mt_object_name_range = lt_obj_name_range.
      mt_object_type_range = lt_obj_type_range.
    ENDIF.

  ENDMETHOD.

  METHOD search.
    DATA: lt_tag_id_range TYPE RANGE OF zabaptags_tag_id.

    lt_tag_id_range = VALUE #( FOR tag_id IN ms_search_params-tag_id ( sign = 'I' option = 'EQ' low = tag_id ) ).
    DATA(lv_tag_count) = lines( lt_tag_id_range ).

    IF ms_search_params-matches_all_tags = abap_true.
      SELECT object_name, object_type
        FROM zabaptags_tgobj
        WHERE tag_id IN @lt_tag_id_range
          AND object_name IN @mt_object_name_range
          AND object_type IN @mt_object_type_range
          AND parent_object_name IN @mt_parent_object_name_range
          AND parent_object_type IN @mt_parent_object_type_range
        GROUP BY object_name, object_type
          HAVING COUNT(*) = @lv_tag_count
        ORDER BY object_type, object_name
      INTO CORRESPONDING FIELDS OF TABLE @mt_tagged_objects_simple
        UP TO @mv_max_results ROWS.
    ELSE.
      SELECT DISTINCT object_name, object_type
        FROM zabaptags_tgobj
        WHERE tag_id IN @lt_tag_id_range
          AND object_name IN @mt_object_name_range
          AND object_type IN @mt_object_type_range
          AND parent_object_name IN @mt_parent_object_name_range
          AND parent_object_type IN @mt_parent_object_type_range
        ORDER BY object_type, object_name
      INTO CORRESPONDING FIELDS OF TABLE @mt_tagged_objects_simple
        UP TO @mv_max_results ROWS.
    ENDIF.

    rf_results = xsdbool( mt_tagged_objects_simple IS NOT INITIAL ).
  ENDMETHOD.

  METHOD post_process_results.

    LOOP AT mt_tagged_objects_simple ASSIGNING FIELD-SYMBOL(<ls_tagged_object>).
      DATA(lv_tadir_object_name) = <ls_tagged_object>-object_name.
      DATA(lv_tadir_object_type) = <ls_tagged_object>-object_type.

      IF <ls_tagged_object>-object_type = c_tadir_types-function.
        ASSIGN mt_func_modules[ function = <ls_tagged_object>-object_name ] TO FIELD-SYMBOL(<ls_function>).
        CHECK sy-subrc = 0.
        lv_tadir_object_type = c_tadir_types-function_group.
        lv_tadir_object_name = <ls_function>-group.
      ENDIF.

      ASSIGN mt_tadir_info[ object_name = lv_tadir_object_name object_type = lv_tadir_object_type ] TO FIELD-SYMBOL(<ls_tadir_info>).
      CHECK sy-subrc = 0.

      DATA(ls_adt_object_ref) = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
          iv_tadir_type       = <ls_tagged_object>-object_type
          iv_name             = <ls_tagged_object>-object_name
      ).
      CHECK ls_adt_object_ref IS NOT INITIAL.

      APPEND INITIAL LINE TO mt_tagged_objects_int ASSIGNING FIELD-SYMBOL(<ls_tagged_object_result>).

      <ls_tagged_object_result>-adt_obj_ref = VALUE #(
         name         = <ls_tagged_object>-object_name
         type         = ls_adt_object_ref-type
         tadir_type   = <ls_tagged_object>-object_type
         uri          = ls_adt_object_ref-uri
         package_name = <ls_tadir_info>-package_name
         owner        = <ls_tadir_info>-author
      ).

      LOOP AT mt_tag_info ASSIGNING FIELD-SYMBOL(<ls_tag_info>) WHERE object_name = <ls_tagged_object>-object_name
                                                                  AND object_type = <ls_tagged_object>-object_type.
        <ls_tagged_object_result>-tags = VALUE #( BASE <ls_tagged_object_result>-tags
          ( tag_id   = <ls_tag_info>-tag_id
            tag_name = <ls_tag_info>-tag_name
            owner    = <ls_tag_info>-owner )
        ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD fill_descriptions.
    DATA: lt_class_result TYPE zsat_entity_t,
          lt_texts        TYPE STANDARD TABLE OF seu_objtxt.

    lt_texts = VALUE #(
      FOR tagged_obj IN mt_tagged_objects_int
      ( object = tagged_obj-adt_obj_ref-tadir_type obj_name = tagged_obj-adt_obj_ref-name )
    ).
    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab      = lt_texts.

    LOOP AT mt_tagged_objects_int ASSIGNING FIELD-SYMBOL(<ls_entity>).
      ASSIGN lt_texts[ obj_name = <ls_entity>-adt_obj_ref-name object = <ls_entity>-adt_obj_ref-tadir_type ] TO FIELD-SYMBOL(<ls_text>).
      IF sy-subrc = 0.
        <ls_entity>-adt_obj_ref-description = <ls_text>-stext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_request_content_handler.
    ro_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TGOBJ_SEARCH_PARAMS'
      root_name    = 'SEARCH_PARAMS'
      content_type = if_rest_media_type=>gc_appl_xml
    ).
  ENDMETHOD.

  METHOD get_response_content_handler.
    ro_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TAGGED_OBJECTS'
      root_name    = 'TAGGED_OBJECTS'
      content_type = if_rest_media_type=>gc_appl_xml
    ).
  ENDMETHOD.



  METHOD retrieve_tag_info.
    DATA: lt_child_tags LIKE mt_tag_info.

    CHECK ms_search_params-with_tag_info = abap_true.

    SELECT tagged_object~object_name,
           tagged_object~object_type,
           tag~tag_id,
           tag~name AS tag_name,
           tag~owner
      FROM zabaptags_tags AS tag
        INNER JOIN zabaptags_tgobj AS tagged_object
          ON tag~tag_id = tagged_object~tag_id
      FOR ALL ENTRIES IN @mt_tagged_objects_simple
      WHERE tagged_object~object_type = @mt_tagged_objects_simple-object_type
        AND tagged_object~object_name = @mt_tagged_objects_simple-object_name
    INTO CORRESPONDING FIELDS OF TABLE @mt_tag_info.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ms_search_params-tag_info_type <> zif_abaptags_c_global=>c_tag_info_types-children.
      RETURN.
    ENDIF.

    " Determine parent/child information about tags of found objects
    SELECT DISTINCT
           tag~tag_id,
           tag~name,
           tag~parent_tag_id,
           tgobj~parent_object_name,
           tgobj~parent_object_type
      FROM zabaptags_tags AS tag
        INNER JOIN zabaptags_tgobj AS tgobj
          ON tag~tag_id = tgobj~tag_id
      FOR ALL ENTRIES IN @mt_tag_info
      WHERE parent_tag_id = @mt_tag_info-tag_id
    INTO TABLE @DATA(lt_parent_tag_info).

    IF sy-subrc <> 0.
      CLEAR mt_tag_info.
    ENDIF.

    LOOP AT mt_tag_info ASSIGNING FIELD-SYMBOL(<ls_tag_info>).

      LOOP AT lt_parent_tag_info ASSIGNING FIELD-SYMBOL(<ls_child_tag>) WHERE parent_tag_id = <ls_tag_info>-tag_id
                                                                          AND parent_object_name = <ls_tag_info>-object_name
                                                                          AND parent_object_type = <ls_tag_info>-object_type.
        DATA(ls_child_tag) = <ls_tag_info>.
        ls_child_tag-tag_id = <ls_child_tag>-tag_id.
        ls_child_tag-tag_name = <ls_tag_info>-tag_name && ' > ' && <ls_child_tag>-name.
        lt_child_tags = VALUE #( BASE lt_child_tags ( ls_child_tag ) ).
      ENDLOOP.

      DELETE mt_tag_info.
    ENDLOOP.

    mt_tag_info = lt_child_tags.
  ENDMETHOD.


  METHOD determine_tadir_info.
    TYPES:
      BEGIN OF lty_s_tadir,
        obj_name TYPE tadir-obj_name,
        object   TYPE tadir-object,
      END OF lty_s_tadir.

    DATA: lt_func_module_range TYPE RANGE OF tfdir-funcname,
          lt_tadir_search      TYPE TABLE OF lty_s_tadir.

    LOOP AT mt_tagged_objects_simple ASSIGNING FIELD-SYMBOL(<ls_tagged_object>).
      IF <ls_tagged_object>-object_type = c_tadir_types-function.
        lt_func_module_range = VALUE #( BASE lt_func_module_range ( sign = 'I' option = 'EQ' low = <ls_tagged_object>-object_name ) ).
      ELSE.
        lt_tadir_search = VALUE #( BASE lt_tadir_search ( obj_name = <ls_tagged_object>-object_name object = <ls_tagged_object>-object_type ) ).
      ENDIF.
    ENDLOOP.

    IF lt_func_module_range IS NOT INITIAL.
      SELECT funcname AS function,
             pname AS group
        FROM tfdir
        WHERE funcname IN @lt_func_module_range
      INTO CORRESPONDING FIELDS OF TABLE @mt_func_modules.

      LOOP AT mt_func_modules ASSIGNING FIELD-SYMBOL(<ls_func_module>).
        DATA(lv_func_group) = |{ <ls_func_module>-group }|.
        DATA(lv_sapl_offset) = find( val = lv_func_group regex = '(?=/.+/)?SAPL' ).
        DATA(lv_offset_after_sapl) = lv_sapl_offset + 4.

        <ls_func_module>-group = |{ lv_func_group(lv_sapl_offset) }{ lv_func_group+lv_offset_after_sapl }|.

        lt_tadir_search = VALUE #( BASE lt_tadir_search ( object = c_tadir_types-function_group obj_name = <ls_func_module>-group ) ).
      ENDLOOP.
    ENDIF.

    SELECT DISTINCT
           obj_name AS object_name,
           object AS object_type,
           devclass AS package_name,
           author
      FROM tadir
      FOR ALL ENTRIES IN @lt_tadir_search
      WHERE obj_name = @lt_tadir_search-obj_name
        AND object   = @lt_tadir_search-object
    INTO CORRESPONDING FIELDS OF TABLE @mt_tadir_info.
  ENDMETHOD.

ENDCLASS.
