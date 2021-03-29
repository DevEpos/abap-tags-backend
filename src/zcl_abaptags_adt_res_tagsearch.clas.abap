"! <p class="shorttext synchronized" lang="en">ADT Resource for abap tag search</p>
CLASS zcl_abaptags_adt_res_tagsearch DEFINITION
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
    CONSTANTS:
      BEGIN OF c_tadir_types,
        function       TYPE trobjtype VALUE 'FUNC',
        function_group TYPE trobjtype VALUE 'FUGR',
      END OF c_tadir_types.

    TYPES:
      BEGIN OF ty_tag_info,
        object_name TYPE sobj_name,
        object_type TYPE trobjtype,
        tag_id      TYPE zabaptags_tag_id,
        tag_name    TYPE zabaptags_tag_name,
        owner       TYPE responsibl,
      END OF ty_tag_info,

      BEGIN OF ty_func_module,
        function TYPE tfdir-funcname,
        group    TYPE tfdir-pname,
      END OF ty_func_module,

      BEGIN OF ty_tadir_info,
        object_name  TYPE tadir-obj_name,
        object_type  TYPE tadir-object,
        package_name TYPE tadir-devclass,
        author       TYPE tadir-author,
      END OF ty_tadir_info.

    DATA:
      func_modules             TYPE HASHED TABLE OF ty_func_module WITH UNIQUE KEY function,
      tadir_infos              TYPE HASHED TABLE OF ty_tadir_info WITH UNIQUE KEY object_name object_type,
      tag_infos                TYPE SORTED TABLE OF ty_tag_info WITH NON-UNIQUE KEY object_name object_type,
      tagged_objects           TYPE STANDARD TABLE OF zabaptags_tagged_object,
      owner_range              TYPE RANGE OF sy-uname,
      max_results              TYPE i,
      object_name_range        TYPE RANGE OF sobj_name,
      object_type_range        TYPE RANGE OF trobjtype,
      parent_object_name_range TYPE RANGE OF sobj_name,
      parent_object_type_range TYPE RANGE OF trobjtype,
      tagged_objects_simple    TYPE STANDARD TABLE OF zabaptags_tgobj,
      search_params            TYPE zabaptags_tgobj_search_params.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieve Parameters</p>
      get_parameters
        IMPORTING
          io_request TYPE REF TO if_adt_rest_request
        RAISING
          cx_adt_rest,
      "! <p class="shorttext synchronized" lang="en">Get Content handler for response</p>
      get_response_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      "! <p class="shorttext synchronized" lang="en">Get Content handler for request</p>
      get_request_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      "! <p class="shorttext synchronized" lang="en">Search for tagged objects</p>
      search,
      determine_obj_name_range
        IMPORTING
          query TYPE string
        RAISING
          cx_adt_uri_mapping,
      post_process_results,
      fill_descriptions,
      retrieve_tag_info,
      determine_tadir_info.

ENDCLASS.



CLASS zcl_abaptags_adt_res_tagsearch IMPLEMENTATION.

  METHOD post.
    request->get_body_data(
      EXPORTING content_handler = get_request_content_handler( )
      IMPORTING data            = search_params ).

    IF search_params-tag_id IS INITIAL AND
      search_params-query IS INITIAL.
      RETURN.
    ENDIF.

    get_parameters( request ).

    search( ).
    IF tagged_objects_simple IS INITIAL.
      RETURN.
    ENDIF.
    determine_tadir_info( ).
    retrieve_tag_info( ).
    post_process_results( ).

    fill_descriptions( ).

    DATA(result) = CORRESPONDING zabaptags_tagged_object_t( tagged_objects ).

    response->set_body_data(
      content_handler = get_response_content_handler( )
      data            = result ).
  ENDMETHOD.

  METHOD get_parameters.
    DATA(scope) = COND #(
      WHEN search_params-search_scope IS INITIAL THEN zif_abaptags_c_global=>scopes-all
      ELSE search_params-search_scope ).
    owner_range = SWITCH #(  scope
      WHEN zif_abaptags_c_global=>scopes-global THEN VALUE #( ( sign = 'I' option = 'EQ' low = space ) )
      WHEN zif_abaptags_c_global=>scopes-user   THEN VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ) ).

    max_results = search_params-max_results.
    IF max_results > 0.
      ADD 1 TO max_results.
    ENDIF.

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
        EXPORTING uri         = query
        IMPORTING object_name = DATA(object_name)
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
        l_query = query(last_char_offset).
      ELSEIF l_query+last_char_offset(1) <> '*'.
        l_query = |{ query }*|.
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

  METHOD search.
    DATA: tag_id_range TYPE RANGE OF zabaptags_tag_id.

    tag_id_range = VALUE #( FOR tag_id IN search_params-tag_id ( sign = 'I' option = 'EQ' low = tag_id ) ).
    DATA(tag_count) = lines( tag_id_range ).

    IF search_params-matches_all_tags = abap_true.
      SELECT object_name, object_type
        FROM zabaptags_tgobj
        WHERE tag_id IN @tag_id_range
          AND object_name IN @object_name_range
          AND object_type IN @object_type_range
          AND parent_object_name IN @parent_object_name_range
          AND parent_object_type IN @parent_object_type_range
        GROUP BY object_name, object_type
          HAVING COUNT(*) = @tag_count
        ORDER BY object_type, object_name
        INTO CORRESPONDING FIELDS OF TABLE @tagged_objects_simple
        UP TO @max_results ROWS.
    ELSE.
      SELECT DISTINCT object_name, object_type
        FROM zabaptags_tgobj
        WHERE tag_id IN @tag_id_range
          AND object_name IN @object_name_range
          AND object_type IN @object_type_range
          AND parent_object_name IN @parent_object_name_range
          AND parent_object_type IN @parent_object_type_range
        ORDER BY object_type, object_name
        INTO CORRESPONDING FIELDS OF TABLE @tagged_objects_simple
        UP TO @max_results ROWS.
    ENDIF.

  ENDMETHOD.

  METHOD post_process_results.

    LOOP AT tagged_objects_simple ASSIGNING FIELD-SYMBOL(<tagged_object>).
      DATA(tadir_object_name) = <tagged_object>-object_name.
      DATA(tadir_object_type) = <tagged_object>-object_type.

      IF <tagged_object>-object_type = c_tadir_types-function.
        ASSIGN func_modules[ function = <tagged_object>-object_name ] TO FIELD-SYMBOL(<function>).
        CHECK sy-subrc = 0.
        tadir_object_type = c_tadir_types-function_group.
        tadir_object_name = <function>-group.
      ENDIF.

      ASSIGN tadir_infos[ object_name = tadir_object_name object_type = tadir_object_type ] TO FIELD-SYMBOL(<tadir_info>).
      CHECK sy-subrc = 0.

      DATA(adt_object_ref) = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
        tadir_type       = <tagged_object>-object_type
        name             = <tagged_object>-object_name ).
      CHECK adt_object_ref IS NOT INITIAL.

      APPEND INITIAL LINE TO tagged_objects ASSIGNING FIELD-SYMBOL(<tagged_object_result>).

      <tagged_object_result>-adt_obj_ref = VALUE #(
        name         = <tagged_object>-object_name
        type         = adt_object_ref-type
        tadir_type   = <tagged_object>-object_type
        uri          = adt_object_ref-uri
        package_name = <tadir_info>-package_name
        owner        = <tadir_info>-author ).

      LOOP AT tag_infos ASSIGNING FIELD-SYMBOL(<tag_info>) WHERE object_name = <tagged_object>-object_name
                                                                AND object_type = <tagged_object>-object_type.
        <tagged_object_result>-tags = VALUE #( BASE <tagged_object_result>-tags
          ( tag_id   = <tag_info>-tag_id
            tag_name = <tag_info>-tag_name
            owner    = <tag_info>-owner ) ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD fill_descriptions.
    DATA: texts TYPE STANDARD TABLE OF seu_objtxt.

    texts = VALUE #(
      FOR tagged_obj IN tagged_objects
      ( object = tagged_obj-adt_obj_ref-tadir_type obj_name = tagged_obj-adt_obj_ref-name ) ).
    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab = texts.

    LOOP AT tagged_objects ASSIGNING FIELD-SYMBOL(<entity>).
      ASSIGN texts[ obj_name = <entity>-adt_obj_ref-name object = <entity>-adt_obj_ref-tadir_type ] TO FIELD-SYMBOL(<text>).
      IF sy-subrc = 0.
        <entity>-adt_obj_ref-description = <text>-stext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_request_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TGOBJ_SEARCH_PARAMS'
      root_name    = 'SEARCH_PARAMS'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.

  METHOD get_response_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TAGGED_OBJECTS'
      root_name    = 'TAGGED_OBJECTS'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.



  METHOD retrieve_tag_info.
    DATA: child_tags LIKE tag_infos.

    CHECK search_params-with_tag_info = abap_true.

    SELECT tagged_object~object_name,
           tagged_object~object_type,
           tag~tag_id,
           tag~name AS tag_name,
           tag~owner
      FROM zabaptags_tags AS tag
        INNER JOIN zabaptags_tgobj AS tagged_object
          ON tag~tag_id = tagged_object~tag_id
      FOR ALL ENTRIES IN @tagged_objects_simple
      WHERE tagged_object~object_type = @tagged_objects_simple-object_type
        AND tagged_object~object_name = @tagged_objects_simple-object_name
      INTO CORRESPONDING FIELDS OF TABLE @tag_infos.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF search_params-tag_info_type <> zif_abaptags_c_global=>tag_info_types-children.
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
      FOR ALL ENTRIES IN @tag_infos
      WHERE parent_tag_id = @tag_infos-tag_id
      INTO TABLE @DATA(parent_tag_info).

    IF sy-subrc <> 0.
      CLEAR tag_infos.
    ENDIF.

    LOOP AT tag_infos ASSIGNING FIELD-SYMBOL(<tag_info>).

      LOOP AT parent_tag_info ASSIGNING FIELD-SYMBOL(<child_tag>) WHERE parent_tag_id = <tag_info>-tag_id
                                                                    AND parent_object_name = <tag_info>-object_name
                                                                    AND parent_object_type = <tag_info>-object_type.
        DATA(child_tag) = <tag_info>.
        child_tag-tag_id = <child_tag>-tag_id.
        child_tag-tag_name = <tag_info>-tag_name && ' > ' && <child_tag>-name.
        child_tags = VALUE #( BASE child_tags ( child_tag ) ).
      ENDLOOP.

      DELETE tag_infos.
    ENDLOOP.

    tag_infos = child_tags.
  ENDMETHOD.


  METHOD determine_tadir_info.
    TYPES:
      BEGIN OF ty_tadir,
        obj_name TYPE tadir-obj_name,
        object   TYPE tadir-object,
      END OF ty_tadir.

    DATA: func_module_range TYPE RANGE OF tfdir-funcname,
          tadir_search      TYPE TABLE OF ty_tadir.

    LOOP AT tagged_objects_simple ASSIGNING FIELD-SYMBOL(<tagged_object>).
      IF <tagged_object>-object_type = c_tadir_types-function.
        func_module_range = VALUE #(
          BASE func_module_range ( sign = 'I' option = 'EQ' low = <tagged_object>-object_name ) ).
      ELSE.
        tadir_search = VALUE #(
          BASE tadir_search ( obj_name = <tagged_object>-object_name object = <tagged_object>-object_type ) ).
      ENDIF.
    ENDLOOP.

    IF func_module_range IS NOT INITIAL.
      SELECT funcname AS function,
             pname AS group
        FROM tfdir
        WHERE funcname IN @func_module_range
        INTO CORRESPONDING FIELDS OF TABLE @func_modules.

      LOOP AT func_modules ASSIGNING FIELD-SYMBOL(<func_module>).
        DATA(func_group) = |{ <func_module>-group }|.
        DATA(sapl_offset) = find( val = func_group regex = '(?=/.+/)?SAPL' ).
        DATA(offset_after_sapl) = sapl_offset + 4.

        <func_module>-group = |{ func_group(sapl_offset) }{ func_group+offset_after_sapl }|.

        tadir_search = VALUE #(
          BASE tadir_search ( object = c_tadir_types-function_group obj_name = <func_module>-group ) ).
      ENDLOOP.
    ENDIF.

    SELECT DISTINCT
           obj_name AS object_name,
           object AS object_type,
           devclass AS package_name,
           author
      FROM tadir
      FOR ALL ENTRIES IN @tadir_search
      WHERE obj_name = @tadir_search-obj_name
        AND object   = @tadir_search-object
      INTO CORRESPONDING FIELDS OF TABLE @tadir_infos.
  ENDMETHOD.

ENDCLASS.
