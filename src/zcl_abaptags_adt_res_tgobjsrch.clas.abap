"! <p class="shorttext synchronized" lang="en">Resource for tagged object search</p>
CLASS zcl_abaptags_adt_res_tgobjsrch DEFINITION
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
    DATA:
      tags_dac                 TYPE REF TO zcl_abaptags_tags_dac,
      tadir_info_reader        TYPE REF TO zcl_abaptags_tadir,
      tgobj_infos              TYPE zif_abaptags_ty_global=>ty_tgobj_infos,
      tagged_objects           TYPE STANDARD TABLE OF zabaptags_tagged_object,
      owner_range              TYPE RANGE OF sy-uname,
      max_results              TYPE i,
      object_name_range        TYPE RANGE OF sobj_name,
      object_type_range        TYPE RANGE OF trobjtype,
      parent_object_name_range TYPE RANGE OF sobj_name,
      parent_object_type_range TYPE RANGE OF trobjtype,
      tagged_objects_db        TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      search_params            TYPE zabaptags_tgobj_search_params,
      tag_id_range             TYPE zif_abaptags_ty_global=>ty_tag_id_range.

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
          cx_adt_rest,
      post_process_results,
      fill_descriptions,
      retrieve_tag_info,
      determine_tadir_info.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobjsrch IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    tags_dac = zcl_abaptags_tags_dac=>get_instance( ).
  ENDMETHOD.


  METHOD post.
    request->get_body_data(
      EXPORTING
        content_handler = get_request_content_handler( )
      IMPORTING
        data            = search_params ).

    IF search_params-tag_id IS INITIAL AND
      search_params-query IS INITIAL.
      RETURN.
    ENDIF.

    get_parameters( request ).

    search( ).
    IF tagged_objects_db IS INITIAL.
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


  METHOD determine_tadir_info.
    tadir_info_reader = NEW zcl_abaptags_tadir(
        keys = VALUE #( FOR tgobj IN tagged_objects_db ( name = tgobj-object_name type = tgobj-object_type ) )
      )->determine_tadir_entries( ).
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


  METHOD post_process_results.

    LOOP AT tagged_objects_db ASSIGNING FIELD-SYMBOL(<tagged_object>).
      DATA(tadir_object_name) = <tagged_object>-object_name.
      DATA(tadir_object_type) = <tagged_object>-object_type.

      TRY.
          DATA(tadir_info) = tadir_info_reader->get_tadir_info(
            name = <tagged_object>-object_name
            type = <tagged_object>-object_type ).
        CATCH cx_sy_itab_line_not_found.
          " TODO: handle some edge cases, like $-packages
          CONTINUE.
      ENDTRY.

      DATA(adt_object_ref) = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
        tadir_type = <tagged_object>-object_type
        name       = <tagged_object>-object_name ).
      CHECK adt_object_ref IS NOT INITIAL.

      DATA(tagged_object) = VALUE zabaptags_tagged_object(
        adt_obj_ref = VALUE #(
          name         = <tagged_object>-object_name
          type         = adt_object_ref-type
          tadir_type   = <tagged_object>-object_type
          uri          = adt_object_ref-uri
          package_name = tadir_info-package_name
          owner        = tadir_info-author ) ).

      LOOP AT tgobj_infos ASSIGNING FIELD-SYMBOL(<tag_info>) WHERE object_name = <tagged_object>-object_name
                                                               AND object_type = <tagged_object>-object_type.
        tagged_object-tags = VALUE #( BASE tagged_object-tags
          ( tag_id   = <tag_info>-tag_id
            tag_name = <tag_info>-tag_name
            owner    = <tag_info>-tag_owner ) ).

        IF search_params-result_group_level = zif_abaptags_c_global=>search_result_group_level-by_tag_and_object.
          APPEND tagged_object TO tagged_objects.
          CLEAR tagged_object-tags.
        ENDIF.

      ENDLOOP.

      IF search_params-result_group_level IS INITIAL OR
          search_params-result_group_level = zif_abaptags_c_global=>search_result_group_level-by_object.
        tagged_objects = VALUE #( BASE tagged_objects ( tagged_object ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD retrieve_tag_info.
    DATA: tgobj_infos_mesh TYPE zif_abaptags_ty_global=>ty_tgobj_info_mesh.

    CHECK search_params-with_tag_info = abap_true.

    tgobj_infos = tags_dac->get_tagged_obj_info(
      tag_id_range   = COND #(
        WHEN search_params-tag_info_type = zif_abaptags_c_global=>tag_info_types-search_focus THEN tag_id_range )
      tagged_objects = tagged_objects_db ).

    IF tgobj_infos IS INITIAL OR
        search_params-tag_info_type <> zif_abaptags_c_global=>tag_info_types-children.
      RETURN.
    ENDIF.

    tgobj_infos_mesh-child_objects = tags_dac->get_children_of_tagged_objects( VALUE #(
      FOR t IN tgobj_infos ( sign = 'I' option = 'EQ' low = t-tag_id ) ) ).

    IF tgobj_infos_mesh-child_objects IS INITIAL.
      CLEAR tgobj_infos.
      RETURN.
    ENDIF.

    tgobj_infos_mesh-objects = tgobj_infos.
    CLEAR tgobj_infos.

    LOOP AT tgobj_infos_mesh-child_objects ASSIGNING FIELD-SYMBOL(<tgobj_child>).
      TRY.
          DATA(child_tgobj_info) = tgobj_infos_mesh-child_objects\parent[ <tgobj_child> ].
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
      child_tgobj_info-tag_id = <tgobj_child>-tag_id.
      child_tgobj_info-tag_name = child_tgobj_info-tag_name && ` > ` && <tgobj_child>-tag_name.
      tgobj_infos = VALUE #( BASE tgobj_infos ( child_tgobj_info ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD search.
    tag_id_range = VALUE zif_abaptags_ty_global=>ty_tag_id_range(
      FOR tag_id IN search_params-tag_id ( sign = 'I' option = 'EQ' low = tag_id ) ).

    tagged_objects_db = tags_dac->find_tagged_objects(
      only_matching_all_tag    = search_params-matches_all_tags
      tag_count                = lines( tag_id_range )
      max_results              = max_results
      tag_id_range             = tag_id_range
      object_name_range        = object_name_range
      object_type_range        = object_type_range
      parent_object_name_range = parent_object_name_range
      parent_object_type_range = parent_object_type_range ).
  ENDMETHOD.

ENDCLASS.
