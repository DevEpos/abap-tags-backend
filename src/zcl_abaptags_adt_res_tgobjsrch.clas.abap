"! <p class="shorttext synchronized">Resource for tagged object search</p>
CLASS zcl_abaptags_adt_res_tgobjsrch DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS c_comp_where_filter TYPE string VALUE `component_name = @space AND component_type = @space`.

    DATA tags_dac TYPE REF TO zcl_abaptags_tags_dac.
    DATA tadir_info_reader TYPE REF TO zcl_abaptags_tadir.
    DATA tgobj_infos TYPE zif_abaptags_ty_global=>ty_tgobj_infos.
    DATA tagged_objects TYPE STANDARD TABLE OF zabaptags_tagged_object.
    DATA owner_range TYPE RANGE OF sy-uname.
    DATA max_results TYPE i.
    DATA object_name_range TYPE RANGE OF sobj_name.
    DATA object_type_range TYPE RANGE OF trobjtype.
    DATA parent_object_name_range TYPE RANGE OF sobj_name.
    DATA parent_object_type_range TYPE RANGE OF trobjtype.
    DATA tagged_objects_db TYPE zif_abaptags_ty_global=>ty_db_tagged_objects.
    DATA search_params TYPE zabaptags_tgobj_search_params.
    DATA comp_tgobj_where_filter TYPE string.
    DATA obj_refs_for_name_mapping TYPE TABLE OF REF TO zabaptags_adt_obj_ref.
    DATA cds_name_mapper TYPE REF TO zcl_abaptags_cds_name_mapper.
    DATA tag_id_range TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    "! <p class="shorttext synchronized">Retrieve Parameters</p>
    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Get Content handler for response</p>
    METHODS get_response_content_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Get Content handler for request</p>
    METHODS get_request_content_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Search for tagged objects</p>
    METHODS search.

    METHODS determine_obj_name_range
      IMPORTING
        !query TYPE string
      RAISING
        cx_adt_rest.

    METHODS post_process_results.
    METHODS fill_descriptions.
    METHODS fill_ddl_display_names.
    METHODS retrieve_tag_info.
    METHODS determine_tadir_info.
    METHODS get_tagged_object_info.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tgobjsrch IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    tags_dac = zcl_abaptags_tags_dac=>get_instance( ).
    cds_name_mapper = NEW #( ).
  ENDMETHOD.

  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = search_params ).

    IF     search_params-tag_id IS INITIAL
       AND search_params-query  IS INITIAL.
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
    fill_ddl_display_names( ).

    DATA(result) = CORRESPONDING zabaptags_tagged_object_t( tagged_objects ).

    response->set_body_data( content_handler = get_response_content_handler( )
                             data            = result ).
  ENDMETHOD.

  METHOD determine_obj_name_range.
    DATA obj_name_type TYPE TABLE OF string.
    DATA obj_name_range LIKE object_name_range.
    DATA obj_type_range LIKE object_type_range.

    IF search_params-query_type = zif_abaptags_c_global=>tag_query_types-object_uri.
      zcl_abaptags_adt_util=>map_uri_to_wb_object( EXPORTING uri         = query
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
    DATA texts TYPE STANDARD TABLE OF seu_objtxt.

    texts = VALUE #( FOR tagged_obj IN tagged_objects
                     ( object = tagged_obj-adt_obj_ref-tadir_type obj_name = tagged_obj-adt_obj_ref-name ) ).
    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES obj_tab = texts.

    LOOP AT tagged_objects ASSIGNING FIELD-SYMBOL(<entity>).
      ASSIGN texts[ obj_name = <entity>-adt_obj_ref-name object = <entity>-adt_obj_ref-tadir_type ] TO FIELD-SYMBOL(<text>).
      IF sy-subrc = 0.
        <entity>-adt_obj_ref-description = <text>-stext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_parameters.
    " TODO: parameter IO_REQUEST is never used (ABAP cleaner)

    DATA(scope) = COND #(
      WHEN search_params-search_scope IS INITIAL
      THEN zif_abaptags_c_global=>scopes-all
      ELSE search_params-search_scope ).
    owner_range = SWITCH #( scope
                            WHEN zif_abaptags_c_global=>scopes-global THEN
                              VALUE #( ( sign = 'I' option = 'EQ' low = space ) )
                            WHEN zif_abaptags_c_global=>scopes-user THEN
                              VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ) ).

    max_results = search_params-max_results.
    IF max_results > 0.
      max_results = max_results + 1.
    ENDIF.

    IF search_params-query IS NOT INITIAL.
      determine_obj_name_range( search_params-query ).
    ENDIF.

    " Check if rows with filled component should be excluded from the search result
    IF search_params-exclude_components = abap_true.
      comp_tgobj_where_filter = c_comp_where_filter.
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
    DATA added_tgobj TYPE REF TO zabaptags_tagged_object.
    DATA adt_object_ref TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.

    " map components to ADT uri
    DATA(cmp_adt_mapper) = NEW zcl_abaptags_comp_adt_mapper( ).
    cmp_adt_mapper->add_components( VALUE #( FOR <tgobj> IN tagged_objects_db WHERE ( component_name IS NOT INITIAL )
                                             ( component_name = <tgobj>-component_name
                                               component_type = <tgobj>-component_type
                                               object_name    = <tgobj>-object_name
                                               object_type    = <tgobj>-object_type ) ) ).

    cmp_adt_mapper->determine_components( ).

    LOOP AT tagged_objects_db ASSIGNING FIELD-SYMBOL(<tagged_object>).
      TRY.
          DATA(tadir_info) = tadir_info_reader->get_tadir_info( name = <tagged_object>-object_name
                                                                type = <tagged_object>-object_type ).
        CATCH cx_sy_itab_line_not_found.
          " TODO: handle some edge cases, like $-packages
          CONTINUE.
      ENDTRY.

      DATA(object_name) = ``.
      DATA(parent_object_name) = ``.
      DATA(adt_type) = ``.

      IF <tagged_object>-component_name IS NOT INITIAL.
        adt_object_ref = cmp_adt_mapper->get_adt_object( VALUE #( object_name    = <tagged_object>-object_name
                                                                  object_type    = <tagged_object>-object_type
                                                                  component_name = <tagged_object>-component_name
                                                                  component_type = <tagged_object>-component_type ) ).
        adt_type = <tagged_object>-component_type.
        object_name = <tagged_object>-component_name.
        parent_object_name = <tagged_object>-object_name.
      ELSE.
        adt_object_ref = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
                             tadir_type = <tagged_object>-object_type
                             name       = <tagged_object>-object_name ).
        adt_type = adt_object_ref-type.
        object_name = <tagged_object>-object_name.
      ENDIF.

      CHECK adt_object_ref IS NOT INITIAL.

      DATA(tagged_object) = VALUE zabaptags_tagged_object( adt_obj_ref = VALUE #(
                                                               name         = object_name
                                                               parent_name  = parent_object_name
                                                               type         = adt_type
                                                               tadir_type   = <tagged_object>-object_type
                                                               uri          = adt_object_ref-uri
                                                               package_name = tadir_info-package_name
                                                               owner        = tadir_info-author ) ).

      LOOP AT tgobj_infos ASSIGNING FIELD-SYMBOL(<tag_info>) WHERE     object_name = <tagged_object>-object_name
                                                                   AND object_type = <tagged_object>-object_type.
        tagged_object-tags = VALUE #( BASE tagged_object-tags
                                      ( tag_id   = <tag_info>-tag_id
                                        tag_name = <tag_info>-tag_name
                                        owner    = <tag_info>-tag_owner ) ).

        IF search_params-result_group_level = zif_abaptags_c_global=>search_result_group_level-by_tag_and_object.
          APPEND tagged_object TO tagged_objects REFERENCE INTO added_tgobj.
          IF cds_name_mapper->collect_entry( name = added_tgobj->adt_obj_ref-name
                                             type = added_tgobj->adt_obj_ref-tadir_type ).
            obj_refs_for_name_mapping = VALUE #( BASE obj_refs_for_name_mapping
                                                 ( REF #( added_tgobj->adt_obj_ref ) ) ).
          ENDIF.
          CLEAR tagged_object-tags.
        ENDIF.

      ENDLOOP.

      IF    search_params-result_group_level IS INITIAL
         OR search_params-result_group_level  = zif_abaptags_c_global=>search_result_group_level-by_object.
        APPEND tagged_object TO tagged_objects REFERENCE INTO added_tgobj.
        IF cds_name_mapper->collect_entry( name = added_tgobj->adt_obj_ref-name
                                           type = added_tgobj->adt_obj_ref-tadir_type ).
          obj_refs_for_name_mapping = VALUE #( BASE obj_refs_for_name_mapping
                                               ( REF #( added_tgobj->adt_obj_ref ) ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD retrieve_tag_info.
    DATA tgobj_infos_mesh TYPE zif_abaptags_ty_global=>ty_tgobj_info_mesh.

    CHECK search_params-with_tag_info = abap_true.

    get_tagged_object_info( ).

    IF    tgobj_infos                 IS INITIAL
       OR search_params-tag_info_type <> zif_abaptags_c_global=>tag_info_types-children.
      RETURN.
    ENDIF.

    tgobj_infos_mesh-child_objects = tags_dac->get_children_of_tagged_objects(
                                         VALUE #( FOR t IN tgobj_infos
                                                  ( sign = 'I' option = 'EQ' low = t-tag_id ) ) ).

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
      child_tgobj_info-tag_id   = <tgobj_child>-tag_id.
      child_tgobj_info-tag_name = |{ child_tgobj_info-tag_name } > { <tgobj_child>-tag_name }|.
      tgobj_infos = VALUE #( BASE tgobj_infos ( child_tgobj_info ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD search.
    tag_id_range = VALUE zif_abaptags_ty_global=>ty_tag_id_range( FOR tag_id IN search_params-tag_id
                                                                  ( sign = 'I' option = 'EQ' low = tag_id ) ).

    DATA(tag_count) = lines( tag_id_range ).

    IF     search_params-matches_all_tags = abap_true
       AND tag_count > 0.
      SELECT object_name, object_type, component_name, component_type
        FROM zabaptags_tgobjn
        WHERE tag_id IN @tag_id_range
          AND object_name IN @object_name_range
          AND object_type IN @object_type_range
          AND parent_object_name IN @parent_object_name_range
          AND parent_object_type IN @parent_object_type_range
          AND (comp_tgobj_where_filter)
        GROUP BY object_name, object_type, component_name, component_type
        HAVING COUNT(*) = @tag_count
        ORDER BY object_type,
                 object_name
        INTO CORRESPONDING FIELDS OF TABLE @tagged_objects_db
        UP TO @max_results ROWS.
    ELSE.
      SELECT DISTINCT object_name, object_type, component_name, component_type
        FROM zabaptags_tgobjn
        WHERE tag_id IN @tag_id_range
          AND object_name IN @object_name_range
          AND object_type IN @object_type_range
          AND parent_object_name IN @parent_object_name_range
          AND parent_object_type IN @parent_object_type_range
          AND (comp_tgobj_where_filter)
        ORDER BY object_type,
                 object_name
        INTO CORRESPONDING FIELDS OF TABLE @tagged_objects_db
        UP TO @max_results ROWS.
    ENDIF.
  ENDMETHOD.

  METHOD get_tagged_object_info.
    SELECT tagged_object~object_name,
           tagged_object~object_type,
           tag~tag_id,
           tag~name                  AS tag_name,
           tag~owner                 AS tag_owner
      FROM zabaptags_tags AS tag
           INNER JOIN zabaptags_tgobjn AS tagged_object
             ON tag~tag_id = tagged_object~tag_id
      FOR ALL ENTRIES IN @tagged_objects_db
      WHERE tagged_object~object_type = @tagged_objects_db-object_type
        AND tagged_object~object_name = @tagged_objects_db-object_name
        AND tagged_object~tag_id IN @tag_id_range
        AND (comp_tgobj_where_filter)
      INTO CORRESPONDING FIELDS OF TABLE @tgobj_infos.
  ENDMETHOD.

  METHOD fill_ddl_display_names.
    CHECK cds_name_mapper->map_entries( ).

    LOOP AT obj_refs_for_name_mapping INTO DATA(obj_ref).
      obj_ref->alt_name = cds_name_mapper->get_display_name( name = obj_ref->name
                                                             type = obj_ref->tadir_type ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
