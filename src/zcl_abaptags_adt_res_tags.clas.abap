"! <p class="shorttext synchronized" lang="en">ADT Resource for ABAP Tags</p>
CLASS zcl_abaptags_adt_res_tags DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,
      post
        REDEFINITION,
      get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_actions,
        lock         TYPE string VALUE 'lock' ##NO_TEXT,
        unlock       TYPE string VALUE 'unlock' ##NO_TEXT,
        batch_delete TYPE string VALUE 'batchDelete' ##NO_TEXT,
        make_global  TYPE string VALUE 'makeGlobal' ##NO_TEXT,
      END OF c_actions,

      BEGIN OF c_params,
        query             TYPE string VALUE 'query' ##NO_TEXT,
        scope             TYPE string VALUE 'scope' ##NO_TEXT,
        action            TYPE string VALUE 'action' ##NO_TEXT,
        no_hierarchy      TYPE string VALUE 'noHierarchy' ##NO_TEXT,
        with_object_count TYPE string VALUE 'withObjectCount' ##NO_TEXT,
      END OF c_params.

    TYPES:
      BEGIN OF ty_tag_map,
        tag_id TYPE zabaptags_tag_id,
        data   TYPE REF TO zabaptags_tag_data,
      END OF ty_tag_map,
      BEGIN OF ty_tag_id,
        tag_id TYPE zabaptags_tag_id,
      END OF ty_tag_id.

    DATA:
      action_name              TYPE string,
      tags                     TYPE zabaptags_tag_data_t,
      tags_dac                 TYPE REF TO zcl_abaptags_tags_dac,
      owner                    TYPE sy-uname,
      owner_range              TYPE RANGE OF sy-uname,
      lock_owner               TYPE sy-uname,
      do_not_resolve_hierarchy TYPE abap_bool,
      query                    TYPE string,
      scope                    TYPE string,
      with_object_count        TYPE abap_bool.

    METHODS:
      get_tags_content_handler
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      get_parameters
        IMPORTING
          request TYPE REF TO if_adt_rest_request
        RAISING
          cx_adt_rest,
      delete_tags,
      create_or_update_tags
        RAISING
          cx_adt_rest,
      set_result
        IMPORTING
          tags     TYPE zabaptags_tag_data_t
          response TYPE REF TO if_adt_rest_response
        RAISING
          cx_adt_rest,
      make_tags_global
        RAISING
          cx_adt_rest,
      validate_tag
        IMPORTING
          tag TYPE zabaptags_tag_data
        RAISING
          cx_adt_rest,
      map_tags_to_root
        IMPORTING
          tags_for_root_mapping TYPE zif_abaptags_ty_global=>ty_tag_with_parent_maps.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tags IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    tags_dac = zcl_abaptags_tags_dac=>get_instance( ).
  ENDMETHOD.


  METHOD post.
    DATA(binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).
    IF binary_data IS NOT INITIAL.
      request->get_body_data( EXPORTING content_handler = get_tags_content_handler( )
                              IMPORTING data            = tags ).
    ENDIF.

    get_parameters( request ).

    IF action_name IS INITIAL.
      create_or_update_tags( ).
    ELSE.
      CASE action_name.

        WHEN c_actions-lock.
          zcl_abaptags_tag_util=>lock_tags(
            lock_owner  = lock_owner
            global_tags = xsdbool( scope = zif_abaptags_c_global=>scopes-global ) ).

        WHEN c_actions-unlock.
          zcl_abaptags_tag_util=>unlock_tags( lock_owner ).

        WHEN c_actions-batch_delete.
          delete_tags( ).

        WHEN c_actions-make_global.
          make_tags_global( ).

        WHEN OTHERS.
          RAISE EXCEPTION TYPE zcx_abaptags_adt_error
            EXPORTING
              textid = zcx_abaptags_adt_error=>unknown_tags_action
              msgv1  = |{ action_name }|.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD get.
    DATA: tag_name_range TYPE RANGE OF zabaptags_tag_name,
          tag_id_range   TYPE RANGE OF zabaptags_tag_id.

    get_parameters( request ).

    IF query IS NOT INITIAL.
      tag_name_range = COND #(
        WHEN query CA '+*' THEN VALUE #( ( sign = 'I' option = 'CP' low = query ) )
        ELSE                    VALUE #( ( sign = 'I' option = 'EQ' low = query ) ) ).
    ENDIF.

    DATA(tags) = tags_dac->find_tags(
      owner_range      = owner_range
      name_upper_range = tag_name_range ).

    IF scope = zif_abaptags_c_global=>scopes-all.
      tags = VALUE #( BASE tags
        ( LINES OF zcl_abaptags_tag_util=>get_shared_tags( abap_true ) ) ).
    ENDIF.

    IF with_object_count = abap_true.
      DATA(tag_counts) = tags_dac->get_tagged_obj_count( tag_ids = VALUE #(
                                                         FOR tag IN tags ( sign = 'I' option = 'EQ' low = tag-tag_id ) ) ).

      IF tag_counts IS NOT INITIAL.

        LOOP AT tags ASSIGNING FIELD-SYMBOL(<tag>).
          ASSIGN tag_counts[ tag_id = <tag>-tag_id ] TO FIELD-SYMBOL(<tag_count>).
          CHECK sy-subrc = 0.
          <tag>-tagged_object_count = <tag_count>-count.
        ENDLOOP.

      ENDIF.
    ENDIF.

    set_result(
      tags     = tags
      response = response ).
  ENDMETHOD.


  METHOD get_tags_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TAGS'
      root_name    = 'TAGS'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.


  METHOD get_parameters.
    query = zcl_abaptags_adt_request_util=>get_request_param_value(
      param_name = c_params-query
      upper_case = abap_true
      request    = request ).

    do_not_resolve_hierarchy = zcl_abaptags_adt_request_util=>get_boolean_req_param(
      param_name = c_params-no_hierarchy
      request    = request ).

    with_object_count = zcl_abaptags_adt_request_util=>get_boolean_req_param(
      param_name = c_params-with_object_count
      request    = request ).

    action_name = zcl_abaptags_adt_request_util=>get_request_param_value(
      param_name = c_params-action
      request    = request ).

    scope = zcl_abaptags_adt_request_util=>get_request_param_value(
      param_name    = c_params-scope
      default_value = zif_abaptags_c_global=>scopes-all
      request       = request ).

    CASE scope.

      WHEN zif_abaptags_c_global=>scopes-global.
        owner_range = VALUE #( ( sign = 'I' option = 'EQ' low = space ) ).
        lock_owner = '*'.

      WHEN zif_abaptags_c_global=>scopes-user.
        owner_range = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).
        owner = sy-uname.
        lock_owner = sy-uname.

      WHEN zif_abaptags_c_global=>scopes-all.
        owner_range = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname )
                               ( sign = 'I' option = 'EQ' low = space ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD delete_tags.
    DATA: tag_id_range TYPE RANGE OF zabaptags_tag_id.

    CHECK tags IS NOT INITIAL.

    " determine sub tags via root/tag map
    SELECT *
      FROM zabaptags_tagsrm
      FOR ALL ENTRIES IN @tags
      WHERE root_tag_id = @tags-tag_id
      INTO TABLE @DATA(root_maps).

    LOOP AT root_maps ASSIGNING FIELD-SYMBOL(<root_map>).
      tag_id_range = VALUE #( BASE tag_id_range ( sign = 'I' option = 'EQ' low = <root_map>-tag_id ) ).
    ENDLOOP.

    " collect all selected tags for later deletion
    LOOP AT tags ASSIGNING FIELD-SYMBOL(<tag>).
      tag_id_range = VALUE #( BASE tag_id_range ( sign = 'I' option = 'EQ' low = <tag>-tag_id ) ).
    ENDLOOP.

    SORT tag_id_range.
    DELETE ADJACENT DUPLICATES FROM tag_id_range.

    zcl_abaptags_tags_dac=>get_instance( )->delete_tag_by_id( tag_id_range ).
  ENDMETHOD.


  METHOD create_or_update_tags.
    DATA: tags_to_update        TYPE zif_abaptags_ty_global=>ty_db_tags,
          tags_for_root_mapping TYPE zif_abaptags_ty_global=>ty_tag_with_parent_maps.

    LOOP AT tags ASSIGNING FIELD-SYMBOL(<tag>).
      <tag>-name_upper = to_upper( <tag>-name ).
      validate_tag( <tag> ).
      IF <tag>-tag_id IS INITIAL.
        TRY.
            <tag>-tag_id = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
          CATCH cx_uuid_error INTO DATA(uuid_error).
            RAISE EXCEPTION TYPE zcx_abaptags_adt_error
              EXPORTING
                previous = uuid_error.
        ENDTRY.

        IF <tag>-parent_tag_id IS NOT INITIAL.
          tags_for_root_mapping = VALUE #( BASE tags_for_root_mapping
            ( tag_id        = <tag>-tag_id
              parent_tag_id = <tag>-parent_tag_id ) ).
        ENDIF.

        <tag>-created_by = sy-uname.
        GET TIME STAMP FIELD <tag>-created_date_time.
      ELSE.
        <tag>-changed_by = sy-uname.
        GET TIME STAMP FIELD <tag>-changed_date_time.
      ENDIF.

      tags_to_update = VALUE #( BASE tags_to_update ( CORRESPONDING #( <tag> ) ) ).
    ENDLOOP.

    map_tags_to_root( tags_for_root_mapping ).

    tags_dac->modify_tags( tags_to_update ).
  ENDMETHOD.


  METHOD set_result.
    FIELD-SYMBOLS: <result> TYPE data.

    IF do_not_resolve_hierarchy = abap_true.
      ASSIGN tags TO <result>.
    ELSE.
      DATA(tags_hierarchical) = zcl_abaptags_tag_util=>build_hierarchical_tags( tags ).
      ASSIGN tags_hierarchical TO <result>.
    ENDIF.

    response->set_body_data( content_handler = get_tags_content_handler( )
                             data            = <result> ).
  ENDMETHOD.


  METHOD make_tags_global.
    DATA: name_upper_range TYPE zif_abaptags_ty_global=>ty_tag_name_range.

    LOOP AT tags ASSIGNING FIELD-SYMBOL(<tag>).
      <tag>-name_upper = to_upper( <tag>-name ).

      CHECK <tag>-parent_tag_id IS INITIAL.

      name_upper_range = VALUE #( BASE name_upper_range ( sign = 'I' option = 'EQ' low = <tag>-name_upper ) ).
    ENDLOOP.

    " Check if there is already a global tag with the same name
    DATA(existing_global_tag) = tags_dac->find_first_global_tag( name_upper_range ).

    IF existing_global_tag IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>global_tag_already_exists
          msgv1  = |{ existing_global_tag-name }|.
    ENDIF.

    zcl_abaptags_tag_util=>determine_all_child_tags( EXPORTING tag_id_only = abap_true
                                                     CHANGING  tags        = tags ).

    DATA(tag_ids) = VALUE zif_abaptags_ty_global=>ty_tag_id_range(
      FOR tag IN tags ( sign = 'I' option = 'EQ' low = tag-tag_id ) ).

    tags_dac->convert_tags_to_global( tag_ids ).
    tags_dac->delete_shared_tags_by_id(
      tag_ids            = tag_ids
      unshare_completely = abap_true ).
  ENDMETHOD.


  METHOD validate_tag.
    DATA: tag_id_range  TYPE RANGE OF zabaptags_tag_id.

    IF tag-tag_id IS NOT INITIAL.
      tag_id_range = VALUE #( ( sign = 'I' option = 'EQ' low = tag-tag_id ) ).
    ENDIF.
    IF tag-parent_tag_id IS NOT INITIAL.
      tag_id_range = VALUE #( BASE tag_id_range
        ( sign = 'I' option = 'EQ' low = tag-parent_tag_id ) ).
    ENDIF.

    IF tag_id_range IS NOT INITIAL.
      DATA(tags) = tags_dac->find_tags(
        columns      = VALUE #( ( `TAG_ID` ) ( `NAME` ) )
        tag_id_range = tag_id_range ).

      " check tag existence if tag will be modified
      IF tag-tag_id IS NOT INITIAL AND
         NOT line_exists( tags[ tag_id = tag-tag_id ] ).
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>tag_no_longer_exists
            msgv1  = |{ tag-name }|
            msgv2  = |{ tag-owner }|.
      ENDIF.

      " check parent tag existence if tag is in a hierarchy
      IF tag-parent_tag_id IS NOT INITIAL AND
         NOT line_exists( tags[ tag_id = tag-parent_tag_id ] ).
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>parent_tag_no_longer_exists
            msgv1  = |{ tag-name }|
            msgv2  = |{ tag-owner }|.
      ENDIF.
    ENDIF.

    CLEAR tag_id_range.

    IF tag-tag_id IS NOT INITIAL.
      tag_id_range = VALUE #( ( sign = 'E' option = 'EQ' low = tag-tag_id ) ).
    ENDIF.

    DATA(tag_exists) = tags_dac->exists_tag(
      tag_id_range        = tag_id_range
      parent_tag_id_range = VALUE #( ( sign = 'I' option = 'EQ' low = tag-parent_tag_id ) )
      owner_range         = VALUE #( ( sign = 'I' option = 'EQ' low = tag-owner ) )
      name_upper_range    = VALUE #( ( sign = 'I' option = 'EQ' low = tag-name_upper ) ) ).

    IF tag_exists = abap_true.
      IF tag-parent_tag_id IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>tag_parent_tag_already_exists
            msgv1  = |{ tag-name }|
            msgv2  = |{ tag-owner }|
            msgv3  = |{ tags[ tag_id = tag-parent_tag_id ]-name }|.
      ELSE.
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>tag_no_longer_exists
            msgv1  = |{ tag-name }|
            msgv2  = |{ tag-owner }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD map_tags_to_root.
    DATA: new_map_entries TYPE zif_abaptags_ty_global=>ty_db_tags_root_maps.

    CHECK tags_for_root_mapping IS NOT INITIAL.

    SELECT tag_id,
           root_tag_id
      FROM zabaptags_tagsrm
      FOR ALL ENTRIES IN @tags_for_root_mapping
      WHERE tag_id = @tags_for_root_mapping-parent_tag_id
      INTO TABLE @DATA(upper_root_maps).

    LOOP AT tags_for_root_mapping ASSIGNING FIELD-SYMBOL(<tag_for_mapping>).
      new_map_entries = VALUE #( BASE new_map_entries
       ( tag_id = <tag_for_mapping>-tag_id root_tag_id = <tag_for_mapping>-parent_tag_id ) ).

      " collect map entries in upper hierarchy levels
      LOOP AT upper_root_maps ASSIGNING FIELD-SYMBOL(<upper_root_map>) WHERE tag_id = <tag_for_mapping>-parent_tag_id.
        new_map_entries = VALUE #( BASE new_map_entries
         ( root_tag_id = <upper_root_map>-root_tag_id tag_id = <tag_for_mapping>-tag_id ) ).
      ENDLOOP.

    ENDLOOP.

    IF new_map_entries IS NOT INITIAL.
      INSERT zabaptags_tagsrm FROM TABLE new_map_entries ACCEPTING DUPLICATE KEYS.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
