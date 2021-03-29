"! <p class="shorttext synchronized" lang="en">ADT Resource for ABAP Tags</p>
CLASS zcl_abaptags_adt_res_tags DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      post
        REDEFINITION,
      get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_tags_content_type TYPE string VALUE 'application/vnd.devepos.adt.tags.data.v2+xml' ##NO_TEXT,
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
      END OF ty_tag_id,
      ty_tag_ids TYPE TABLE OF ty_tag_id.

    DATA:
      action_name              TYPE string,
      tags                     TYPE zabaptags_tag_data_t,
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
          cx_adt_rest.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tags IMPLEMENTATION.

  METHOD post.
    DATA(binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).
    IF binary_data IS NOT INITIAL.
      request->get_body_data(
        EXPORTING content_handler = get_tags_content_handler( )
        IMPORTING data            = tags ).
    ENDIF.

    get_parameters( request ).

    IF action_name IS INITIAL.
      " create/update tags
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
          tag_id_range   TYPE RANGE OF zabaptags_tag_id,
          tags           TYPE zabaptags_tag_data_t.

    get_parameters( request ).

    IF query IS NOT INITIAL.
      tag_name_range = COND #(
        WHEN query CA '+*' THEN VALUE #( ( sign = 'I' option = 'CP' low = query ) )
        ELSE                    VALUE #( ( sign = 'I' option = 'EQ' low = query ) ) ).
    ENDIF.

    SELECT *
      FROM zabaptags_tags
      WHERE owner IN @owner_range
        AND name_upper IN @tag_name_range
      INTO CORRESPONDING FIELDS OF TABLE @tags.

    IF with_object_count = abap_true.
      SELECT tag_id, COUNT(*) AS tag_count
        FROM zabaptags_tgobj
        WHERE tag_id IN @tag_id_range
        GROUP BY tag_id
        INTO TABLE @DATA(tag_counts).

      IF tag_counts IS NOT INITIAL.

        LOOP AT tags ASSIGNING FIELD-SYMBOL(<tag>).
          ASSIGN tag_counts[ tag_id = <tag>-tag_id ] TO FIELD-SYMBOL(<tag_count>).
          CHECK sy-subrc = 0.
          <tag>-tagged_object_count = <tag_count>-tag_count.
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
      param_name    = c_params-no_hierarchy
      request       = request ).

    with_object_count = zcl_abaptags_adt_request_util=>get_boolean_req_param(
      param_name    = c_params-with_object_count
      request       = request ).

    action_name = zcl_abaptags_adt_request_util=>get_request_param_value(
      param_name    = c_params-action
      request       = request ).

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
        owner =
        lock_owner = sy-uname.

      WHEN zif_abaptags_c_global=>scopes-all.
        owner_range = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname )
                                  ( sign = 'I' option = 'EQ' low = space )  ).
    ENDCASE.

  ENDMETHOD.

  METHOD delete_tags.
    DATA: tag_id_range TYPE RANGE OF zabaptags_tag_id.

    CHECK tags IS NOT INITIAL.

    zcl_abaptags_tag_util=>determine_all_child_tags(
      EXPORTING tag_id_only = abap_true
      CHANGING  tags        = tags ).

    SORT tags.
    DELETE ADJACENT DUPLICATES FROM tags.

    tag_id_range = VALUE #( FOR tag IN tags ( sign = 'I' option = 'EQ' low = tag-tag_id ) ).

    DELETE FROM zabaptags_tgobj WHERE tag_id IN tag_id_range.
    DELETE FROM zabaptags_tags WHERE tag_id IN tag_id_range.

    COMMIT WORK.
  ENDMETHOD.


  METHOD create_or_update_tags.
    DATA: update             TYPE TABLE OF zabaptags_tags,
          existing_tag_range TYPE RANGE OF zabaptags_tag_id,
          existing_tag_id    TYPE TABLE OF zabaptags_tag_id.

    LOOP AT tags ASSIGNING FIELD-SYMBOL(<tag>).
      <tag>-name_upper = to_upper( <tag>-name ).
      validate_tag( <tag> ).
      IF <tag>-tag_id IS INITIAL.
        <tag>-tag_id = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
        <tag>-created_by = sy-uname.
        GET TIME STAMP FIELD <tag>-created_date_time.
      ELSE.
        <tag>-changed_by = sy-uname.
        GET TIME STAMP FIELD <tag>-changed_date_time.
      ENDIF.

      update = VALUE #( BASE update ( CORRESPONDING #( <tag> ) ) ).
    ENDLOOP.

    MODIFY zabaptags_tags FROM TABLE update.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.


  METHOD set_result.
    FIELD-SYMBOLS: <result> TYPE data.

    IF do_not_resolve_hierarchy = abap_true.
      ASSIGN tags TO <result>.
    ELSE.
      DATA(tags_hierarchical) = zcl_abaptags_tag_util=>build_hierarchical_tags( tags ).
      ASSIGN tags_hierarchical TO <result>.
    ENDIF.

    response->set_body_data(
      content_handler = get_tags_content_handler( )
      data            = <result> ).
  ENDMETHOD.


  METHOD make_tags_global.
    DATA: user_tags        TYPE RANGE OF zabaptags_tag_id,
          changed_datetime TYPE timestampl.

    LOOP AT tags ASSIGNING FIELD-SYMBOL(<tag>).
      <tag>-name_upper = to_upper( <tag>-name ).
    ENDLOOP.

    " Check if there is already a global tag with the same name
    SELECT name
      FROM zabaptags_tags
      FOR ALL ENTRIES IN @tags
      WHERE owner = ''
        AND parent_tag_id = '00000000000000000000000000000000'
        AND name_upper = @tags-name_upper
      INTO TABLE @DATA(existing_global).

    IF existing_global IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>global_tag_already_exists
          msgv1  = |{ existing_global[ 1 ]-name }|.
    ENDIF.

    GET TIME STAMP FIELD changed_datetime.
    zcl_abaptags_tag_util=>determine_all_child_tags(
      EXPORTING tag_id_only = abap_true
      CHANGING  tags        = tags
    ).
    user_tags = VALUE #( FOR tag IN tags ( sign = 'I' option = 'EQ' low = tag-tag_id ) ).
    UPDATE zabaptags_tags SET owner             = '',
                              changed_by        = @sy-uname,
                              changed_date_time = @changed_datetime
                          WHERE tag_id IN @user_tags.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.

  METHOD validate_tag.
    DATA: tag_id_range  TYPE RANGE OF zabaptags_tag_id,
          parent_tag_id TYPE zabaptags_tag_name,
          tag_id        TYPE TABLE OF zabaptags_tags.

    IF tag-tag_id IS NOT INITIAL.
      tag_id_range = VALUE #( ( sign = 'I' option = 'EQ' low = tag-tag_id ) ).
    ENDIF.
    IF tag-parent_tag_id IS NOT INITIAL.
      tag_id_range = VALUE #( BASE tag_id_range
        ( sign = 'I' option = 'EQ' low = tag-parent_tag_id ) ).
    ENDIF.

    IF tag_id_range IS NOT INITIAL.
      SELECT tag_id,
             name
        FROM zabaptags_tags
        WHERE tag_id IN @tag_id_range
        INTO TABLE @tag_id.

      " check tag existence if tag will be modified
      IF tag-tag_id IS NOT INITIAL AND
         NOT line_exists( tag_id[ tag_id = tag-tag_id ] ).
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>tag_no_longer_exists
            msgv1  = |{ tag-name }|
            msgv2  = |{ tag-owner }|.
      ENDIF.

      " check parent tag existence if tag is in a hierarchy
      IF tag-parent_tag_id IS NOT INITIAL AND
         NOT line_exists( tag_id[ tag_id = tag-parent_tag_id ] ).
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

    SELECT SINGLE @abap_true
      FROM zabaptags_tags
      WHERE tag_id IN @tag_id_range
        AND parent_tag_id = @tag-parent_tag_id
        AND name_upper = @tag-name_upper
        AND owner = @tag-owner
      INTO @DATA(exists).

    IF exists = abap_true.
      IF tag-parent_tag_id IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>tag_parent_tag_already_exists
            msgv1  = |{ tag-name }|
            msgv2  = |{ tag-owner }|
            msgv3  = |{ tag_id[ tag_id = tag-parent_tag_id ]-name }|.
      ELSE.
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>tag_no_longer_exists
            msgv1  = |{ tag-name }|
            msgv2  = |{ tag-owner }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
