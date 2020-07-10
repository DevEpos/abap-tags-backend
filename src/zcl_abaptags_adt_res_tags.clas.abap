"! <p class="shorttext synchronized" lang="en">ADT Resource for ABAP Tags</p>
CLASS zcl_abaptags_adt_res_tags DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS post
        REDEFINITION.
    METHODS get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_actions,
        lock         TYPE string VALUE 'lock',
        unlock       TYPE string VALUE 'unlock',
        batch_delete TYPE string VALUE 'batchDelete',
        make_global  TYPE string VALUE 'makeGlobal',
      END OF c_actions.

    CONSTANTS:
      BEGIN OF c_params,
        query             TYPE string VALUE 'query',
        scope             TYPE string VALUE 'scope',
        action            TYPE string VALUE 'action',
        no_hierarchy      TYPE string VALUE 'noHierarchy',
        with_object_count TYPE string VALUE 'withObjectCount',
      END OF c_params.

    TYPES:
      BEGIN OF ty_s_tag_map,
        tag_id TYPE zabaptags_tag_id,
        data   TYPE REF TO zabaptags_tag_data,
      END OF ty_s_tag_map,
      BEGIN OF ty_s_tag_id,
        tag_id TYPE zabaptags_tag_id,
      END OF ty_s_tag_id,
      ty_t_tag_id TYPE TABLE OF ty_s_tag_id.

    DATA mv_action TYPE string.
    DATA mt_tags TYPE zabaptags_tag_data_t.
    DATA mv_owner TYPE sy-uname.
    DATA mt_owner_range TYPE RANGE OF sy-uname.
    DATA mv_lock_owner TYPE sy-uname.
    DATA mf_no_hierarchy TYPE abap_bool.
    DATA: mv_query             TYPE string,
          mv_scope             TYPE string,
          mf_with_object_count TYPE abap_bool.

    METHODS get_tags_content_handler
      RETURNING
        VALUE(ro_content_handler) TYPE REF TO if_adt_rest_content_handler.
    METHODS get_parameters
      IMPORTING
        io_request TYPE REF TO if_adt_rest_request
      RAISING
        cx_adt_rest.
    METHODS delete_tags.
    METHODS find_child_tags
      IMPORTING
        it_tags TYPE ty_t_tag_id
      EXPORTING
        et_tags TYPE ty_t_tag_id.

    METHODS create_or_update_tags.
    METHODS update_tag
      IMPORTING
        iv_parent_tag_id TYPE zabaptags_tag_id OPTIONAL
      CHANGING
        cs_tag           TYPE zabaptags_tag_data.

    METHODS set_result
      IMPORTING
        it_tags     TYPE zabaptags_tag_data_t
        io_response TYPE REF TO if_adt_rest_response
      RAISING
        cx_adt_rest.
    METHODS make_tags_global
      RAISING
        cx_adt_rest.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tags IMPLEMENTATION.

  METHOD post.
    DATA(lv_binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).
    IF lv_binary_data IS NOT INITIAL.
      request->get_body_data(
         EXPORTING content_handler = get_tags_content_handler( )
         IMPORTING data            = mt_tags
      ).
    ENDIF.

    get_parameters( request ).

    IF mv_action IS INITIAL.
      " create/update tags
      create_or_update_tags( ).
    ELSE.
      CASE mv_action.

        WHEN c_actions-lock.
          zcl_abaptags_tag_util=>lock_tags(
              iv_lock_owner  = mv_lock_owner
              if_global_tags = xsdbool( mv_scope = zif_abaptags_c_global=>c_scopes-global )
          ).

        WHEN c_actions-unlock.
          zcl_abaptags_tag_util=>unlock_tags( mv_lock_owner ).

        WHEN c_actions-batch_delete.
          delete_tags( ).

        WHEN c_actions-make_global.
          make_tags_global( ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD get.
    DATA: lt_tag_name_range TYPE RANGE OF zabaptags_tag_name,
          lt_tag_id_range   TYPE RANGE OF zabaptags_tag_id,
          lt_tags           TYPE zabaptags_tag_data_t.

    get_parameters( request ).

    IF mv_query IS NOT INITIAL.
      lt_tag_name_range = COND #(
        WHEN mv_query CA '+*' THEN VALUE #( ( sign = 'I' option = 'CP' low = mv_query ) )
        ELSE                       VALUE #( ( sign = 'I' option = 'EQ' low = mv_query ) )
      ).
    ENDIF.

    SELECT *
      FROM zabaptags_tags
      WHERE owner IN @mt_owner_range
        AND name_upper IN @lt_tag_name_range
    INTO CORRESPONDING FIELDS OF TABLE @lt_tags.

    IF mf_with_object_count = abap_true.
      SELECT tag_id, COUNT(*) AS tag_count
        FROM zabaptags_tgobj
        WHERE tag_id IN @lt_tag_id_range
        GROUP BY tag_id
      INTO TABLE @DATA(lt_tag_counts).

      IF lt_tag_counts IS NOT INITIAL.
        LOOP AT lt_tags ASSIGNING FIELD-SYMBOL(<ls_tag>).
          ASSIGN lt_tag_counts[ tag_id = <ls_tag>-tag_id ] TO FIELD-SYMBOL(<ls_tag_count>).
          CHECK sy-subrc = 0.
          <ls_tag>-tagged_object_count = <ls_tag_count>-tag_count.
        ENDLOOP.
      ENDIF.
    ENDIF.

    set_result(
        it_tags     = lt_tags
        io_response = response
    ).
  ENDMETHOD.

  METHOD get_tags_content_handler.
    ro_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
       st_name      = 'ZABAPTAGS_TAGS'
       root_name    = 'TAGS'
       content_type = if_rest_media_type=>gc_appl_xml
    ).
  ENDMETHOD.

  METHOD get_parameters.
    mv_query = zcl_abaptags_adt_request_util=>get_request_param_value(
      iv_param_name = c_params-query
      if_upper_case = abap_true
      io_request    = io_request
    ).

    mf_no_hierarchy = zcl_abaptags_adt_request_util=>get_boolean_req_param(
      iv_param_name    = c_params-no_hierarchy
      io_request       = io_request
    ).

    mf_with_object_count = zcl_abaptags_adt_request_util=>get_boolean_req_param(
      iv_param_name    = c_params-with_object_count
      io_request       = io_request
    ).

    mv_action = zcl_abaptags_adt_request_util=>get_request_param_value(
      iv_param_name    = c_params-action
      io_request       = io_request
    ).

    mv_scope = zcl_abaptags_adt_request_util=>get_request_param_value(
        iv_param_name    = c_params-scope
        iv_default_value = zif_abaptags_c_global=>c_scopes-all
        io_request       = io_request
    ).

    CASE mv_scope.

      WHEN zif_abaptags_c_global=>c_scopes-global.
        mt_owner_range = VALUE #( ( sign = 'I' option = 'EQ' low = space ) ).
        mv_lock_owner = '*'.

      WHEN zif_abaptags_c_global=>c_scopes-user.
        mt_owner_range = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname ) ).
        mv_owner =
        mv_lock_owner = sy-uname.

      WHEN zif_abaptags_c_global=>c_scopes-all.
        mt_owner_range = VALUE #( ( sign = 'I' option = 'EQ' low = sy-uname )
                                  ( sign = 'I' option = 'EQ' low = space )  ).
    ENDCASE.

  ENDMETHOD.

  METHOD delete_tags.
    DATA: lt_tag_id_range TYPE RANGE OF zabaptags_tag_id.

    CHECK mt_tags IS NOT INITIAL.

    zcl_abaptags_tag_util=>determine_all_child_tags(
      EXPORTING if_only_tag_id = abap_true
      CHANGING  ct_tags        = mt_tags
    ).

    SORT mt_tags.
    DELETE ADJACENT DUPLICATES FROM mt_tags.

    lt_tag_id_range = VALUE #( FOR tag IN mt_tags ( sign = 'I' option = 'EQ' low = tag-tag_id ) ).

    DELETE FROM zabaptags_tgobj WHERE tag_id IN lt_tag_id_range.
    DELETE FROM zabaptags_tags WHERE tag_id IN lt_tag_id_range.

    COMMIT WORK.
  ENDMETHOD.

  METHOD find_child_tags.
    CHECK it_tags IS NOT INITIAL.

    CLEAR et_tags.

    SELECT tag_id
      FROM zabaptags_tags
      FOR ALL ENTRIES IN @it_tags
      WHERE parent_tag_id = @it_tags-tag_id
    INTO CORRESPONDING FIELDS OF TABLE @et_tags.
  ENDMETHOD.

  METHOD create_or_update_tags.
    DATA: lt_update TYPE TABLE OF zabaptags_tags.

    LOOP AT mt_tags ASSIGNING FIELD-SYMBOL(<ls_tag>).
      IF <ls_tag>-tag_id IS INITIAL.
        <ls_tag>-tag_id = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
        <ls_tag>-created_by = sy-uname.
        GET TIME STAMP FIELD <ls_tag>-created_date_time.
        <ls_tag>-owner = mv_owner.
      ELSEIF <ls_tag>-is_changed = abap_true.
        <ls_tag>-changed_by = sy-uname.
        GET TIME STAMP FIELD <ls_tag>-changed_date_time.
      ENDIF.

      <ls_tag>-name_upper = to_upper( <ls_tag>-name ).
      lt_update = VALUE #( BASE lt_update ( CORRESPONDING #( <ls_tag> ) ) ).
    ENDLOOP.

    MODIFY zabaptags_tags FROM TABLE lt_update.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.


  METHOD update_tag.
    cs_tag-parent_tag_id = iv_parent_tag_id.

    IF cs_tag-tag_id IS INITIAL.
      cs_tag-tag_id = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
      cs_tag-created_by = sy-uname.
      GET TIME STAMP FIELD cs_tag-created_date_time.
      cs_tag-owner = mv_owner.
    ELSEIF cs_tag-is_changed = abap_true.
      cs_tag-changed_by = sy-uname.
      GET TIME STAMP FIELD cs_tag-changed_date_time.
    ENDIF.

    cs_tag-name_upper = to_upper( cs_tag-name ).
  ENDMETHOD.


  METHOD set_result.
    FIELD-SYMBOLS: <lt_result> TYPE data.

    IF mf_no_hierarchy = abap_true.
      ASSIGN it_tags TO <lt_result>.
    ELSE.
      DATA(lt_tags_hierarchical) = zcl_abaptags_tag_util=>build_hierarchical_tags( it_tags ).
      ASSIGN lt_tags_hierarchical TO <lt_result>.
    ENDIF.

    io_response->set_body_data(
        content_handler = get_tags_content_handler( )
        data            = <lt_result>
    ).
  ENDMETHOD.


  METHOD make_tags_global.
    DATA: lt_user_tags        TYPE RANGE OF zabaptags_tag_id,
          lv_changed_datetime TYPE timestampl.

    " Check if there is already a global tag with the same name
    SELECT name
      FROM zabaptags_tags
      FOR ALL ENTRIES IN @mt_tags
      WHERE owner IS INITIAL
        AND parent_tag_id IS INITIAL
        AND name_upper = @mt_tags-name_upper
    INTO TABLE @DATA(lt_existing_global).

    IF lt_existing_global IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>global_tag_already_exists
          msgv1  = |{ lt_existing_global[ 1 ]-name }|.
    ENDIF.

    GET TIME STAMP FIELD lv_changed_datetime.
    zcl_abaptags_tag_util=>determine_all_child_tags(
      EXPORTING if_only_tag_id = abap_true
      CHANGING  ct_tags        = mt_tags
    ).
    lt_user_tags = VALUE #( FOR tag IN mt_tags ( sign = 'I' option = 'EQ' low = tag-tag_id ) ).
    UPDATE zabaptags_tags SET owner             = '',
                              changed_by        = @sy-uname,
                              changed_date_time = @lv_changed_datetime
                          WHERE tag_id IN @lt_user_tags.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
