"! <p class="shorttext synchronized" lang="en">ADT Resource for Tagged Objects</p>
CLASS zcl_abaptags_adt_res_tgobj DEFINITION
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
      END OF c_actions.
    CONSTANTS:
      BEGIN OF c_params,
        action     TYPE string VALUE 'action',
        object_uri TYPE string VALUE 'objectUri',
      END OF c_params.

    TYPES:
      BEGIN OF ty_s_new_tag_map,
        tag_name TYPE zabaptags_tag_name,
        owner    TYPE uname,
        id       TYPE zabaptags_tag_id,
      END OF ty_s_new_tag_map.

    DATA mv_action TYPE string.
    DATA mt_new_tag_map TYPE HASHED TABLE OF ty_s_new_tag_map WITH UNIQUE KEY tag_name owner.
    DATA mt_tagged_objects TYPE zabaptags_tagged_object_t.
    DATA mt_tagged_object_int TYPE TABLE OF zabaptags_tgobj.

    METHODS get_content_handler
      RETURNING
        VALUE(ro_content_handler) TYPE REF TO if_adt_rest_content_handler.
    METHODS create_tagged_objects
      RAISING
        cx_adt_rest.
    METHODS prepare_for_db_insert
      RAISING
        cx_adt_rest.
    METHODS create_non_persisted_tags
      RAISING
        cx_adt_rest.
    METHODS delete_tags_from_objects
      RAISING
        cx_adt_rest.
    METHODS validate_tags
      RAISING
        cx_adt_rest.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobj IMPLEMENTATION.

  METHOD post.
    DATA(lv_binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).
    IF lv_binary_data IS NOT INITIAL.
      request->get_body_data(
         EXPORTING content_handler = get_content_handler( )
         IMPORTING data            = mt_tagged_objects
      ).
    ENDIF.

    CHECK mt_tagged_objects IS NOT INITIAL.

    mv_action = zcl_abaptags_adt_request_util=>get_request_param_value(
        iv_param_name    = c_params-action
        io_request       = request
    ).

    IF mv_action IS INITIAL.
*      " create/update tags
      create_tagged_objects( ).
    ELSE.
      CASE mv_action.
*
*        WHEN c_actions-lock.
*          lock( ).
*
*        WHEN c_actions-unlock.
*          unlock( ).
*
        WHEN c_actions-batch_delete.
          delete_tags_from_objects( ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.

  METHOD get.
    DATA: lt_texts TYPE TABLE OF seu_objtxt,
          lt_tags  TYPE zcl_abaptags_tag_util=>ty_t_tag_info.

    DATA(lv_object_uri) = zcl_abaptags_adt_request_util=>get_request_param_value(
        iv_param_name    = c_params-object_uri
        if_mandatory     = abap_true
        io_request       = request
    ).

    zcl_abaptags_adt_util=>map_uri_to_wb_object(
     EXPORTING iv_uri         = lv_object_uri
     IMPORTING ev_object_name = DATA(lv_tadir_object)
               ev_tadir_type  = DATA(lv_tadir_type)
               es_object_type = DATA(ls_adt_type)
    ).

    SELECT DISTINCT
           tags~tag_id,
           tags~parent_tag_id,
           tags~owner,
           tags~name
      FROM zabaptags_tgobj AS tgobj
        INNER JOIN zabaptags_tags AS tags
          ON tgobj~tag_id = tags~tag_id
      WHERE tgobj~object_name = @lv_tadir_object
        AND tgobj~object_type = @lv_tadir_type
        AND ( tags~owner = @space OR tags~owner = @sy-uname )
      ORDER BY owner, name
    INTO CORRESPONDING FIELDS OF TABLE @lt_tags.

    zcl_abaptags_tag_util=>det_hierarchical_tag_names( CHANGING ct_tag_info = lt_tags ).

    lt_texts = VALUE #( ( object = lv_tadir_type obj_name = lv_tadir_object ) ).

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab = lt_texts.

    mt_tagged_objects = VALUE #(
      ( adt_obj_ref = VALUE #(
         name        = lv_tadir_object
         description = lt_texts[ 1 ]-stext
         tadir_type  = lv_tadir_type
         type        = COND #( WHEN ls_adt_type-subtype_wb <> space THEN |{ ls_adt_type-objtype_tr }/{ ls_adt_type-subtype_wb }| ELSE ls_adt_type )
         uri         = lv_object_uri
        )
        tags = VALUE #(
          FOR tag IN lt_tags
          ( tag_id   = tag-tag_id
            tag_name = tag-full_hierarchy
            owner    = tag-owner )
        )
      )
    ).

    response->set_body_data(
        content_handler = get_content_handler( )
        data            = mt_tagged_objects
    ).
  ENDMETHOD.

  METHOD get_content_handler.
    ro_content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
       st_name      = 'ZABAPTAGS_TAGGED_OBJECTS'
       root_name    = 'TAGGED_OBJECTS'
       content_type = if_rest_media_type=>gc_appl_xml
    ).
  ENDMETHOD.

  METHOD create_tagged_objects.
    create_non_persisted_tags( ).
    prepare_for_db_insert( ).

    IF mt_tagged_object_int IS NOT INITIAL.
      INSERT zabaptags_tgobj FROM TABLE mt_tagged_object_int ACCEPTING DUPLICATE KEYS.
      IF sy-dbcnt > 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_non_persisted_tags.
    DATA: lt_new_tags          TYPE TABLE OF zabaptags_tags,
          lv_created_date_time TYPE tzonref-tstamps.

    FIELD-SYMBOLS: <ls_tagged_object> TYPE zabaptags_tagged_object,
                   <ls_tag>           TYPE zabaptags_adt_object_tag.

    ASSIGN mt_tagged_objects[ 1 ] TO <ls_tagged_object>.

    LOOP AT <ls_tagged_object>-tags ASSIGNING <ls_tag> WHERE tag_id IS INITIAL.
      IF sy-tabix = 1.
        GET TIME STAMP FIELD lv_created_date_time.
      ENDIF.

      TRY.
          DATA(lv_tag_id) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
        CATCH cx_uuid_error.
      ENDTRY.
      mt_new_tag_map = VALUE #( BASE mt_new_tag_map ( id       = lv_tag_id
                                                      tag_name = <ls_tag>-tag_name
                                                      owner    = <ls_tag>-owner ) ).
      lt_new_tags = VALUE #( BASE lt_new_tags
        ( tag_id            = lv_tag_id
          name              = <ls_tag>-tag_name
          name_upper        = to_upper( <ls_tag>-tag_name )
          owner             = <ls_tag>-owner
          created_by        = sy-uname
          created_date_time = lv_created_date_time )
      ).
    ENDLOOP.

    IF lt_new_tags IS INITIAL.
      RETURN.
    ENDIF.

*.. check if some of the tags already exist in the database
    SELECT tag_id, name, owner
      FROM zabaptags_tags
      FOR ALL ENTRIES IN @lt_new_tags
      WHERE name_upper = @lt_new_tags-name_upper
        AND owner      = @lt_new_tags-owner
    INTO TABLE @DATA(lt_existing_tags)
      UP TO 1 ROWS.

    IF sy-subrc = 0.
      DATA(ls_first_existing_tag) = lt_existing_tags[ 1 ].
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>tag_already_exists
          msgv1  = |{ ls_first_existing_tag-name }|
          msgv2  = COND #( WHEN ls_first_existing_tag-owner IS NOT INITIAL THEN ls_first_existing_tag-owner ELSE '*' ).
    ENDIF.

    INSERT zabaptags_tags FROM TABLE lt_new_tags.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      CLEAR mt_new_tag_map.
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>tags_persisting_failure.
    ENDIF.

  ENDMETHOD.

  METHOD prepare_for_db_insert.
    DATA: lv_tadir_object  TYPE string,
          lv_tadir_type    TYPE trobjtype,
          lv_parent_object TYPE string,
          lv_parent_type   TYPE trobjtype.

    FIELD-SYMBOLS: <ls_tagged_object> TYPE zabaptags_tagged_object,
                   <ls_tag>           TYPE zabaptags_adt_object_tag.
    validate_tags( ).

    LOOP AT mt_tagged_objects ASSIGNING <ls_tagged_object>.

      zcl_abaptags_adt_util=>map_uri_to_wb_object(
        EXPORTING iv_uri         = <ls_tagged_object>-adt_obj_ref-uri
        IMPORTING ev_object_name = lv_tadir_object
                  ev_tadir_type  = lv_tadir_type
      ).

      LOOP AT <ls_tagged_object>-tags ASSIGNING <ls_tag>.
        CLEAR: lv_parent_object,
               lv_parent_type.

        IF <ls_tag>-tag_id IS INITIAL.
          ASSIGN mt_new_tag_map[ tag_name = <ls_tag>-tag_name
                                 owner    = <ls_tag>-owner ] TO FIELD-SYMBOL(<ls_new_tag>).
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_abaptags_adt_error
              EXPORTING
                textid = zcx_abaptags_adt_error=>tag_with_name_not_found
                msgv1  = |{ <ls_tag>-tag_name }|
                msgv2  = COND #( WHEN <ls_tag>-owner IS INITIAL THEN '*' ELSE <ls_tag>-owner ).
          ENDIF.
          <ls_tag>-tag_id = <ls_new_tag>-id.
        ENDIF.

        IF <ls_tag>-parent_uri IS NOT INITIAL.
          zcl_abaptags_adt_util=>map_uri_to_wb_object(
            EXPORTING iv_uri         = <ls_tag>-parent_uri
            IMPORTING ev_object_name = lv_parent_object
                      ev_tadir_type  = lv_parent_type
          ).
        ENDIF.

        mt_tagged_object_int = VALUE #( BASE mt_tagged_object_int
         ( object_type        = lv_tadir_type
           object_name        = lv_tadir_object
           tag_id             = <ls_tag>-tag_id
           parent_object_type = lv_parent_type
           parent_object_name = lv_parent_object
           tagged_by          = sy-uname
           tagged_date        = sy-datum )
        ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD delete_tags_from_objects.
    DATA: lv_tadir_object          TYPE string,
          lv_tadir_type            TYPE trobjtype,
          lt_tagged_objects_delete TYPE TABLE OF zabaptags_tgobj.

    FIELD-SYMBOLS: <ls_tagged_object> TYPE zabaptags_tagged_object,
                   <ls_tag>           TYPE zabaptags_adt_object_tag.


    LOOP AT mt_tagged_objects ASSIGNING <ls_tagged_object>.

      zcl_abaptags_adt_util=>map_uri_to_wb_object(
        EXPORTING iv_uri         = <ls_tagged_object>-adt_obj_ref-uri
        IMPORTING ev_object_name = lv_tadir_object
                  ev_tadir_type  = lv_tadir_type
      ).

      LOOP AT <ls_tagged_object>-tags ASSIGNING <ls_tag>.
        lt_tagged_objects_delete = VALUE #( BASE lt_tagged_objects_delete
         ( object_type        = lv_tadir_type
           object_name        = lv_tadir_object
           tag_id             = <ls_tag>-tag_id )
        ).
      ENDLOOP.

    ENDLOOP.

    IF lt_tagged_objects_delete IS NOT INITIAL.
      DELETE zabaptags_tgobj FROM TABLE lt_tagged_objects_delete.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD validate_tags.
    DATA: lt_remaining_tags TYPE TABLE OF zabaptags_tags,
          lt_tag_id         TYPE SORTED TABLE OF zabaptags_tag_id WITH UNIQUE KEY table_line.

    LOOP AT mt_tagged_objects ASSIGNING FIELD-SYMBOL(<ls_tagged_obj>).

      LOOP AT <ls_tagged_obj>-tags ASSIGNING FIELD-SYMBOL(<ls_tag>).
        INSERT <ls_tag>-tag_id INTO TABLE lt_tag_id.
      ENDLOOP.

    ENDLOOP.

    IF lt_tag_id IS NOT INITIAL.
      SELECT tag_id,
             name,
             parent_tag_id
        FROM zabaptags_tags
        FOR ALL ENTRIES IN @lt_tag_id
        WHERE tag_id = @lt_tag_id-table_line
      INTO CORRESPONDING FIELDS OF TABLE @lt_remaining_tags.

      IF lines( lt_remaining_tags ) <> lines( lt_tag_id ).
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>chosen_tags_no_longer_exist.
      ENDIF.
    ENDIF.

    CLEAR lt_tag_id.
    LOOP AT lt_remaining_tags ASSIGNING FIELD-SYMBOL(<ls_remaining_tag>) WHERE parent_tag_id IS NOT INITIAL.
      INSERT <ls_remaining_tag>-parent_tag_id INTO TABLE lt_tag_id.
    ENDLOOP.

    IF lt_tag_id IS NOT INITIAL.
      SELECT tag_id,
             name
        FROM zabaptags_tags
        FOR ALL ENTRIES IN @lt_tag_id
        WHERE tag_id = @lt_tag_id-table_line
      INTO CORRESPONDING FIELDS OF TABLE @lt_remaining_tags.

      IF lines( lt_remaining_tags ) <> lines( lt_tag_id ).
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>parents_of_chs_tags_deleted.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
