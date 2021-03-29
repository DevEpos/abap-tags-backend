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
      END OF c_actions,
      BEGIN OF c_params,
        action     TYPE string VALUE 'action',
        object_uri TYPE string VALUE 'objectUri',
      END OF c_params.

    TYPES:
      BEGIN OF ty_tag_map,
        tag_name TYPE zabaptags_tag_name,
        owner    TYPE uname,
        id       TYPE zabaptags_tag_id,
      END OF ty_tag_map.

    DATA:
      action_name       TYPE string,
      new_tag_map       TYPE HASHED TABLE OF ty_tag_map WITH UNIQUE KEY tag_name owner,
      tagged_objects    TYPE zabaptags_tagged_object_t,
      tagged_objects_db TYPE TABLE OF zabaptags_tgobj.

    METHODS:
      get_content_handler
        RETURNING
          VALUE(content_handler) TYPE REF TO if_adt_rest_content_handler,
      create_tagged_objects
        RAISING
          cx_adt_rest,
      prepare_for_db_insert
        RAISING
          cx_adt_rest,
      create_non_persisted_tags
        RAISING
          cx_adt_rest,
      delete_tags_from_objects
        RAISING
          cx_adt_rest,
      validate_tags
        RAISING
          cx_adt_rest.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobj IMPLEMENTATION.

  METHOD post.
    DATA(binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).
    IF binary_data IS NOT INITIAL.
      request->get_body_data(
        EXPORTING content_handler = get_content_handler( )
        IMPORTING data            = tagged_objects ).
    ENDIF.

    CHECK tagged_objects IS NOT INITIAL.

    action_name = zcl_abaptags_adt_request_util=>get_request_param_value(
      param_name    = c_params-action
      request       = request ).

    IF action_name IS INITIAL.
*      " create/update tags
      create_tagged_objects( ).
    ELSE.
      CASE action_name.
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
    DATA: texts TYPE TABLE OF seu_objtxt,
          tags  TYPE zcl_abaptags_tag_util=>ty_tag_infos.

    DATA(object_uri) = zcl_abaptags_adt_request_util=>get_request_param_value(
      param_name    = c_params-object_uri
      mandatory     = abap_true
      request       = request ).

    zcl_abaptags_adt_util=>map_uri_to_wb_object(
     EXPORTING uri         = object_uri
     IMPORTING object_name = DATA(tadir_object)
               tadir_type  = DATA(tadir_type)
               object_type = DATA(adt_type) ).

    SELECT DISTINCT
           tags~tag_id,
           tags~parent_tag_id,
           tags~owner,
           tags~name
      FROM zabaptags_tgobj AS tgobj
        INNER JOIN zabaptags_tags AS tags
          ON tgobj~tag_id = tags~tag_id
      WHERE tgobj~object_name = @tadir_object
        AND tgobj~object_type = @tadir_type
        AND ( tags~owner = @space OR tags~owner = @sy-uname )
      ORDER BY owner, name
      INTO CORRESPONDING FIELDS OF TABLE @tags.

    zcl_abaptags_tag_util=>det_hierarchical_tag_names( CHANGING tag_info = tags ).

    texts = VALUE #( ( object = tadir_type obj_name = tadir_object ) ).

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab = texts.

    tagged_objects = VALUE #(
      ( adt_obj_ref = VALUE #(
          name        = tadir_object
          description = texts[ 1 ]-stext
          tadir_type  = tadir_type
          type        = COND #(
            WHEN adt_type-subtype_wb <> space THEN |{ adt_type-objtype_tr }/{ adt_type-subtype_wb }| ELSE adt_type )
          uri         = object_uri )
        tags = VALUE #(
          FOR tag IN tags
          ( tag_id   = tag-tag_id
            tag_name = tag-full_hierarchy
            owner    = tag-owner ) ) ) ).

    response->set_body_data(
      content_handler = get_content_handler( )
      data            = tagged_objects ).
  ENDMETHOD.

  METHOD get_content_handler.
    content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
      st_name      = 'ZABAPTAGS_TAGGED_OBJECTS'
      root_name    = 'TAGGED_OBJECTS'
      content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.

  METHOD create_tagged_objects.
    create_non_persisted_tags( ).
    prepare_for_db_insert( ).

    IF tagged_objects_db IS NOT INITIAL.
      INSERT zabaptags_tgobj FROM TABLE tagged_objects_db ACCEPTING DUPLICATE KEYS.
      IF sy-dbcnt > 0.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_non_persisted_tags.
    DATA: new_tags          TYPE TABLE OF zabaptags_tags,
          created_date_time TYPE tzonref-tstamps.

    FIELD-SYMBOLS: <tagged_object> TYPE zabaptags_tagged_object,
                   <tag>           TYPE zabaptags_adt_object_tag.

    ASSIGN tagged_objects[ 1 ] TO <tagged_object>.

    LOOP AT <tagged_object>-tags ASSIGNING <tag> WHERE tag_id IS INITIAL.
      IF sy-tabix = 1.
        GET TIME STAMP FIELD created_date_time.
      ENDIF.

      TRY.
          DATA(tag_id) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
        CATCH cx_uuid_error.
      ENDTRY.
      new_tag_map = VALUE #( BASE new_tag_map ( id       = tag_id
                                                tag_name = <tag>-tag_name
                                                owner    = <tag>-owner ) ).
      new_tags = VALUE #( BASE new_tags
        ( tag_id            = tag_id
          name              = <tag>-tag_name
          name_upper        = to_upper( <tag>-tag_name )
          owner             = <tag>-owner
          created_by        = sy-uname
          created_date_time = created_date_time ) ).
    ENDLOOP.

    IF new_tags IS INITIAL.
      RETURN.
    ENDIF.

*.. check if some of the tags already exist in the database
    SELECT tag_id, name, owner
      FROM zabaptags_tags
      FOR ALL ENTRIES IN @new_tags
      WHERE name_upper = @new_tags-name_upper
        AND owner      = @new_tags-owner
      INTO TABLE @DATA(existing_tags)
      UP TO 1 ROWS.

    IF sy-subrc = 0.
      DATA(first_existing_tag) = existing_tags[ 1 ].
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>tag_already_exists
          msgv1  = |{ first_existing_tag-name }|
          msgv2  = COND #( WHEN first_existing_tag-owner IS NOT INITIAL THEN first_existing_tag-owner ELSE '*' ).
    ENDIF.

    INSERT zabaptags_tags FROM TABLE new_tags.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      CLEAR new_tag_map.
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>tags_persisting_failure.
    ENDIF.

  ENDMETHOD.

  METHOD prepare_for_db_insert.
    DATA: tadir_object  TYPE string,
          tadir_type    TYPE trobjtype,
          parent_object TYPE string,
          parent_type   TYPE trobjtype.

    FIELD-SYMBOLS: <tagged_object> TYPE zabaptags_tagged_object,
                   <tag>           TYPE zabaptags_adt_object_tag.
    validate_tags( ).

    LOOP AT tagged_objects ASSIGNING <tagged_object>.

      zcl_abaptags_adt_util=>map_uri_to_wb_object(
        EXPORTING uri         = <tagged_object>-adt_obj_ref-uri
        IMPORTING object_name = tadir_object
                  tadir_type  = tadir_type ).

      LOOP AT <tagged_object>-tags ASSIGNING <tag>.
        CLEAR: parent_object,
               parent_type.

        IF <tag>-tag_id IS INITIAL.
          ASSIGN new_tag_map[ tag_name = <tag>-tag_name
                                 owner    = <tag>-owner ] TO FIELD-SYMBOL(<new_tag>).
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_abaptags_adt_error
              EXPORTING
                textid = zcx_abaptags_adt_error=>tag_with_name_not_found
                msgv1  = |{ <tag>-tag_name }|
                msgv2  = COND #( WHEN <tag>-owner IS INITIAL THEN '*' ELSE <tag>-owner ).
          ENDIF.
          <tag>-tag_id = <new_tag>-id.
        ENDIF.

        IF <tag>-parent_uri IS NOT INITIAL.
          zcl_abaptags_adt_util=>map_uri_to_wb_object(
            EXPORTING uri         = <tag>-parent_uri
            IMPORTING object_name = parent_object
                      tadir_type  = parent_type ).
        ENDIF.

        tagged_objects_db = VALUE #( BASE tagged_objects_db
         ( object_type        = tadir_type
           object_name        = tadir_object
           tag_id             = <tag>-tag_id
           parent_object_type = parent_type
           parent_object_name = parent_object
           tagged_by          = sy-uname
           tagged_date        = sy-datum ) ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD delete_tags_from_objects.
    DATA: tadir_object          TYPE string,
          tadir_type            TYPE trobjtype,
          tagged_objects_delete TYPE TABLE OF zabaptags_tgobj.

    FIELD-SYMBOLS: <tagged_object> TYPE zabaptags_tagged_object,
                   <tag>           TYPE zabaptags_adt_object_tag.


    LOOP AT tagged_objects ASSIGNING <tagged_object>.

      zcl_abaptags_adt_util=>map_uri_to_wb_object(
        EXPORTING uri         = <tagged_object>-adt_obj_ref-uri
        IMPORTING object_name = tadir_object
                  tadir_type  = tadir_type ).

      LOOP AT <tagged_object>-tags ASSIGNING <tag>.
        tagged_objects_delete = VALUE #( BASE tagged_objects_delete
         ( object_type        = tadir_type
           object_name        = tadir_object
           tag_id             = <tag>-tag_id ) ).
      ENDLOOP.

    ENDLOOP.

    IF tagged_objects_delete IS NOT INITIAL.
      DELETE zabaptags_tgobj FROM TABLE tagged_objects_delete.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD validate_tags.
    DATA: remaining_tags TYPE TABLE OF zabaptags_tags,
          tag_ids        TYPE SORTED TABLE OF zabaptags_tag_id WITH UNIQUE KEY table_line.

    LOOP AT tagged_objects ASSIGNING FIELD-SYMBOL(<tagged_obj>).

      LOOP AT <tagged_obj>-tags ASSIGNING FIELD-SYMBOL(<tag>) WHERE tag_id IS NOT INITIAL.
        INSERT <tag>-tag_id INTO TABLE tag_ids.
      ENDLOOP.

    ENDLOOP.

    IF tag_ids IS NOT INITIAL.
      SELECT tag_id,
             name,
             parent_tag_id
        FROM zabaptags_tags
        FOR ALL ENTRIES IN @tag_ids
        WHERE tag_id = @tag_ids-table_line
        INTO CORRESPONDING FIELDS OF TABLE @remaining_tags.

      IF lines( remaining_tags ) <> lines( tag_ids ).
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>chosen_tags_no_longer_exist.
      ENDIF.
    ENDIF.

    CLEAR tag_ids.
    LOOP AT remaining_tags ASSIGNING FIELD-SYMBOL(<remaining_tag>) WHERE parent_tag_id IS NOT INITIAL.
      INSERT <remaining_tag>-parent_tag_id INTO TABLE tag_ids.
    ENDLOOP.

    IF tag_ids IS NOT INITIAL.
      SELECT tag_id,
             name
        FROM zabaptags_tags
        FOR ALL ENTRIES IN @tag_ids
        WHERE tag_id = @tag_ids-table_line
        INTO CORRESPONDING FIELDS OF TABLE @remaining_tags.

      IF lines( remaining_tags ) <> lines( tag_ids ).
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING
            textid = zcx_abaptags_adt_error=>parents_of_chs_tags_deleted.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
