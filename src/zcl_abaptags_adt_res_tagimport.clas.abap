"! <p class="shorttext synchronized">Resource for ABAP Tags Import</p>
CLASS zcl_abaptags_adt_res_tagimport DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_pers_to_imp_tag,
        import_tag_id TYPE zabaptags_tag_id,
        db_tag_id     TYPE zabaptags_tag_id,
      END OF ty_pers_to_imp_tag.

    DATA db_to_imp_tag_map TYPE HASHED TABLE OF ty_pers_to_imp_tag WITH UNIQUE KEY import_tag_id.

    DATA import_request TYPE zabaptags_data_export.

    METHODS get_request_content_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS import_tags.
    METHODS import_tagged_objects.
    METHODS import_shared_tags.
    METHODS validate_tagged_objects
              RAISING
                zcx_abaptags_adt_error.

    METHODS propagate_parent_tag_id
      IMPORTING
        tag TYPE zabaptags_tag_data.

    METHODS get_child_tags
      IMPORTING
        tag           TYPE zabaptags_tag_data
      RETURNING
        VALUE(result) TYPE zabaptags_tag_data_t.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tagimport IMPLEMENTATION.
  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = import_request ).

    validate_tagged_objects( ).

    import_tags( ).
    import_tagged_objects( ).
    import_shared_tags( ).
  ENDMETHOD.

  METHOD get_request_content_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                 st_name      = 'ZABAPTAGS_TAG_IMPORT_REQUEST'
                 root_name    = 'REQUEST'
                 content_type = if_rest_media_type=>gc_appl_xml ).
  ENDMETHOD.

  METHOD import_tags.
    DATA temp_tags LIKE import_request-tags.
    DATA existing_tags LIKE import_request-tags.
    DATA tags_to_update TYPE zif_abaptags_ty_global=>ty_db_tags.
    DATA tags_to_insert TYPE zif_abaptags_ty_global=>ty_db_tags.

    DATA(pending_tags) = import_request-tags.

    WHILE pending_tags IS NOT INITIAL.
      " perform some pre data conversion to get save result from database query
      LOOP AT pending_tags REFERENCE INTO DATA(pending_tag).
        pending_tag->name_upper = to_upper( pending_tag->name ).
        pending_tag->owner      = to_upper( pending_tag->owner ).
      ENDLOOP.

      SELECT * FROM zabaptags_tags
        FOR ALL ENTRIES IN @pending_tags
        WHERE (     name_upper    = @pending_tags-name_upper
                AND owner         = @pending_tags-owner
                AND parent_tag_id = @pending_tags-parent_tag_id )
           OR tag_id = @pending_tags-tag_id
        INTO CORRESPONDING FIELDS OF TABLE @existing_tags.

      " merge existing with to be imported
      LOOP AT pending_tags REFERENCE INTO pending_tag.
        " Find tag by semantic key
        DATA(by_semantic_key) = REF #( existing_tags[ KEY semantic
                                                      name_upper    = pending_tag->name_upper
                                                      owner         = pending_tag->owner
                                                      parent_tag_id = pending_tag->parent_tag_id ] OPTIONAL ).
        IF by_semantic_key IS NOT INITIAL.
          " check if tag id matches as well
          IF by_semantic_key->tag_id = pending_tag->tag_id.
            " check if update is really required
            IF pending_tag->description <> by_semantic_key->description.
              APPEND CORRESPONDING #( pending_tag->* ) TO tags_to_update.
            ENDIF.
          ELSE.
            " we need to update the tag id of the pending tag
            INSERT VALUE #( import_tag_id = pending_tag->tag_id
                            db_tag_id     = by_semantic_key->tag_id ) INTO TABLE db_to_imp_tag_map.
            pending_tag->tag_id = by_semantic_key->tag_id.
            propagate_parent_tag_id( pending_tag->* ).

            " check if update is really required
            IF pending_tag->description <> by_semantic_key->description.
              APPEND CORRESPONDING #( pending_tag->* ) TO tags_to_update.
            ENDIF.
          ENDIF.
        ELSE.
          " check if the tag is already existing with the same UUID, then it is an update
          DATA(by_tag_id) = REF #( existing_tags[ KEY tag_id
                                                  tag_id = pending_tag->tag_id ] OPTIONAL ).
          IF by_tag_id IS NOT INITIAL.
            " definitely an update at this point
            APPEND CORRESPONDING #( pending_tag->* ) TO tags_to_update.
          ELSE.
            " new tag to be imported, as we searched by semantic key already there is not overlap and we can insert it
            IF pending_tag->tag_id IS INITIAL.
              pending_tag->tag_id = cl_system_uuid=>create_uuid_x16_static( ).
              propagate_parent_tag_id( pending_tag->* ).
            ENDIF.
            APPEND CORRESPONDING #( pending_tag->* ) TO tags_to_insert.
          ENDIF.
        ENDIF.

        " collect children if existing
        temp_tags = VALUE #( BASE temp_tags ( LINES OF get_child_tags( pending_tag->* ) ) ).
      ENDLOOP.

      pending_tags = temp_tags.
      CLEAR temp_tags.

    ENDWHILE.

    IF tags_to_insert IS NOT INITIAL.
      DATA(root_mapper) = NEW zcl_abaptags_root_mapper( ).
      LOOP AT tags_to_insert REFERENCE INTO DATA(tag_to_insert).
        root_mapper->collect_parent( tag_id = tag_to_insert->tag_id parent_tag_id = tag_to_insert->parent_tag_id ).
        CLEAR tag_to_insert->admin_data.
        tag_to_insert->created_by = sy-uname.
        GET TIME STAMP FIELD tag_to_insert->created_date_time.
      ENDLOOP.
      root_mapper->map_tags_to_root( ).
      INSERT zabaptags_tags FROM TABLE tags_to_insert.
    ENDIF.

    IF tags_to_update IS NOT INITIAL.
      LOOP AT tags_to_update REFERENCE INTO DATA(tag_to_update).
        tag_to_update->changed_by = sy-uname.
        GET TIME STAMP FIELD tag_to_update->changed_date_time.
      ENDLOOP.

      UPDATE zabaptags_tags FROM TABLE tags_to_update.
    ENDIF.
  ENDMETHOD.

  METHOD import_tagged_objects.
    " Determination of correct TADIR type
    " -------------------------------------
    " Apart from a view exceptions we can just use the first 4 letters of the object or parent object type
    " (exceptions: functions (fugr/ff)

    " Sync tag id - if tags have been imported with new tag id

  ENDMETHOD.

  METHOD import_shared_tags.
    " Sync tag id - if tags have been imported with new tag id

    " Update shared users (should this always be an overwrite???)
  ENDMETHOD.

  METHOD validate_tagged_objects.
    " TODO: collect objects with errors so user knows what to fix
    LOOP AT import_request-tagged_objects REFERENCE INTO DATA(tgobj).
      IF tgobj->tag_id IS INITIAL.
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING textid = zcx_abaptags_adt_error=>tgobj_with_missing_tag_id
                    msgv1  = |[{ tgobj->object_type }]: { tgobj->object_name }|.
      ENDIF.

      IF tgobj->parent_object_name IS NOT INITIAL AND tgobj->parent_tag_id IS INITIAL.
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING textid = zcx_abaptags_adt_error=>tgobj_with_missing_par_tag_id
                    msgv1  = |[{ tgobj->object_type }]: { tgobj->object_name }|
                    msgv2  = |[{ tgobj->parent_object_type }]: { tgobj->parent_object_name }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD propagate_parent_tag_id.
    FIELD-SYMBOLS <child_tags> TYPE zabaptags_tag_data_t.

    CHECK tag-child_tags IS NOT INITIAL.

    ASSIGN tag-child_tags->* TO <child_tags>.

    LOOP AT <child_tags> REFERENCE INTO DATA(child_tag).
      child_tag->parent_tag_id = tag-tag_id.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_child_tags.
    FIELD-SYMBOLS <child_tags> LIKE result.

    CHECK tag-child_tags IS NOT INITIAL.
    ASSIGN tag-child_tags->* TO <child_tags>.
    result = <child_tags>.
  ENDMETHOD.

ENDCLASS.
