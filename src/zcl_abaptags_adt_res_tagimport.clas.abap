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
    TYPES  BEGIN OF ty_tgobj_info_enh.
             INCLUDE TYPE zabaptags_tgobj_info.
    TYPES:   object_type_tadir        TYPE trobjtype,
             parent_object_type_tadir TYPE trobjtype.
    TYPES  END OF ty_tgobj_info_enh.

    TYPES ty_tgobj_infos_sorted TYPE TABLE OF ty_tgobj_info_enh WITH EMPTY KEY
      WITH NON-UNIQUE SORTED KEY semkey COMPONENTS tag_id
                                                   object_type_tadir
                                                   object_name
                                                   parent_tag_id
                                                   parent_object_type_tadir
                                                   parent_object_name
                                                   component_type
                                                   component_name.

    DATA db_to_imp_tag_map TYPE HASHED TABLE OF ty_pers_to_imp_tag WITH UNIQUE KEY import_tag_id.
    DATA tgobjs_for_import TYPE ty_tgobj_infos_sorted.
    DATA tgobjs_invalid TYPE ty_tgobj_infos_sorted.
    DATA import_request TYPE zabaptags_data_export.
    DATA objs_tadir_check TYPE REF TO zcl_abaptags_tadir.
    DATA parent_objs_tadir_check TYPE REF TO zcl_abaptags_tadir.
    DATA component_mapper TYPE REF TO zcl_abaptags_comp_adt_mapper.

    METHODS get_request_content_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS import_tags
      RAISING
        cx_uuid_error.

    METHODS import_tagged_objects
      RAISING
        cx_uuid_error.

    METHODS import_shared_tags.

    METHODS propagate_parent_tag_id
      IMPORTING
        tag TYPE zabaptags_tag_data.

    METHODS get_child_tags
      IMPORTING
        tag           TYPE zabaptags_tag_data
      RETURNING
        VALUE(result) TYPE zabaptags_tag_data_t.

    METHODS update_tag_ids
      IMPORTING
        tgobj TYPE REF TO zabaptags_tgobj_info.

    METHODS prepare_tgobj.
    METHODS validate_objects.
    METHODS filter_out_existing.

    METHODS is_parent_obj_valid
      IMPORTING
        tgobj         TYPE REF TO ty_tgobj_info_enh
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS validate_tag_refs.

    METHODS insert_new_tgobj
      RAISING
        cx_uuid_error.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tagimport IMPLEMENTATION.
  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_content_handler( )
                            IMPORTING data            = import_request ).

    TRY.
        import_tags( ).
        import_shared_tags( ).
        import_tagged_objects( ).
      CATCH cx_uuid_error INTO DATA(uuid_error).
        ROLLBACK WORK.
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING previous = uuid_error.
    ENDTRY.
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

  METHOD import_shared_tags.
    DATA shared_tags_to_insert TYPE TABLE OF zabaptags_shtags.

    CHECK import_request-shared_tags IS NOT INITIAL.

    " Sync tag id - if tags have been imported with new tag id
    LOOP AT import_request-shared_tags REFERENCE INTO DATA(shared_tag).
      DATA(updated_tag) = VALUE #( db_to_imp_tag_map[ import_tag_id = shared_tag->tag_id ] OPTIONAL ).
      IF updated_tag IS NOT INITIAL.
        shared_tag->tag_id = updated_tag-db_tag_id.
      ENDIF.

      shared_tags_to_insert = VALUE #( BASE shared_tags_to_insert FOR user IN shared_tag->users
                                       ( tag_id = shared_tag->tag_id shared_user = user-name ) ).
    ENDLOOP.

    INSERT zabaptags_shtags FROM TABLE shared_tags_to_insert ACCEPTING DUPLICATE KEYS.
  ENDMETHOD.

  METHOD import_tagged_objects.
    prepare_tgobj( ).
    validate_objects( ).
    validate_tag_refs( ).
    filter_out_existing( ).
    insert_new_tgobj( ).
  ENDMETHOD.

  METHOD prepare_tgobj.
    DATA obj_keys TYPE zif_abaptags_ty_global=>ty_tadir_keys.
    DATA parent_obj_keys TYPE zif_abaptags_ty_global=>ty_tadir_keys.
    DATA comp_obj_infos TYPE zif_abaptags_ty_global=>ty_local_adt_obj_infos.
    DATA msg TYPE string ##NEEDED.

    LOOP AT import_request-tagged_objects REFERENCE INTO DATA(tgobj_pending).
      IF tgobj_pending->tag_id IS INITIAL.
        MESSAGE e014(zabaptags) WITH |[{ tgobj_pending->object_type }]: { tgobj_pending->object_name }| INTO msg.
        tgobjs_invalid = VALUE #( BASE tgobjs_invalid ( CORRESPONDING #( tgobj_pending->* ) ) ).
        CONTINUE.
      ENDIF.

      IF tgobj_pending->parent_object_name IS NOT INITIAL AND tgobj_pending->parent_tag_id IS INITIAL.
        MESSAGE e015(zabaptags) WITH |[{ tgobj_pending->object_type }]: { tgobj_pending->object_name }|
                                     |[{ tgobj_pending->parent_object_type }]: { tgobj_pending->parent_object_name }| INTO msg.
        tgobjs_invalid = VALUE #( BASE tgobjs_invalid ( CORRESPONDING #( tgobj_pending->* ) ) ).
        CONTINUE.
      ENDIF.

      IF db_to_imp_tag_map IS NOT INITIAL.
        update_tag_ids( tgobj = tgobj_pending ).
      ENDIF.

      DATA(tgobj_for_import) = CORRESPONDING ty_tgobj_info_enh( tgobj_pending->* ).
      tgobj_for_import-object_type_tadir = SWITCH #( tgobj_for_import-object_type
                                                     WHEN zif_abaptags_c_global=>wb_object_types-function
                                                     THEN zif_abaptags_c_global=>object_types-function
                                                     ELSE tgobj_for_import-object_type ).

      obj_keys = VALUE #( BASE obj_keys
                          ( type = tgobj_for_import-object_type_tadir name = tgobj_for_import-object_name ) ).

      IF tgobj_for_import-parent_object_type IS NOT INITIAL.
        tgobj_for_import-parent_object_type_tadir = SWITCH trobjtype( tgobj_for_import-parent_object_type
                                                                      WHEN zif_abaptags_c_global=>wb_object_types-function
                                                                      THEN zif_abaptags_c_global=>object_types-function
                                                                      ELSE tgobj_for_import-parent_object_type ).
        parent_obj_keys = VALUE #(
            BASE parent_obj_keys
            ( type = tgobj_for_import-parent_object_type_tadir name = tgobj_for_import-parent_object_name ) ).
      ENDIF.

      IF tgobj_for_import-component_name IS NOT INITIAL.
        comp_obj_infos = VALUE #( BASE comp_obj_infos
                                  ( object_name    = tgobj_for_import-object_name
                                    object_type    = tgobj_for_import-object_type_tadir
                                    component_name = tgobj_for_import-component_name
                                    component_type = tgobj_for_import-component_type ) ).
      ENDIF.

      tgobjs_for_import = VALUE #( BASE tgobjs_for_import ( tgobj_for_import ) ).
    ENDLOOP.

    CLEAR import_request-tagged_objects.

    objs_tadir_check = NEW zcl_abaptags_tadir( obj_keys )->determine_tadir_entries( ).
    parent_objs_tadir_check = NEW zcl_abaptags_tadir( parent_obj_keys )->determine_tadir_entries( ).
    component_mapper = NEW #( ).
    component_mapper->add_components( comp_obj_infos ).
    component_mapper->determine_components( ).
  ENDMETHOD.

  METHOD validate_objects.
    DATA adt_object_ref TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.

    LOOP AT tgobjs_for_import REFERENCE INTO DATA(tgobj).
      TRY.
          objs_tadir_check->get_tadir_info( name = tgobj->object_name
                                            type = tgobj->object_type_tadir  ).
        CATCH cx_sy_itab_line_not_found.
          tgobjs_invalid = VALUE #( BASE tgobjs_invalid ( tgobj->* ) ).
          DELETE tgobjs_for_import.
          CONTINUE.
      ENDTRY.

      IF tgobj->component_name IS NOT INITIAL.
        adt_object_ref = component_mapper->get_adt_object( VALUE #( object_name    = tgobj->object_name
                                                                    object_type    = tgobj->object_type
                                                                    component_name = tgobj->component_name
                                                                    component_type = tgobj->component_type ) ).

      ELSE.
        adt_object_ref = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type( tadir_type = tgobj->object_type_tadir
                                                                                name       = tgobj->object_name ).
      ENDIF.

      IF adt_object_ref-uri IS INITIAL.
        " No URI means that the TADIR object or the component does not longer exist
        tgobjs_invalid = VALUE #( BASE tgobjs_invalid ( tgobj->* ) ).
        DELETE tgobjs_for_import.
        CONTINUE.
      ENDIF.

      IF tgobj->parent_object_name IS NOT INITIAL AND NOT is_parent_obj_valid( tgobj ).
        tgobjs_invalid = VALUE #( BASE tgobjs_invalid ( tgobj->* ) ).
        DELETE tgobjs_for_import.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_parent_obj_valid.
    result = abap_true.

    IF tgobj->parent_object_name IS INITIAL OR tgobj->parent_object_type IS INITIAL.
      CLEAR tgobj->parent_tag_id. " not required during import/export
      CLEAR tgobj->parent_tag_name.
      RETURN.
    ENDIF.

    TRY.
        parent_objs_tadir_check->get_tadir_info( name = tgobj->parent_object_name
                                                 type = tgobj->parent_object_type_tadir ).
        DATA(adt_object_ref) = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
                                   tadir_type = tgobj->parent_object_type_tadir
                                   name       = tgobj->parent_object_name ).
        IF adt_object_ref IS INITIAL.
          result = abap_false.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        result = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_tag_refs.
    " REVISIT: currently only the existence of the tags is checked, not if the hierarchy is correct
    CHECK tgobjs_for_import IS NOT INITIAL.

    DATA tag_ids TYPE HASHED TABLE OF zabaptags_tag_id WITH UNIQUE KEY table_line.

    SELECT tag_id FROM zabaptags_tags
      FOR ALL ENTRIES IN @tgobjs_for_import
      WHERE tag_id = @tgobjs_for_import-tag_id
         OR tag_id = @tgobjs_for_import-parent_tag_id
      INTO TABLE @tag_ids.

    LOOP AT tgobjs_for_import REFERENCE INTO DATA(tgobj).
      IF NOT line_exists( tag_ids[ table_line = tgobj->tag_id ] ).
        tgobjs_invalid = VALUE #( BASE tgobjs_invalid ( tgobj->* ) ).
        DELETE tgobjs_for_import.
      ENDIF.

      IF tgobj->parent_tag_id IS NOT INITIAL AND NOT line_exists( tag_ids[ table_line = tgobj->parent_tag_id ] ).
        tgobjs_invalid = VALUE #( BASE tgobjs_invalid ( tgobj->* ) ).
        DELETE tgobjs_for_import.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD filter_out_existing.
    DATA existing_entries TYPE ty_tgobj_infos_sorted.

    CHECK tgobjs_for_import IS NOT INITIAL.

    SELECT object_name,
           object_type        AS object_type_tadir,
           tag_id,
           parent_object_name,
           parent_object_type AS parent_object_type_tadir,
           parent_tag_id,
           component_type,
           component_name
      FROM zabaptags_tgobjn
      FOR ALL ENTRIES IN @tgobjs_for_import
      WHERE object_name        = @tgobjs_for_import-object_name
        AND object_type        = @tgobjs_for_import-object_type_tadir
        AND tag_id             = @tgobjs_for_import-tag_id
        AND parent_tag_id      = @tgobjs_for_import-parent_tag_id
        AND parent_object_type = @tgobjs_for_import-parent_object_type_tadir
        AND parent_object_name = @tgobjs_for_import-parent_object_name
        AND component_name     = @tgobjs_for_import-component_name
        AND component_type     = @tgobjs_for_import-component_type
      INTO CORRESPONDING FIELDS OF TABLE @existing_entries.

    IF existing_entries IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT tgobjs_for_import REFERENCE INTO DATA(tgobj).
      IF line_exists( existing_entries[ KEY semkey
                                        tag_id                   = tgobj->tag_id
                                        object_type_tadir        = tgobj->object_type_tadir
                                        object_name              = tgobj->object_name
                                        parent_tag_id            = tgobj->parent_tag_id
                                        parent_object_type_tadir = tgobj->parent_object_type_tadir
                                        parent_object_name       = tgobj->parent_object_name
                                        component_type           = tgobj->component_type
                                        component_name           = tgobj->component_name ] ).
        DELETE tgobjs_for_import.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD insert_new_tgobj.
    DATA tgobjs_db TYPE zif_abaptags_ty_global=>ty_db_tagged_objects.

    CHECK tgobjs_for_import IS NOT INITIAL.

    LOOP AT tgobjs_for_import REFERENCE INTO DATA(tgobj).
      tgobj->id = cl_system_uuid=>create_uuid_x16_static( ).
      tgobjs_db = VALUE #( BASE tgobjs_db
                           ( CORRESPONDING #( tgobj->* MAPPING object_type = object_type_tadir
                                                               parent_object_type = parent_object_type_tadir ) ) ).
    ENDLOOP.

    INSERT zabaptags_tgobjn FROM TABLE tgobjs_db.
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

  METHOD update_tag_ids.
    DATA(db_tag) = VALUE #( db_to_imp_tag_map[ import_tag_id = tgobj->tag_id ] OPTIONAL ).
    IF db_tag IS NOT INITIAL.
      tgobj->tag_id = db_tag-db_tag_id.
    ENDIF.

    IF tgobj->parent_object_name IS NOT INITIAL.
      db_tag = VALUE #( db_to_imp_tag_map[ import_tag_id = tgobj->parent_tag_id ] OPTIONAL ).
      IF db_tag IS NOT INITIAL.
        tgobj->parent_tag_id = db_tag-db_tag_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
