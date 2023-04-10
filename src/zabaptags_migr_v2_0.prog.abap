*&---------------------------------------------------------------------*
*& Migration Report for ABAP Tags to version v2.0.0
*&---------------------------------------------------------------------*
*&  -> new database table ZABAPTAGS_TGOBJN as successor of ZABAPTAGS_TGOBJ
*&  -> new mapping table ZABAPTAGS_TAGSRM for root node of tree to all child nodes
*&---------------------------------------------------------------------*
REPORT zabaptags_migr_v2_0.

CLASS lcl_migrator DEFINITION.

  PUBLIC SECTION.
    METHODS:
      start.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_package_size TYPE i VALUE 10.

    TYPES:
      ty_tag_ids TYPE TABLE OF zabaptags_tag_id WITH EMPTY KEY.

    DATA:
      migrated_tgobj_count  TYPE i,
      migrated_tagsrm_count TYPE i.

    METHODS:
      is_tgobj_migration_required
        RETURNING
          VALUE(result) TYPE abap_bool,
      is_tagsrm_migration_required
        RETURNING
          VALUE(result) TYPE abap_bool,
      migrate_tgobj,
      migrate_tagsrm,
      print_tgobj_migr_count,
      print_tagsrm_migr_count,
      filter_existing_tgobj
        CHANGING
          tagged_objects TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      build_and_collect_root_map
        IMPORTING
          root_tags     TYPE ty_tag_ids
          child_tags    TYPE zabaptags_tag_data-child_tags
        CHANGING
          tag_root_maps TYPE zif_abaptags_ty_global=>ty_db_tags_root_maps.
ENDCLASS.

CLASS lcl_migrator IMPLEMENTATION.

  METHOD start.
    IF is_tgobj_migration_required( ).
      migrate_tgobj( ).
    ENDIF.

    IF is_tagsrm_migration_required( ).
      migrate_tagsrm( ).
    ENDIF.

    print_tgobj_migr_count( ).
    print_tagsrm_migr_count( ).

  ENDMETHOD.


  METHOD is_tgobj_migration_required.
    SELECT SINGLE @abap_true
      FROM zabaptags_tgobj
      INTO @result.
  ENDMETHOD.


  METHOD is_tagsrm_migration_required.
    SELECT SINGLE @abap_true
      FROM zabaptags_tags AS tag
      WHERE tag~parent_tag_id <> '00000000000000000000000000000000'
        AND NOT EXISTS (
          SELECT @abap_true
            FROM zabaptags_tagsrm
            WHERE root_tag_id = tag~parent_tag_id )
      INTO @result.
  ENDMETHOD.


  METHOD migrate_tgobj.
    DATA: tgobjs_to_migr TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
          tgobj_curs     TYPE cursor.

    OPEN CURSOR WITH HOLD @tgobj_curs FOR
      SELECT tgobj~object_type,
             tgobj~object_name,
             tgobj~tag_id,
             tag~parent_tag_id,
             tgobj~parent_object_type,
             tgobj~parent_object_name,
             tgobj~tagged_by,
             tgobj~tagged_date
        FROM zabaptags_tgobj AS tgobj
          INNER JOIN zabaptags_tags AS tag
            ON tgobj~tag_id = tag~tag_id.

    DO.
      FETCH NEXT CURSOR @tgobj_curs
        INTO CORRESPONDING FIELDS OF TABLE @tgobjs_to_migr
        PACKAGE SIZE @c_package_size.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      filter_existing_tgobj( CHANGING tagged_objects = tgobjs_to_migr ).

      LOOP AT tgobjs_to_migr ASSIGNING FIELD-SYMBOL(<tgobj_to_migr>).
        TRY.
            <tgobj_to_migr>-id = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.
            DELETE tgobjs_to_migr.
            CONTINUE.
        ENDTRY.
      ENDLOOP.

      INSERT zabaptags_tgobjn FROM TABLE tgobjs_to_migr.
      migrated_tgobj_count = migrated_tgobj_count + sy-dbcnt.
    ENDDO.

    CLOSE CURSOR @tgobj_curs.

    " delete entries in old db table
    " TODO: uncomment the next line if all is correct
    DELETE FROM zabaptags_tgobj.
  ENDMETHOD.


  METHOD migrate_tagsrm.
    DATA: tags_flat     TYPE zabaptags_tag_data_t,
          tag_root_maps TYPE zif_abaptags_ty_global=>ty_db_tags_root_maps.

    " 1) read all tags
    SELECT tag_id,
           parent_tag_id
      FROM zabaptags_tags
      INTO CORRESPONDING FIELDS OF TABLE @tags_flat.

    DATA(hier_tags) = zcl_abaptags_tag_util=>build_hierarchical_tags( tags_flat = tags_flat ).

    " 2) non hierarchical tags are not relevant
    DELETE hier_tags WHERE child_tags IS INITIAL.

    LOOP AT hier_tags ASSIGNING FIELD-SYMBOL(<hier_tag>).
      build_and_collect_root_map( EXPORTING root_tags     = VALUE #( ( <hier_tag>-tag_id ) )
                                            child_tags    = <hier_tag>-child_tags
                                  CHANGING  tag_root_maps = tag_root_maps ).
    ENDLOOP.

    IF tag_root_maps IS NOT INITIAL.
      INSERT zabaptags_tagsrm FROM TABLE tag_root_maps ACCEPTING DUPLICATE KEYS.
      migrated_tagsrm_count = sy-dbcnt.
    ENDIF.

  ENDMETHOD.


  METHOD print_tgobj_migr_count.
    IF migrated_tgobj_count > 0.
      WRITE: / |{ migrated_tgobj_count } entries were migrated from ZABAPTAGS_TGOBJ to ZABAPTAGS_TGOBJN|.
    ELSE.
      WRITE: / |Migration for ZABAPTAGS_TGOBJ not necessary|.
    ENDIF.
  ENDMETHOD.


  METHOD print_tagsrm_migr_count.
    IF migrated_tagsrm_count > 0.
      WRITE: / |{ migrated_tagsrm_count } tags were mapped into table ZABAPTAGS_TAGSRM.|.
    ELSE.
      WRITE: / |Migration for ZABAPTAGS_TAGSRM is not necessary|.
    ENDIF.
  ENDMETHOD.


  METHOD filter_existing_tgobj.
    DATA: existing_entries TYPE SORTED TABLE OF zabaptags_tgobjn
            WITH UNIQUE KEY tag_id object_type object_name parent_tag_id parent_object_type parent_object_name.

    IF tagged_objects IS INITIAL.
      RETURN.
    ENDIF.

    SELECT *
      FROM zabaptags_tgobjn
      FOR ALL ENTRIES IN @tagged_objects
      WHERE tag_id = @tagged_objects-tag_id
        AND object_name = @tagged_objects-object_name
        AND object_type = @tagged_objects-object_type
        AND parent_tag_id = @tagged_objects-parent_tag_id
        AND parent_object_type = @tagged_objects-parent_object_type
        AND parent_object_name = @tagged_objects-parent_object_name
      INTO CORRESPONDING FIELDS OF TABLE @existing_entries.

    LOOP AT tagged_objects ASSIGNING FIELD-SYMBOL(<tagged_object>).
      IF line_exists( existing_entries[ tag_id             = <tagged_object>-tag_id
                                        object_type        = <tagged_object>-object_type
                                        object_name        = <tagged_object>-object_name
                                        parent_tag_id      = <tagged_object>-parent_tag_id
                                        parent_object_type = <tagged_object>-parent_object_type
                                        parent_object_name = <tagged_object>-parent_object_name ] ).
        DELETE tagged_objects.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD build_and_collect_root_map.

    FIELD-SYMBOLS: <child_tags> TYPE zabaptags_tag_data_t.

    CHECK child_tags IS NOT INITIAL.

    ASSIGN child_tags->* TO <child_tags>.

    LOOP AT <child_tags> ASSIGNING FIELD-SYMBOL(<child_tag>).

      LOOP AT root_tags INTO DATA(root_tag_id).
        tag_root_maps = VALUE #( BASE tag_root_maps
          ( root_tag_id = root_tag_id
            tag_id      = <child_tag>-tag_id ) ).
      ENDLOOP.

      build_and_collect_root_map(
        EXPORTING
          root_tags     = VALUE #( BASE root_tags ( <child_tag>-tag_id ) )
          child_tags    = <child_tag>-child_tags
        CHANGING
          tag_root_maps = tag_root_maps ).
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.
  NEW lcl_migrator( )->start( ).
