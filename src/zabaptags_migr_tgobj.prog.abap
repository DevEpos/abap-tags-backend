*&---------------------------------------------------------------------*
*& Migration Report for database table ZABAPTAGS_TGOBJ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabaptags_migr_tgobj.

CLASS lcl_migrator DEFINITION.

  PUBLIC SECTION.
    METHODS:
      start.
  PROTECTED SECTION.
    METHODS is_migration_necessary
      RETURNING
        VALUE(result) TYPE abap_bool.
  PRIVATE SECTION.
    CONSTANTS: c_package_size TYPE i VALUE 10.
    DATA:
      migrated_count TYPE i.

    METHODS:
      migrate_data,
      print_migration_count,
      filter_existing
        CHANGING
          tagged_objects TYPE zif_abaptags_ty_global=>ty_db_tagged_objects.
ENDCLASS.

CLASS lcl_migrator IMPLEMENTATION.

  METHOD start.
    IF NOT is_migration_necessary( ).
      RETURN.
    ENDIF.

    migrate_data( ).
    print_migration_count( ).
  ENDMETHOD.


  METHOD is_migration_necessary.
    SELECT SINGLE @abap_true
      FROM zabaptags_tgobj
      INTO @result.
  ENDMETHOD.


  METHOD migrate_data.
    DATA: tgobjs_to_migr TYPE zif_abaptags_ty_global=>ty_db_tagged_objects.

    OPEN CURSOR WITH HOLD @DATA(tgobj_curs) FOR
      SELECT tgobj~object_type,
             tgobj~object_name,
             tgobj~tag_id,
             tag~parent_tag_id,
             tgobj~parent_object_type,
             tgobj~parent_object_name,
             tgobj~tagged_by,
             tgobj~tagged_date
        FROM zabaptags_tgobj AS tgobj
          LEFT OUTER JOIN zabaptags_tags AS tag
            ON tgobj~tag_id = tag~tag_id.

    DO.
      FETCH NEXT CURSOR @tgobj_curs
        INTO CORRESPONDING FIELDS OF TABLE @tgobjs_to_migr
        PACKAGE SIZE @c_package_size.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      filter_existing( CHANGING tagged_objects = tgobjs_to_migr ).

      LOOP AT tgobjs_to_migr ASSIGNING FIELD-SYMBOL(<tgobj_to_migr>).
        TRY.
            <tgobj_to_migr>-id = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.
            DELETE tgobjs_to_migr.
            CONTINUE.
        ENDTRY.
      ENDLOOP.

      INSERT zabaptags_tgobjn FROM TABLE tgobjs_to_migr.
      migrated_count = migrated_count + sy-dbcnt.
    ENDDO.

    CLOSE CURSOR @tgobj_curs.

  ENDMETHOD.


  METHOD print_migration_count.
    WRITE: / |{ migrated_count } entries were migrated from ZABAPTAGS_TGOBJ to ZABAPTAGS_TGOBJN|.
  ENDMETHOD.


  METHOD filter_existing.
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

ENDCLASS.

START-OF-SELECTION.
  NEW lcl_migrator( )->start( ).
