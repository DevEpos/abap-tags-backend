"! <p class="shorttext synchronized" lang="en">Deletes tags from objects</p>
CLASS zcl_abaptags_tgobj_delete DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates new instance of Tgobj. Deletion Handler</p>
      constructor
        IMPORTING
          tagged_object_keys TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      " <p class="shorttext synchronized" lang="en">Runs Tagged Object Deletion</p>
      run.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tgobj_info,
        id          TYPE zabaptags_tgobjn-id,
        tag_id      TYPE zabaptags_tgobjn-tag_id,
        object_name TYPE zabaptags_tgobjn-object_name,
        object_type TYPE zabaptags_tgobjn-object_type,
      END OF ty_tgobj_info,

      BEGIN OF ty_tag_info,
        tag_id        TYPE zabaptags_tag_id,
        parent_tag_id TYPE zabaptags_tag_id,
      END OF ty_tag_info.

    DATA:
      tagged_object_keys  TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      full_tgobj_keys     TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      tag_id_obj_keys     TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      child_tgobj_entries TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      tag_id_range_keys   TYPE RANGE OF zabaptags_tag_id,
      tag_parent_infos    TYPE SORTED TABLE OF ty_tag_info WITH UNIQUE KEY tag_id parent_tag_id,
      tgobj_infos         TYPE STANDARD TABLE OF ty_tgobj_info,
      tgobj_update        TYPE zif_abaptags_ty_global=>ty_db_tagged_objects.

    METHODS:
      fill_semantic_key_tables,
      select_tagged_object_infos,
      find_dependent_children,
      delete_tagged_objects,
      select_tag_infos,
      process_dependent_children,
      update_dependent_objects.
ENDCLASS.



CLASS zcl_abaptags_tgobj_delete IMPLEMENTATION.

  METHOD constructor.
    me->tagged_object_keys = tagged_object_keys.
  ENDMETHOD.


  METHOD run.
    fill_semantic_key_tables( ).
    select_tagged_object_infos( ).
    find_dependent_children( ).
    select_tag_infos( ).
    process_dependent_children( ).
    update_dependent_objects( ).
    delete_tagged_objects( ).

    COMMIT WORK.
  ENDMETHOD.


  METHOD fill_semantic_key_tables.

    LOOP AT tagged_object_keys ASSIGNING FIELD-SYMBOL(<tgobj>).
      IF <tgobj>-parent_object_name IS NOT INITIAL AND
          <tgobj>-parent_object_type IS NOT INITIAL AND
          <tgobj>-parent_tag_id IS NOT INITIAL.
        full_tgobj_keys = VALUE #( BASE full_tgobj_keys ( <tgobj> ) ).
      ELSEIF <tgobj>-object_type IS NOT INITIAL AND
          <tgobj>-object_name IS NOT INITIAL.
        tag_id_obj_keys = VALUE #( BASE tag_id_obj_keys ( <tgobj> ) ).
      ELSE.
        tag_id_range_keys = VALUE #( BASE tag_id_range_keys ( sign = 'I' option = 'EQ' low = <tgobj>-tag_id ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD select_tagged_object_infos.
    " a) Collect the keys by tag / object identifier
    IF tag_id_obj_keys IS NOT INITIAL.
      SELECT id, tag_id, object_name, object_type
        FROM zabaptags_tgobjn
        FOR ALL ENTRIES IN @tag_id_obj_keys
        WHERE object_name = @tag_id_obj_keys-object_name
          AND object_type = @tag_id_obj_keys-object_type
          AND tag_id      = @tag_id_obj_keys-tag_id
        APPENDING TABLE @tgobj_infos.
    ENDIF.

    " b) Collect the keys by full semantic key
    IF full_tgobj_keys IS NOT INITIAL.
      SELECT id, tag_id, object_name, object_type
        FROM zabaptags_tgobjn
        FOR ALL ENTRIES IN @full_tgobj_keys
        WHERE object_name        = @full_tgobj_keys-object_name
          AND object_type        = @full_tgobj_keys-object_type
          AND tag_id             = @full_tgobj_keys-tag_id
          AND parent_object_name = @full_tgobj_keys-parent_object_name
          AND parent_object_type = @full_tgobj_keys-parent_object_type
          AND parent_tag_id      = @full_tgobj_keys-parent_tag_id
        APPENDING TABLE @tgobj_infos.
    ENDIF.

    " c) Collect the keys by tag id only
    IF tag_id_range_keys IS NOT INITIAL.
      SELECT id, tag_id, object_name, object_type
        FROM zabaptags_tgobjn
        WHERE tag_id IN @tag_id_range_keys
        APPENDING TABLE @tgobj_infos.
    ENDIF.
  ENDMETHOD.


  METHOD find_dependent_children.
    CHECK tgobj_infos IS NOT INITIAL.

    SELECT *
      FROM zabaptags_tgobjn
      FOR ALL ENTRIES IN @tgobj_infos
      WHERE (     parent_tag_id      = @tgobj_infos-tag_id
              AND parent_object_name = @tgobj_infos-object_name
              AND parent_object_type = @tgobj_infos-object_type )
         OR (     parent_tag_id      = @tgobj_infos-tag_id
              AND parent_object_name = @space
              AND parent_object_type = @space )
      INTO CORRESPONDING FIELDS OF TABLE @child_tgobj_entries.
  ENDMETHOD.


  METHOD select_tag_infos.
    CHECK child_tgobj_entries IS NOT INITIAL.

    SELECT tag_id,parent_tag_id
      FROM zabaptags_tags
      FOR ALL ENTRIES IN @child_tgobj_entries
      WHERE tag_id = @child_tgobj_entries-tag_id
      INTO CORRESPONDING FIELDS OF TABLE @tag_parent_infos.
  ENDMETHOD.


  METHOD process_dependent_children.

    LOOP AT child_tgobj_entries ASSIGNING FIELD-SYMBOL(<child_tgobj>)
        GROUP BY ( tag_id      = <child_tgobj>-tag_id
                   object_name = <child_tgobj>-object_name
                   object_type = <child_tgobj>-object_type
                   count       = GROUP SIZE )
        ASSIGNING FIELD-SYMBOL(<child_tgobj_group>).

      DATA(counter) = 1.

      LOOP AT GROUP <child_tgobj_group> ASSIGNING FIELD-SYMBOL(<child_tgobj_group_entry>).

        IF counter > 1.
          tgobj_infos = VALUE #( BASE tgobj_infos ( id = <child_tgobj_group_entry>-id ) ).
        ELSE.
          CLEAR: <child_tgobj_group_entry>-parent_object_name,
                 <child_tgobj_group_entry>-parent_object_type.
          " reset parent tag id to direct parent tag, or clear the value if the parent cannot be found
          DATA(new_parent_tag_id) = VALUE #(
            tag_parent_infos[ tag_id = <child_tgobj_group_entry>-tag_id ]-parent_tag_id OPTIONAL ).
          IF <child_tgobj_group_entry>-parent_tag_id <> new_parent_tag_id.
            <child_tgobj_group_entry>-parent_tag_id = new_parent_tag_id.
            tgobj_update = VALUE #( BASE tgobj_update ( <child_tgobj_group_entry> ) ).
          ENDIF.
        ENDIF.

        counter = counter + 1.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD update_dependent_objects.
    CHECK tgobj_update IS NOT INITIAL.

    MODIFY zabaptags_tgobjn FROM TABLE tgobj_update.
  ENDMETHOD.


  METHOD delete_tagged_objects.
    CHECK tgobj_infos IS NOT INITIAL.

    DELETE zabaptags_tgobjn FROM TABLE tgobj_infos.
  ENDMETHOD.

ENDCLASS.
