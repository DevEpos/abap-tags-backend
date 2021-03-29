"! <p class="shorttext synchronized" lang="en">Utility methods for ABAP Tags</p>
CLASS zcl_abaptags_tag_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_tag_data,
        tag_id TYPE zabaptags_tag_id,
        data   TYPE REF TO zabaptags_tag_data,
      END OF ty_tag_data,
      ty_tag_data_map TYPE HASHED TABLE OF ty_tag_data WITH UNIQUE KEY tag_id,
      BEGIN OF ty_tag_id,
        tag_id TYPE zabaptags_tag_id,
      END OF ty_tag_id,
      ty_tag_ids TYPE TABLE OF ty_tag_id,
      BEGIN OF ty_tag_info,
        tag_id         TYPE zabaptags_tag_id,
        name           TYPE zabaptags_tag_name,
        parent_tag_id  TYPE zabaptags_tag_id,
        owner          TYPE responsibl,
        full_hierarchy TYPE string,
      END OF ty_tag_info,
      ty_tag_infos TYPE STANDARD TABLE OF ty_tag_info.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Builds hierarchical tags from flat tags table</p>
      build_hierarchical_tags
        IMPORTING
          tags_flat          TYPE zabaptags_tag_data_t
        RETURNING
          VALUE(tags_result) TYPE zabaptags_tag_data_t,
      "! <p class="shorttext synchronized" lang="en">Unlocks tags</p>
      unlock_tags
        IMPORTING
          lock_owner TYPE uname,
      "! <p class="shorttext synchronized" lang="en">Locks Tags against foreign access</p>
      lock_tags
        IMPORTING
          lock_owner  TYPE uname
          global_tags TYPE abap_bool OPTIONAL
        RAISING
          cx_adt_res_no_access,
      "! <p class="shorttext synchronized" lang="en">Retrieves Lock Entry for Tags</p>
      get_tags_lock_entry
        IMPORTING
          owner            TYPE uname
        RETURNING
          VALUE(locked_by) TYPE uname,
      det_hierarchical_tag_names
        CHANGING
          tag_info TYPE ty_tag_infos,
      "! <p class="shorttext synchronized" lang="en">Determines all child tags for the given tags</p>
      determine_all_child_tags
        IMPORTING
          tag_id_only TYPE abap_bool OPTIONAL
        CHANGING
          tags        TYPE zabaptags_tag_data_t.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_abaptags_tag_util IMPLEMENTATION.

  METHOD build_hierarchical_tags.
    DATA: tmp_tag_map   TYPE ty_tag_data_map,
          has_map_entry TYPE abap_bool.

    FIELD-SYMBOLS: <parent>   TYPE zabaptags_tag_data,
                   <children> TYPE zabaptags_tag_data_t.

    CHECK tags_flat IS NOT INITIAL.

    DATA(tags_hier) = tags_flat.

    LOOP AT tags_hier ASSIGNING FIELD-SYMBOL(<tag>) WHERE parent_tag_id IS NOT INITIAL.
      CLEAR: has_map_entry.
      " Find your parent in the current table
      ASSIGN tags_hier[ tag_id = <tag>-parent_tag_id ] TO <parent>.
      IF sy-subrc <> 0.
        " Maybe the tag was already removed and added to the map
        ASSIGN tmp_tag_map[ tag_id = <tag>-parent_tag_id ] TO FIELD-SYMBOL(<map_entry>).
        IF sy-subrc <> 0.
          DELETE tags_hier.
        ELSE.
          has_map_entry = abap_true.
          ASSIGN <map_entry>-data->* TO <parent>.
        ENDIF.
      ENDIF.

      IF <parent>-child_tags IS NOT BOUND.
        <parent>-child_tags = NEW zabaptags_tag_data_t( ).
      ENDIF.
      ASSIGN <parent>-child_tags->* TO <children>.

      APPEND INITIAL LINE TO <children> ASSIGNING FIELD-SYMBOL(<ls_child>).
      <ls_child> = <tag>.
      SORT <children> BY name.
      IF has_map_entry = abap_false.
        INSERT VALUE #(
          tag_id = <tag>-tag_id
          data   = REF #( <ls_child> ) ) INTO TABLE tmp_tag_map.
      ENDIF.

      DELETE tags_hier.

      UNASSIGN <parent>.
    ENDLOOP.

    tags_result = tags_hier.
    SORT tags_result BY owner name.
  ENDMETHOD.

  METHOD lock_tags.
    DATA: text TYPE string ##needed.

    CALL FUNCTION 'ENQUEUE_EZABAPTAGS_TAG'
      EXPORTING
        _scope         = 1
        owner          = lock_owner
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      DATA(locked_by) = get_tags_lock_entry( lock_owner ).
      IF global_tags = abap_true.
        MESSAGE e000(zabaptags) WITH locked_by INTO text.
      ELSE.
        MESSAGE e001(zabaptags) WITH lock_owner locked_by INTO text.
      ENDIF.

      RAISE EXCEPTION TYPE cx_adt_res_no_access
        EXPORTING
          textid = cx_adt_rest=>create_textid_from_msg_params( ).
    ENDIF.
  ENDMETHOD.

  METHOD unlock_tags.

    CALL FUNCTION 'DEQUEUE_EZABAPTAGS_TAG'
      EXPORTING
        _scope = '1'
        owner  = lock_owner.

  ENDMETHOD.

  METHOD get_tags_lock_entry.
    DATA: enq_entries TYPE TABLE OF seqg3,
          enq_count   TYPE sy-tabix,
          uname       TYPE sy-uname.

    DATA(lock_arg) = CONV eqegraarg( owner ).

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gname                 = 'ZABAPTAGS_TAGS_LOCK'
        garg                  = lock_arg
        guname                = uname
      IMPORTING
        number                = enq_count
      TABLES
        enq                   = enq_entries
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
    IF sy-subrc = 0 AND enq_count = 1.
      locked_by = enq_entries[ 1 ]-guname.
    ENDIF.
  ENDMETHOD.

  METHOD det_hierarchical_tag_names.
    TYPES:
      BEGIN OF ty_tag_parent_info,
        tag_id             TYPE zabaptags_tag_id,
        name               TYPE zabaptags_tag_name,
        parent_tag_id      TYPE zabaptags_tag_id,
        originating_tag_id TYPE zabaptags_tag_id,
        level              TYPE i,
      END OF ty_tag_parent_info.

    DATA: parent_tags     TYPE TABLE OF ty_tag_parent_info,
          parent_tags_tmp LIKE parent_tags,
          parent_tags_all LIKE parent_tags.

    FIELD-SYMBOLS: <parent_tag> TYPE ty_tag_parent_info,
                   <tag_info>   TYPE zcl_abaptags_tag_util=>ty_tag_info.

    LOOP AT tag_info ASSIGNING <tag_info>.
      IF <tag_info>-parent_tag_id IS NOT INITIAL.
        APPEND INITIAL LINE TO parent_tags ASSIGNING <parent_tag>.
        <parent_tag> = CORRESPONDING #( <tag_info> ).
        <parent_tag>-originating_tag_id = <tag_info>-tag_id.
      ELSE.
        <tag_info>-full_hierarchy = <tag_info>-name.
      ENDIF.
    ENDLOOP.

    IF parent_tags IS NOT INITIAL.
      parent_tags_all = parent_tags.
    ENDIF.

    WHILE parent_tags IS NOT INITIAL.
      SELECT tag_id,
             name,
             parent_tag_id
        FROM zabaptags_tags
        FOR ALL ENTRIES IN @parent_tags
        WHERE tag_id = @parent_tags-parent_tag_id
        INTO TABLE @DATA(parent_tag_info).

      LOOP AT parent_tags ASSIGNING <parent_tag>.
        ASSIGN parent_tag_info[ tag_id = <parent_tag>-parent_tag_id ] TO FIELD-SYMBOL(<ls_parent_tag_info>).
        CHECK sy-subrc = 0.

        DATA(ls_parent_tag_info) = VALUE ty_tag_parent_info(
          tag_id        = <ls_parent_tag_info>-tag_id
          name          = <ls_parent_tag_info>-name
          parent_tag_id = <ls_parent_tag_info>-parent_tag_id
          originating_tag_id = <parent_tag>-originating_tag_id
          level         = <parent_tag>-level + 1 ).
        parent_tags_all = VALUE #( BASE parent_tags_all ( ls_parent_tag_info ) ).
        IF ls_parent_tag_info-parent_tag_id IS NOT INITIAL.
          parent_tags_tmp = VALUE #( BASE parent_tags_tmp ( ls_parent_tag_info ) ).
        ENDIF.
      ENDLOOP.

      parent_tags = parent_tags_tmp.
      CLEAR parent_tags_tmp.
    ENDWHILE.

    SORT parent_tags_all BY originating_tag_id level.

    LOOP AT tag_info ASSIGNING <tag_info> WHERE parent_tag_id IS NOT INITIAL.

      LOOP AT parent_tags_all ASSIGNING <parent_tag> WHERE originating_tag_id = <tag_info>-tag_id.
        <tag_info>-full_hierarchy = COND #(
          WHEN <tag_info>-full_hierarchy IS INITIAL THEN <parent_tag>-name
          ELSE                                           |{ <tag_info>-full_hierarchy } < { <parent_tag>-name }| ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD determine_all_child_tags.
    DATA: new_child_tags TYPE TABLE OF zabaptags_tags.

    DATA(child_tags) = tags.
    DATA(fields) = COND #( WHEN tag_id_only = abap_true THEN |TAG_ID| ELSE |*| ).

    WHILE child_tags IS NOT INITIAL.
      SELECT (fields)
        FROM zabaptags_tags
        FOR ALL ENTRIES IN @child_tags
        WHERE parent_tag_id = @child_tags-tag_id
        INTO CORRESPONDING FIELDS OF TABLE @new_child_tags.

      tags = CORRESPONDING #( BASE ( tags ) new_child_tags ).
      child_tags = CORRESPONDING #( new_child_tags ).
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
