"! <p class="shorttext synchronized">Utility methods for ABAP Tags</p>
CLASS zcl_abaptags_tag_util DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Retrieves shared tags of other users
    CLASS-METHODS get_shared_tags
      IMPORTING
        consider_children_as_shared TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)               TYPE zabaptags_tag_data_t.

    "! Builds hierarchical tags from flat tags table
    CLASS-METHODS build_hierarchical_tags
      IMPORTING
        tags_flat              TYPE zabaptags_tag_data_t
        create_missing_parents TYPE abap_bool OPTIONAL
        ignore_missing_parents TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(tags_result)     TYPE zabaptags_tag_data_t.

    "! Unlocks tags
    CLASS-METHODS unlock_tags
      IMPORTING
        lock_owner TYPE uname.

    "! Locks Tags against foreign access
    CLASS-METHODS lock_tags
      IMPORTING
        lock_owner  TYPE uname
        global_tags TYPE abap_bool OPTIONAL
      RAISING
        cx_adt_res_no_access.

    "! Retrieves Lock Entry for Tags
    CLASS-METHODS get_tags_lock_entry
      IMPORTING
        owner            TYPE uname
      RETURNING
        VALUE(locked_by) TYPE uname.

    CLASS-METHODS det_hierarchical_tag_names
      CHANGING
        tag_info TYPE zif_abaptags_ty_global=>ty_tag_infos.

    "! Determines all child tags for the given tags
    CLASS-METHODS determine_all_child_tags
      IMPORTING
        tag_id_only TYPE abap_bool OPTIONAL
      CHANGING
        tags        TYPE zabaptags_tag_data_t.

    "! Gets adjusted objects types for the object type / parent object type
    "! of the given tagged object
    CLASS-METHODS get_adt_types_for_tgobj
      IMPORTING
        tagged_object      TYPE zif_abaptags_ty_global=>ty_tagged_object
      EXPORTING
        object_type        TYPE swo_objtyp
        parent_object_type TYPE swo_objtyp.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tag_parent_info,
        tag_id             TYPE zabaptags_tag_id,
        name               TYPE zabaptags_tag_name,
        parent_tag_id      TYPE zabaptags_tag_id,
        originating_tag_id TYPE zabaptags_tag_id,
        level              TYPE i,
      END OF ty_tag_parent_info.
ENDCLASS.


CLASS zcl_abaptags_tag_util IMPLEMENTATION.
  METHOD build_hierarchical_tags.
    DATA tmp_tag_map TYPE zif_abaptags_ty_global=>ty_tag_data_map.

    FIELD-SYMBOLS <children> TYPE zabaptags_tag_data_t.

    CHECK tags_flat IS NOT INITIAL.

    DATA(tags_hier) = tags_flat.

    LOOP AT tags_hier REFERENCE INTO DATA(tag) WHERE parent_tag_id IS NOT INITIAL.
      " Find your parent in the current table
      DATA(parent) = REF #( tags_hier[ KEY tag_id
                                       tag_id = tag->parent_tag_id ] OPTIONAL ).
      IF parent IS NOT BOUND.
        " Maybe the tag was already removed and added to the map
        DATA(map_entry) = REF #( tmp_tag_map[ tag_id = tag->parent_tag_id ] OPTIONAL ).
        IF map_entry IS BOUND.
          parent = map_entry->data.
        ELSE.
          IF create_missing_parents = abap_true.
            APPEND INITIAL LINE TO tags_hier REFERENCE INTO parent.
            parent->tag_id = tag->parent_tag_id.
          ELSE.
            IF ignore_missing_parents = abap_false.
              DELETE tags_hier.
            ENDIF.
            CONTINUE.
          ENDIF.
        ENDIF.
      ENDIF.

      IF parent->child_tags IS NOT BOUND.
        parent->child_tags = NEW zabaptags_tag_data_t( ).
      ENDIF.
      ASSIGN parent->child_tags->* TO <children>.

      APPEND INITIAL LINE TO <children> REFERENCE INTO DATA(child).
      child->* = tag->*.
      SORT <children> BY name.
      INSERT VALUE #( tag_id = tag->tag_id
                      data   = child ) INTO TABLE tmp_tag_map.

      DELETE tags_hier.

      CLEAR parent.
    ENDLOOP.

    tags_result = tags_hier.
    SORT tags_result BY owner
                        name.
  ENDMETHOD.

  METHOD det_hierarchical_tag_names.
    DATA parent_tags TYPE TABLE OF ty_tag_parent_info.
    DATA parent_tags_tmp LIKE parent_tags.
    DATA parent_tags_all LIKE parent_tags.

    FIELD-SYMBOLS <parent_tag> TYPE ty_tag_parent_info.
    FIELD-SYMBOLS <tag_info> TYPE zif_abaptags_ty_global=>ty_tag_info.

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

    DATA(tag_db_columns) = VALUE string_table( ( `TAG_ID` ) ( `NAME` ) ( `PARENT_TAG_ID` ) ).

    WHILE parent_tags IS NOT INITIAL.
      DATA(parent_tags_db) = zcl_abaptags_tags_dac=>get_instance( )->find_tags(
                                 columns      = tag_db_columns
                                 tag_id_range = VALUE #( FOR parent IN parent_tags
                                                         ( sign = 'I' option = 'EQ' low = parent-parent_tag_id ) ) ).

      LOOP AT parent_tags ASSIGNING <parent_tag>.
        ASSIGN parent_tags_db[ KEY tag_id
                               tag_id = <parent_tag>-parent_tag_id ] TO FIELD-SYMBOL(<parent_tag_db>).
        CHECK sy-subrc = 0.

        DATA(parent_tag_info) = VALUE ty_tag_parent_info( tag_id             = <parent_tag_db>-tag_id
                                                          name               = <parent_tag_db>-name
                                                          parent_tag_id      = <parent_tag_db>-parent_tag_id
                                                          originating_tag_id = <parent_tag>-originating_tag_id
                                                          level              = <parent_tag>-level + 1 ).
        parent_tags_all = VALUE #( BASE parent_tags_all ( parent_tag_info ) ).
        IF parent_tag_info-parent_tag_id IS NOT INITIAL.
          parent_tags_tmp = VALUE #( BASE parent_tags_tmp ( parent_tag_info ) ).
        ENDIF.
      ENDLOOP.

      parent_tags = parent_tags_tmp.
      CLEAR parent_tags_tmp.
    ENDWHILE.

    SORT parent_tags_all BY originating_tag_id
                            level.

    LOOP AT tag_info ASSIGNING <tag_info> WHERE parent_tag_id IS NOT INITIAL.

      LOOP AT parent_tags_all ASSIGNING <parent_tag> WHERE originating_tag_id = <tag_info>-tag_id.
        <tag_info>-full_hierarchy = COND #(
          WHEN <tag_info>-full_hierarchy IS INITIAL
          THEN <parent_tag>-name
          ELSE |{ <tag_info>-full_hierarchy } < { <parent_tag>-name }| ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD determine_all_child_tags.
    DATA new_child_tags TYPE zabaptags_tag_data_t.

    DATA(child_tags) = tags.
    DATA(columns) = COND string_table( WHEN tag_id_only = abap_true THEN VALUE #( ( `TAG_ID` ) ) ).

    WHILE child_tags IS NOT INITIAL.
      new_child_tags = zcl_abaptags_tags_dac=>get_instance( )->find_tags(
                           columns             = columns
                           parent_tag_id_range = VALUE #( FOR child IN child_tags
                                                          ( sign = 'I' option = 'EQ' low = child-tag_id ) ) ).

      tags = CORRESPONDING #( BASE ( tags ) new_child_tags ).
      child_tags = CORRESPONDING #( new_child_tags ).
    ENDWHILE.
  ENDMETHOD.

  METHOD lock_tags.
    DATA text TYPE string ##NEEDED.

    CALL FUNCTION 'ENQUEUE_EZABAPTAGS_TAG'
      EXPORTING  _scope         = 1
                 owner          = lock_owner
      EXCEPTIONS foreign_lock   = 1
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
        EXPORTING textid = cx_adt_rest=>create_textid_from_msg_params( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_tags_lock_entry.
    DATA enq_entries TYPE TABLE OF seqg3.
    DATA enq_count TYPE sy-tabix.
    DATA uname TYPE sy-uname.

    DATA(lock_arg) = CONV eqegraarg( owner ).

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING  gname                 = 'ZABAPTAGS_TAGS_LOCK'
                 garg                  = lock_arg
                 guname                = uname
      IMPORTING  number                = enq_count
      TABLES     enq                   = enq_entries
      EXCEPTIONS communication_failure = 1
                 system_failure        = 2
                 OTHERS                = 3.
    IF sy-subrc = 0 AND enq_count = 1.
      locked_by = enq_entries[ 1 ]-guname.
    ENDIF.
  ENDMETHOD.

  METHOD get_shared_tags.
    DATA(shared_tags) = zcl_abaptags_tags_dac=>get_instance( )->find_shared_tags( ).
    IF shared_tags IS INITIAL.
      RETURN.
    ENDIF.

    IF consider_children_as_shared = abap_false.
      RETURN.
    ENDIF.

    " also include all child tags of the shared tags
    zcl_abaptags_tag_util=>determine_all_child_tags( CHANGING tags = shared_tags ).
    LOOP AT shared_tags ASSIGNING FIELD-SYMBOL(<shared_tag>).
      <shared_tag>-is_shared        = abap_true.
      <shared_tag>-is_shared_for_me = abap_true.
    ENDLOOP.

    result = shared_tags.
  ENDMETHOD.

  METHOD unlock_tags.
    CALL FUNCTION 'DEQUEUE_EZABAPTAGS_TAG'
      EXPORTING _scope = '1'
                owner  = lock_owner.
  ENDMETHOD.

  METHOD get_adt_types_for_tgobj.
    object_type = zcl_abaptags_adt_util=>get_adt_type_for_object( name = tagged_object-object_name
                                                                  type = tagged_object-object_type ).

    IF     tagged_object-parent_object_type IS NOT INITIAL
       AND tagged_object-parent_object_name IS NOT INITIAL.
      parent_object_type = zcl_abaptags_adt_util=>get_adt_type_for_object( name = tagged_object-parent_object_name
                                                                           type = tagged_object-parent_object_type ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
