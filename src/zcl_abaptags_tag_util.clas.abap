"! <p class="shorttext synchronized" lang="en">Utility methods for ABAP Tags</p>
CLASS zcl_abaptags_tag_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_s_tag_map,
        tag_id TYPE zabaptags_tag_id,
        data   TYPE REF TO zabaptags_tag_data,
      END OF ty_s_tag_map,
      ty_t_tag_map TYPE HASHED TABLE OF ty_s_tag_map WITH UNIQUE KEY tag_id,
      BEGIN OF ty_s_tag_id,
        tag_id TYPE zabaptags_tag_id,
      END OF ty_s_tag_id,
      ty_t_tag_id TYPE TABLE OF ty_s_tag_id.

    TYPES:
      BEGIN OF ty_s_tag_info,
        tag_id         TYPE zabaptags_tag_id,
        name           TYPE zabaptags_tag_name,
        parent_tag_id  TYPE zabaptags_tag_id,
        owner          TYPE responsibl,
        full_hierarchy TYPE string,
      END OF ty_s_tag_info,
      ty_t_tag_info TYPE STANDARD TABLE OF ty_s_tag_info.

    "! <p class="shorttext synchronized" lang="en">Builds hierarchical tags from flat tags table</p>
    CLASS-METHODS build_hierarchical_tags
      IMPORTING
        it_tags        TYPE zabaptags_tag_data_t
      RETURNING
        VALUE(rt_tags) TYPE zabaptags_tag_data_t.
    "! <p class="shorttext synchronized" lang="en">Unlocks tags</p>
    CLASS-METHODS unlock_tags
      IMPORTING
        iv_lock_owner TYPE uname.
    "! <p class="shorttext synchronized" lang="en">Locks Tags against foreign access</p>
    CLASS-METHODS lock_tags
      IMPORTING
        iv_lock_owner  TYPE uname
        if_global_tags TYPE abap_bool OPTIONAL
      RAISING
        cx_adt_res_no_access.
    "! <p class="shorttext synchronized" lang="en">Retrieves Lock Entry for Tags</p>
    CLASS-METHODS get_tags_lock_entry
      IMPORTING
        iv_owner            TYPE uname
      RETURNING
        VALUE(rv_locked_by) TYPE uname.
    CLASS-METHODS det_hierarchical_tag_names
      CHANGING
        ct_tag_info TYPE ty_t_tag_info.
    "! <p class="shorttext synchronized" lang="en">Determines all child tags for the given tags</p>
    CLASS-METHODS determine_all_child_tags
      IMPORTING
        if_only_tag_id TYPE abap_bool OPTIONAL
      CHANGING
        ct_tags        TYPE zabaptags_tag_data_t.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_abaptags_tag_util IMPLEMENTATION.

  METHOD build_hierarchical_tags.
    DATA: lt_tag_map       TYPE ty_t_tag_map,
          lf_has_map_entry TYPE abap_bool.

    FIELD-SYMBOLS: <ls_parent>   TYPE zabaptags_tag_data,
                   <lt_children> TYPE zabaptags_tag_data_t.

    CHECK it_tags IS NOT INITIAL.

    DATA(lt_tags) = it_tags.

    LOOP AT lt_tags ASSIGNING FIELD-SYMBOL(<ls_tag>) WHERE parent_tag_id IS NOT INITIAL.
      CLEAR: lf_has_map_entry.
      " Find your parent in the current table
      ASSIGN lt_tags[ tag_id = <ls_tag>-parent_tag_id ] TO <ls_parent>.
      IF sy-subrc <> 0.
        " Maybe the tag was already removed and added to the map
        ASSIGN lt_tag_map[ tag_id = <ls_tag>-parent_tag_id ] TO FIELD-SYMBOL(<ls_map_entry>).
        IF sy-subrc <> 0.
          DELETE lt_tags.
        ELSE.
          lf_has_map_entry = abap_true.
          ASSIGN <ls_map_entry>-data->* TO <ls_parent>.
        ENDIF.
      ENDIF.

      IF <ls_parent>-child_tags IS NOT BOUND.
        <ls_parent>-child_tags = NEW zabaptags_tag_data_t( ).
      ENDIF.
      ASSIGN <ls_parent>-child_tags->* TO <lt_children>.

      APPEND INITIAL LINE TO <lt_children> ASSIGNING FIELD-SYMBOL(<ls_child>).
      <ls_child> = <ls_tag>.
      SORT <lt_children> BY name.
      IF lf_has_map_entry = abap_false.
        INSERT VALUE #(
          tag_id = <ls_tag>-tag_id
          data   = REF #( <ls_child> )
        ) INTO TABLE lt_tag_map.
      ENDIF.

      DELETE lt_tags.

      UNASSIGN <ls_parent>.
    ENDLOOP.

    rt_tags = lt_tags.
    SORT rt_tags BY owner name.
  ENDMETHOD.

  METHOD lock_tags.
    DATA: lv_text TYPE string ##needed.

    CALL FUNCTION 'ENQUEUE_EZABAPTAGS_TAG'
      EXPORTING
        _scope         = 1
        owner          = iv_lock_owner
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      DATA(lv_locked_by) = get_tags_lock_entry( iv_lock_owner ).
      IF if_global_tags = abap_true.
        MESSAGE e000(zabaptags) WITH lv_locked_by INTO lv_text.
      ELSE.
        MESSAGE e001(zabaptags) WITH iv_lock_owner lv_locked_by INTO lv_text.
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
        owner  = iv_lock_owner.

  ENDMETHOD.

  METHOD get_tags_lock_entry.
    DATA: lt_enq       TYPE TABLE OF seqg3,
          lv_enq_count TYPE sy-tabix,
          lv_uname     TYPE sy-uname.

    DATA(lv_lock_arg) = CONV eqegraarg( iv_owner ).

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gname                 = 'ZABAPTAGS_TAGS_LOCK'
        garg                  = lv_lock_arg
        guname                = lv_uname
      IMPORTING
        number                = lv_enq_count
      TABLES
        enq                   = lt_enq
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.
    IF sy-subrc = 0 AND lv_enq_count = 1.
      rv_locked_by = lt_enq[ 1 ]-guname.
    ENDIF.
  ENDMETHOD.

  METHOD det_hierarchical_tag_names.
    TYPES:
      BEGIN OF lty_s_tag_parent_info,
        tag_id             TYPE zabaptags_tag_id,
        name               TYPE zabaptags_tag_name,
        parent_tag_id      TYPE zabaptags_tag_id,
        originating_tag_id TYPE zabaptags_tag_id,
        level              TYPE i,
      END OF lty_s_tag_parent_info.

    DATA: lf_tag_with_parent TYPE abap_bool,
          lt_parent_tags     TYPE TABLE OF lty_s_tag_parent_info,
          lt_parent_tags_tmp LIKE lt_parent_tags,
          lt_parent_tags_all LIKE lt_parent_tags.

    FIELD-SYMBOLS: <ls_parent_tag> TYPE lty_s_tag_parent_info,
                   <ls_tag_info>   TYPE zcl_abaptags_tag_util=>ty_s_tag_info.

    LOOP AT ct_tag_info ASSIGNING <ls_tag_info>.
      IF <ls_tag_info>-parent_tag_id IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_parent_tags ASSIGNING <ls_parent_tag>.
        <ls_parent_tag> = CORRESPONDING #( <ls_tag_info> ).
        <ls_parent_tag>-originating_tag_id = <ls_tag_info>-tag_id.
      ELSE.
        <ls_tag_info>-full_hierarchy = <ls_tag_info>-name.
      ENDIF.
    ENDLOOP.

    IF lt_parent_tags IS NOT INITIAL.
      lt_parent_tags_all = lt_parent_tags.
    ENDIF.

    WHILE lt_parent_tags IS NOT INITIAL.
      SELECT tag_id,
             name,
             parent_tag_id
        FROM zabaptags_tags
        FOR ALL ENTRIES IN @lt_parent_tags
        WHERE tag_id = @lt_parent_tags-parent_tag_id
      INTO TABLE @DATA(lt_parent_tag_info).

      LOOP AT lt_parent_tags ASSIGNING <ls_parent_tag>.
        ASSIGN lt_parent_tag_info[ tag_id = <ls_parent_tag>-parent_tag_id ] TO FIELD-SYMBOL(<ls_parent_tag_info>).
        CHECK sy-subrc = 0.

        DATA(ls_parent_tag_info) = VALUE lty_s_tag_parent_info(
          tag_id        = <ls_parent_tag_info>-tag_id
          name          = <ls_parent_tag_info>-name
          parent_tag_id = <ls_parent_tag_info>-parent_tag_id
          originating_tag_id = <ls_parent_tag>-originating_tag_id
          level         = <ls_parent_tag>-level + 1
        ).
        lt_parent_tags_all = VALUE #( BASE lt_parent_tags_all ( ls_parent_tag_info ) ).
        IF ls_parent_tag_info-parent_tag_id IS NOT INITIAL.
          lt_parent_tags_tmp = VALUE #( BASE lt_parent_tags_tmp ( ls_parent_tag_info ) ).
        ENDIF.
      ENDLOOP.

      lt_parent_tags = lt_parent_tags_tmp.
      CLEAR lt_parent_tags_tmp.
    ENDWHILE.

    SORT lt_parent_tags_all BY originating_tag_id level.

    LOOP AT ct_tag_info ASSIGNING <ls_tag_info> WHERE parent_tag_id IS NOT INITIAL.

      LOOP AT lt_parent_tags_all ASSIGNING <ls_parent_tag> WHERE originating_tag_id = <ls_tag_info>-tag_id.
        <ls_tag_info>-full_hierarchy = COND #( WHEN <ls_tag_info>-full_hierarchy IS INITIAL THEN
                                                  <ls_parent_tag>-name
                                               ELSE
*                                                  |{ <ls_parent_tag>-name } > { <ls_tag_info>-full_hierarchy }| ).
                                                  |{ <ls_tag_info>-full_hierarchy } < { <ls_parent_tag>-name }| ).
      ENDLOOP.

    ENDLOOP.
  ENDMETHOD.

  METHOD determine_all_child_tags.
    DATA: lt_new_child_tags TYPE TABLE OF zabaptags_tags.

    DATA(lt_child_tags) = ct_tags.
    DATA(lv_fields) = COND #( WHEN if_only_tag_id = abap_true THEN |TAG_ID| ELSE |*| ).

    WHILE lt_child_tags IS NOT INITIAL.
      SELECT (lv_fields)
        FROM zabaptags_tags
        FOR ALL ENTRIES IN @lt_child_tags
        WHERE parent_tag_id = @lt_child_tags-tag_id
      INTO CORRESPONDING FIELDS OF TABLE @lt_new_child_tags.

      ct_tags = CORRESPONDING #( BASE ( ct_tags ) lt_new_child_tags ).
      lt_child_tags = CORRESPONDING #( lt_new_child_tags ).
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.
