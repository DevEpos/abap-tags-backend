"! <p class="shorttext synchronized">Resource for deletion check of tagged objects</p>
CLASS zcl_abaptags_adt_res_tgobjdchk DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_tgobj2child_entry,
        id       TYPE zabaptags_i_tgobjn-id,
        child_id TYPE zabaptags_i_tgobjn-id,
      END OF ty_tgobj2child_entry,

      ty_tgobj2child_map TYPE SORTED TABLE OF ty_tgobj2child_entry WITH NON-UNIQUE KEY id child_id.

    TYPES BEGIN OF ty_ext_check_result.
            INCLUDE TYPE zabaptags_tgobj_delchk_object.
    TYPES   not_deletable_dep_count TYPE i.
    TYPES END OF ty_ext_check_result.

    DATA request_info TYPE zabaptags_tgobj_delchk_request.
    DATA check_results TYPE SORTED TABLE OF ty_ext_check_result WITH UNIQUE KEY tagged_object_id.
    DATA undeletable_ids TYPE RANGE OF sysuuid_x16.

    METHODS get_request_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS get_response_handler
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    METHODS prefill_result.

    METHODS check_delete_possibility
      IMPORTING
        object_ids TYPE zabaptags_raw16_t.

    METHODS check_parent_delete_status.
    METHODS fill_error_messages.
ENDCLASS.


CLASS zcl_abaptags_adt_res_tgobjdchk IMPLEMENTATION.
  METHOD post.
    request->get_body_data( EXPORTING content_handler = get_request_handler( )
                            IMPORTING data            = request_info ).
    prefill_result( ).
    check_delete_possibility( request_info-tagged_object_ids ).
    check_parent_delete_status( ).
    fill_error_messages( ).

    response->set_body_data( content_handler = get_response_handler( )
                             data            = CORRESPONDING zabaptags_tgobj_delchk_obj_t( check_results ) ).
  ENDMETHOD.

  METHOD get_request_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                 st_name   = 'ZABAPTAGS_TGOBJ_DELCHK_REQ'
                 root_name = 'REQUEST' ).
  ENDMETHOD.

  METHOD get_response_handler.
    result = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_xml_using_st(
                 st_name   = 'ZABAPTAGS_TGOBJ_DELCHK_RES'
                 root_name = 'CHECK_RESULTS' ).
  ENDMETHOD.

  METHOD prefill_result.
    LOOP AT request_info-tagged_object_ids INTO DATA(id).
      check_results = VALUE #( BASE check_results ( tagged_object_id = id ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_delete_possibility.
    DATA found_objects TYPE ty_tgobj2child_map.

    CHECK object_ids IS NOT INITIAL.

    " 1) find tagged objects including any child objects (necessary for deletion check)
    SELECT tgobj~id,
           child~id AS child_id
      FROM zabaptags_i_tgobjn AS tgobj
           LEFT OUTER JOIN zabaptags_i_tgobjn AS child
             ON  tgobj~tagid         = child~parenttagid
             AND tgobj~objectname    = child~parentobjectname
             AND tgobj~objecttype    = child~parentobjecttype
             " Hint: Components can not have children at this time
             AND tgobj~componentname = @space
      FOR ALL ENTRIES IN @object_ids
      WHERE tgobj~id = @object_ids-table_line
      INTO TABLE @found_objects.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT found_objects REFERENCE INTO DATA(found_obj)
         GROUP BY found_obj->id.

      DATA(checked_obj) = REF #( check_results[ tagged_object_id = found_obj->id ] ).
      DATA(dep_children_not_del_count) = 0.

      LOOP AT GROUP found_obj REFERENCE INTO DATA(found_obj_entry).
        IF found_obj_entry->child_id IS NOT INITIAL.
          " check if it marked for deletion
          IF NOT line_exists( check_results[ tagged_object_id = found_obj_entry->child_id ] ).
            dep_children_not_del_count = dep_children_not_del_count + 1.
          ELSE.
            checked_obj->dependent_object_ids = VALUE #( BASE checked_obj->dependent_object_ids
                                                         ( found_obj_entry->child_id ) ).
          ENDIF.

        ENDIF.
      ENDLOOP.

      IF dep_children_not_del_count = 0.
        checked_obj->is_deletable = abap_true.
      ELSEIF dep_children_not_del_count > 0.
        " check if dependent Ids are deleted as well
        checked_obj->not_deletable_dep_count = dep_children_not_del_count.
        undeletable_ids = VALUE #( BASE undeletable_ids
                                   ( sign = 'I' option = 'EQ' low = checked_obj->tagged_object_id ) ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD check_parent_delete_status.
    WHILE undeletable_ids IS NOT INITIAL.
      DATA(temp_undeletable_ids) = undeletable_ids.
      CLEAR undeletable_ids.

      LOOP AT check_results REFERENCE INTO DATA(check_result) WHERE     is_deletable          = abap_true
                                                                    AND dependent_object_ids IS NOT INITIAL.

        " TODO: variable is assigned but never used (ABAP cleaner)
        LOOP AT check_result->dependent_object_ids INTO DATA(child_id) WHERE table_line IN temp_undeletable_ids.
          check_result->not_deletable_dep_count = check_result->not_deletable_dep_count + 1.
        ENDLOOP.

        IF sy-subrc = 0.
          undeletable_ids = VALUE #( BASE undeletable_ids
                                     ( sign = 'I' option = 'EQ' low = check_result->tagged_object_id ) ).
        ENDIF.

      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.

  METHOD fill_error_messages.
    LOOP AT check_results REFERENCE INTO DATA(checked_obj) WHERE not_deletable_dep_count > 0.
      checked_obj->message      = |Used by { checked_obj->not_deletable_dep_count } child object| &&
                                   COND #( WHEN checked_obj->not_deletable_dep_count > 1 THEN 's' ELSE '' ).
      checked_obj->message_type = zif_abaptags_c_global=>message_types-error.
      checked_obj->is_deletable = abap_false.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
