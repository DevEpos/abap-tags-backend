"! <p class="shorttext synchronized" lang="en">ADT Resource for Tagged Objects</p>
CLASS zcl_abaptags_adt_res_tgobj DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,
      post
        REDEFINITION,
      get
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_actions,
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
      tags_dac          TYPE REF TO zcl_abaptags_tags_dac,
      new_tag_map       TYPE HASHED TABLE OF ty_tag_map WITH UNIQUE KEY tag_name owner,
      tagged_objects    TYPE zabaptags_tagged_object_t,
      tagged_objects_db TYPE zif_abaptags_ty_global=>ty_db_tagged_objects.

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
          cx_adt_rest,
      collect_tgobj_for_insert
        IMPORTING
          tadir_object TYPE string
          tadir_type   TYPE trobjtype
        CHANGING
          tags         TYPE zabaptags_adt_object_tag_t
        RAISING
          cx_adt_rest,
      fill_primary_keys
        RAISING
          zcx_abaptags_adt_error.
ENDCLASS.



CLASS zcl_abaptags_adt_res_tgobj IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    tags_dac = zcl_abaptags_tags_dac=>get_instance( ).
  ENDMETHOD.


  METHOD post.
    DATA(binary_data) = request->get_inner_rest_request( )->get_entity( )->get_binary_data( ).
    IF binary_data IS NOT INITIAL.
      request->get_body_data(
        EXPORTING
          content_handler = get_content_handler( )
        IMPORTING
          data            = tagged_objects ).
    ENDIF.

    CHECK tagged_objects IS NOT INITIAL.

    action_name = zcl_abaptags_adt_request_util=>get_request_param_value(
      param_name = c_params-action
      request    = request ).

    IF action_name IS INITIAL.
      " create/update tags
      create_tagged_objects( ).
    ELSE.
      CASE action_name.

        WHEN c_actions-batch_delete.
          delete_tags_from_objects( ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD get.
    DATA: texts TYPE TABLE OF seu_objtxt.

    DATA(object_uri) = zcl_abaptags_adt_request_util=>get_request_param_value(
      param_name = c_params-object_uri
      mandatory  = abap_true
      request    = request ).

    tagged_objects = VALUE #(
      ( NEW zcl_abaptags_tgobj_read_single( object_uri )->run( ) )
      ( LINES OF NEW zcl_abaptags_tgobj_read_locals( object_uri )->run( ) ) ).

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
    tags_dac->insert_tagged_objects( tagged_objects_db ).
  ENDMETHOD.


  METHOD create_non_persisted_tags.
    DATA: new_tags          TYPE zif_abaptags_ty_global=>ty_db_tags,
          created_date_time TYPE timestampl.

    FIELD-SYMBOLS: <tagged_object> TYPE zabaptags_tagged_object,
                   <tag>           TYPE zabaptags_adt_object_tag.

    ASSIGN tagged_objects[ 1 ] TO <tagged_object>.

    LOOP AT <tagged_object>-tags ASSIGNING <tag> WHERE tag_id IS INITIAL.
      IF sy-tabix = 1.
        GET TIME STAMP FIELD created_date_time.
      ENDIF.

      TRY.
          DATA(tag_id) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
        CATCH cx_uuid_error INTO DATA(uuid_error).
          RAISE EXCEPTION TYPE zcx_abaptags_adt_error
            EXPORTING
              previous = uuid_error.
      ENDTRY.
      new_tag_map = VALUE #( BASE new_tag_map
        ( id       = tag_id
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
    DATA(existing_tag) = tags_dac->find_first_tag_by_tags( new_tags ).

    IF existing_tag IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>tag_already_exists
          msgv1  = |{ existing_tag-name }|
          msgv2  = COND #( WHEN existing_tag-owner IS NOT INITIAL THEN existing_tag-owner ELSE '*' ).
    ENDIF.

    IF NOT tags_dac->insert_tags( new_tags ).
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

      zcl_abaptags_adt_util=>map_uri_to_wb_object( EXPORTING uri         = <tagged_object>-adt_obj_ref-uri
                                                   IMPORTING object_name = tadir_object
                                                             tadir_type  = tadir_type ).

      collect_tgobj_for_insert( EXPORTING tadir_object = tadir_object
                                          tadir_type   = tadir_type
                                CHANGING  tags         = <tagged_object>-tags ).
    ENDLOOP.

    tags_dac->filter_existing_tagged_objects( CHANGING tagged_objects = tagged_objects_db ).
    fill_primary_keys( ).
  ENDMETHOD.


  METHOD fill_primary_keys.

    LOOP AT tagged_objects_db ASSIGNING FIELD-SYMBOL(<tgobj_db_new>).
      TRY.
          <tgobj_db_new>-id = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
          RAISE EXCEPTION TYPE zcx_abaptags_adt_error
            EXPORTING
              textid = zcx_abaptags_adt_error=>tags_persisting_failure.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD delete_tags_from_objects.
    DATA: tadir_object      TYPE string,
          tadir_type        TYPE trobjtype,
          tagged_objects_db TYPE zif_abaptags_ty_global=>ty_db_tagged_objects.

    FIELD-SYMBOLS: <tagged_object> TYPE zabaptags_tagged_object,
                   <tag>           TYPE zabaptags_adt_object_tag.


    LOOP AT tagged_objects ASSIGNING <tagged_object>.

      IF <tagged_object>-adt_obj_ref-uri IS NOT INITIAL.
        zcl_abaptags_adt_util=>map_uri_to_wb_object( EXPORTING uri         = <tagged_object>-adt_obj_ref-uri
                                                     IMPORTING object_name = tadir_object
                                                               tadir_type  = tadir_type ).
      ENDIF.

      LOOP AT <tagged_object>-tags ASSIGNING <tag>.
        IF <tag>-parent_uri IS NOT INITIAL.
          zcl_abaptags_adt_util=>map_uri_to_wb_object( EXPORTING uri         = <tag>-parent_uri
                                                       IMPORTING object_name = DATA(parent_obj_name)
                                                                 tadir_type  = DATA(parent_tadir_type) ).
        ENDIF.

        tagged_objects_db = VALUE #( BASE tagged_objects_db
         ( object_type        = tadir_type
           object_name        = tadir_object
           tag_id             = <tag>-tag_id
           parent_tag_id      = <tag>-parent_tag_id
           parent_object_name = parent_obj_name
           parent_object_type = parent_tadir_type ) ).
      ENDLOOP.

    ENDLOOP.

    NEW zcl_abaptags_tgobj_delete( tagged_objects_db )->run( ).
  ENDMETHOD.


  METHOD validate_tags.
    DATA: existing_tags TYPE zabaptags_tag_data_t,
          tag_id_range  TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    LOOP AT tagged_objects ASSIGNING FIELD-SYMBOL(<tagged_obj>).

      LOOP AT <tagged_obj>-tags ASSIGNING FIELD-SYMBOL(<tag>) WHERE tag_id IS NOT INITIAL.
        tag_id_range = VALUE #( BASE tag_id_range ( sign = 'I' option = 'EQ' low = <tag>-tag_id ) ).
      ENDLOOP.

    ENDLOOP.

    IF tag_id_range IS INITIAL.
      RETURN.
    ENDIF.

    SORT tag_id_range.
    DELETE ADJACENT DUPLICATES FROM tag_id_range.

    existing_tags = tags_dac->find_tags(
      columns      = VALUE #( ( `TAG_ID` ) ( `PARENT_TAG_ID` ) )
      tag_id_range = tag_id_range ).

    IF lines( existing_tags ) <> lines( tag_id_range ).
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>chosen_tags_no_longer_exist.
    ENDIF.

    CLEAR tag_id_range.

    LOOP AT existing_tags ASSIGNING FIELD-SYMBOL(<existing_tag>) WHERE parent_tag_id IS NOT INITIAL.
      tag_id_range = VALUE #( BASE tag_id_range ( sign = 'I' option = 'EQ' low = <existing_tag>-parent_tag_id ) ).
    ENDLOOP.

    IF tag_id_range IS INITIAL.
      RETURN.
    ENDIF.

    SORT tag_id_range.
    DELETE ADJACENT DUPLICATES FROM tag_id_range.

    DATA(tag_count) = tags_dac->count_tags( tag_id_range ).

    IF tag_count <> lines( tag_id_range ).
      RAISE EXCEPTION TYPE zcx_abaptags_adt_error
        EXPORTING
          textid = zcx_abaptags_adt_error=>parents_of_chs_tags_deleted.
    ENDIF.

  ENDMETHOD.


  METHOD collect_tgobj_for_insert.

    DATA: parent_object TYPE string,
          parent_type   TYPE trobjtype.

    FIELD-SYMBOLS <tag> TYPE zabaptags_adt_object_tag.

    LOOP AT tags ASSIGNING <tag>.
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
        zcl_abaptags_adt_util=>map_uri_to_wb_object( EXPORTING uri         = <tag>-parent_uri
                                                     IMPORTING object_name = parent_object
                                                               tadir_type  = parent_type ).
      ENDIF.

      tagged_objects_db = VALUE #( BASE tagged_objects_db
       ( object_type        = tadir_type
         object_name        = tadir_object
         tag_id             = <tag>-tag_id
         parent_tag_id      = <tag>-parent_tag_id
         parent_object_type = parent_type
         parent_object_name = parent_object
         tagged_by          = sy-uname
         tagged_date        = sy-datum ) ).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
