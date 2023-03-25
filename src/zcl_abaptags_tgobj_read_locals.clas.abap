"! <p class="shorttext synchronized" lang="en">Reads tagged object infos for local clas, intf, etc.</p>
CLASS zcl_abaptags_tgobj_read_locals DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          object_uri TYPE string,
      run
        RETURNING
          VALUE(result) TYPE zabaptags_tagged_object_t
        RAISING
          cx_adt_uri_mapping.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES BEGIN OF ty_tgobj_info.
    INCLUDE TYPE zif_abaptags_ty_global=>ty_tag_info.
    TYPES parent_tag_name TYPE string.
    TYPES component_name TYPE sobj_name.
    TYPES component_type TYPE swo_objtyp.
    TYPES parent_object_name TYPE sobj_name.
    TYPES parent_object_type TYPE trobjtype.
    TYPES parent_object_uri TYPE string.
    TYPES END OF ty_tgobj_info.

    TYPES:
      ty_tgobj_infos TYPE TABLE OF ty_tgobj_info WITH EMPTY KEY.

    DATA:
      object_uri     TYPE string,
      object_name    TYPE string,
      object_type    TYPE wbobjtype,
      tadir_type     TYPE trobjtype,
      tgobj_infos    TYPE TABLE OF ty_tgobj_info.

    METHODS:
      find_tags_of_of_local_objects
        RETURNING
          VALUE(result) TYPE ty_tgobj_infos,
      find_shared_tags_of_loc_objs
        RETURNING
          VALUE(result) TYPE ty_tgobj_infos,
      find_shared_tags_of_logon_user
        RETURNING
          VALUE(result) TYPE zif_abaptags_ty_global=>ty_tag_id_range,
      read_tags_of_object,
      get_result
        RETURNING
          VALUE(result) TYPE zabaptags_tagged_object_t,
      read_parent_obj_infos,
      get_adt_obj
        IMPORTING
          object_name   TYPE sobj_name
          object_type   TYPE trobjtype
        RETURNING
          VALUE(result) TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.
ENDCLASS.



CLASS zcl_abaptags_tgobj_read_locals IMPLEMENTATION.

  METHOD constructor.
    me->object_uri = object_uri.
  ENDMETHOD.


  METHOD run.
    zcl_abaptags_adt_util=>map_uri_to_wb_object( EXPORTING uri         = object_uri
                                                 IMPORTING object_name = object_name
                                                           tadir_type  = tadir_type
                                                           object_type = object_type ).

    read_tags_of_object( ).
    read_parent_obj_infos( ).

    result = get_result( ).
  ENDMETHOD.


  METHOD find_shared_tags_of_loc_objs.

    DATA(shared_tags_of_logon_user) = find_shared_tags_of_logon_user( ).

    IF shared_tags_of_logon_user IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT
           tag~tag_id,
           tgobj~parent_tag_id,
           tag~owner,
           tag~name,
           parent~name AS parent_tag_name,
           tgobj~component_name,
           tgobj~component_type,
           tgobj~parent_object_name,
           tgobj~parent_object_type
      FROM zabaptags_tgobjn AS tgobj
        INNER JOIN zabaptags_tags AS tag
          ON tgobj~tag_id = tag~tag_id
        LEFT OUTER JOIN zabaptags_tags AS parent
          ON tgobj~parent_tag_id = parent~tag_id
      WHERE tgobj~component_name <> @space
        AND tgobj~tag_id IN @shared_tags_of_logon_user
        AND tgobj~object_name = @object_name
        AND tgobj~object_type = @tadir_type
        AND tag~owner <> @sy-uname
        AND tag~owner <> @space
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD find_shared_tags_of_logon_user.
    DATA: current_usr_shared_tags TYPE RANGE OF zabaptags_tag_id.

    SELECT 'I' AS sign,
           'EQ' AS option,
           tag_id AS low
      FROM zabaptags_shtags
      WHERE shared_user = @sy-uname
      INTO CORRESPONDING FIELDS OF TABLE @current_usr_shared_tags.

    IF sy-subrc = 0.
      result = current_usr_shared_tags.

      " additionally collect all the child tags of the
      " found shared tags
      SELECT 'I' AS sign,
             'EQ' AS option,
             tag_id AS low
        FROM zabaptags_tagsrm
        WHERE root_tag_id IN @current_usr_shared_tags
        APPENDING CORRESPONDING FIELDS OF TABLE @result.
    ENDIF.

  ENDMETHOD.


  METHOD find_tags_of_of_local_objects.
    SELECT DISTINCT
           tag~tag_id,
           tgobj~parent_tag_id,
           tag~owner,
           tag~name,
           parent~name AS parent_tag_name,
           tgobj~component_name,
           tgobj~component_type,
           tgobj~parent_object_name,
           tgobj~parent_object_type
      FROM zabaptags_tgobjn AS tgobj
        INNER JOIN zabaptags_tags AS tag
          ON tgobj~tag_id = tag~tag_id
        LEFT OUTER JOIN zabaptags_tags AS parent
          ON tgobj~parent_tag_id = parent~tag_id
      WHERE tgobj~component_name <> @space
        AND tgobj~object_name = @object_name
        AND tgobj~object_type = @tadir_type
        AND ( tag~owner = @sy-uname OR tag~owner = @space )
      ORDER BY tag~owner, tag~name
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD get_adt_obj.
    CHECK: object_name IS NOT INITIAL,
           object_type IS NOT INITIAL.

    result = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
      tadir_type = object_type
      name       = object_name ).
  ENDMETHOD.


  METHOD get_result.

    DATA(comp_adt_mapper) = NEW zcl_abaptags_comp_adt_mapper( ).
    comp_adt_mapper->add_components( VALUE #(
      FOR <tgobj> IN tgobj_infos ( object_name = object_name
                                   object_type = object_type
                                   component_name = <tgobj>-component_name
                                   component_type = <tgobj>-component_type ) ) ).
    comp_adt_mapper->determine_components( ).

    LOOP AT tgobj_infos ASSIGNING FIELD-SYMBOL(<local_obj_info>)
        GROUP BY ( component_name = <local_obj_info>-component_name
                   component_type = <local_obj_info>-component_type )
        ASSIGNING FIELD-SYMBOL(<local_obj_info_entry>).

      DATA(local_adt_obj_ref) = comp_adt_mapper->get_adt_object( VALUE #(
        object_name    = object_name
        object_type    = object_type-objtype_tr
        component_name = <local_obj_info_entry>-component_name
        component_type = <local_obj_info_entry>-component_type ) ).

      IF local_adt_obj_ref IS INITIAL OR local_adt_obj_ref-uri IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(tagged_local_obj) = VALUE zabaptags_tagged_object(
        adt_obj_ref = VALUE #(
          name        = <local_obj_info_entry>-component_name
          " Type will be changed during mapping to a more generic type so
          " we keep the originally persisted one
          type        = <local_obj_info_entry>-component_type
          parent_name = object_name
          uri         = local_adt_obj_ref-uri
          parent_uri  = object_uri ) ).

      LOOP AT GROUP <local_obj_info_entry> ASSIGNING FIELD-SYMBOL(<local_obj_info_group_entry>).
        DATA(parent) = get_adt_obj(
          object_name = <local_obj_info_group_entry>-parent_object_name
          object_type = <local_obj_info_group_entry>-parent_object_type ).

        tagged_local_obj-tags = VALUE #( BASE tagged_local_obj-tags
          ( tag_id        = <local_obj_info_group_entry>-tag_id
            tag_name      = <local_obj_info_group_entry>-full_hierarchy
            owner         = <local_obj_info_group_entry>-owner
            parent_name   = <local_obj_info_group_entry>-parent_object_name
            parent_type   = parent-type
            parent_tag_id = <local_obj_info_group_entry>-parent_tag_id
            parent_uri    = parent-uri ) ).
      ENDLOOP.

      result = VALUE #( BASE result ( tagged_local_obj ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD read_parent_obj_infos.

    LOOP AT tgobj_infos ASSIGNING FIELD-SYMBOL(<tgobj_info>).
      IF <tgobj_info>-parent_tag_name IS NOT INITIAL.
        <tgobj_info>-full_hierarchy = |{ <tgobj_info>-name } < { <tgobj_info>-parent_tag_name }|.
      ELSE.
        <tgobj_info>-full_hierarchy = <tgobj_info>-name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD read_tags_of_object.
    tgobj_infos = VALUE #(
      ( LINES OF find_tags_of_of_local_objects( ) )
      ( LINES OF find_shared_tags_of_loc_objs( ) ) ).
  ENDMETHOD.

ENDCLASS.
