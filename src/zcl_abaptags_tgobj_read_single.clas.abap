"! <p class="shorttext synchronized" lang="en">Reads Tagged Object information about a single object</p>
CLASS zcl_abaptags_tgobj_read_single DEFINITION
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
          VALUE(result) TYPE zabaptags_tagged_object
        RAISING
          cx_adt_uri_mapping.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES BEGIN OF ty_tgobj_info.
    INCLUDE TYPE zif_abaptags_ty_global=>ty_tag_info.
    TYPES parent_tag_name TYPE string.
    TYPES parent_object_name TYPE sobj_name.
    TYPES parent_object_type TYPE trobjtype.
    TYPES parent_object_uri TYPE string.
    TYPES END OF ty_tgobj_info.

    TYPES:
      ty_tgobj_infos TYPE TABLE OF ty_tgobj_info WITH EMPTY KEY.

    DATA:
      object_uri    TYPE string,
      object_name   TYPE string,
      object_type   TYPE wbobjtype,
      tadir_type    TYPE trobjtype,
      tgobj_infos   TYPE TABLE OF ty_tgobj_info,
      texts         TYPE TABLE OF seu_objtxt,

      tagged_object TYPE zabaptags_tagged_object.

    METHODS:
      find_tags_of_object
        RETURNING
          VALUE(result) TYPE ty_tgobj_infos,
      find_shared_tags_of_object
        RETURNING
          VALUE(result) TYPE ty_tgobj_infos,
      find_shared_tags_of_logon_user
        RETURNING
          VALUE(result) TYPE zif_abaptags_ty_global=>ty_tag_id_range,
      read_tags_of_object,
      get_result
        RETURNING
          VALUE(result) TYPE zabaptags_tagged_object,
      read_object_texts,
      read_parent_obj_infos,
      get_adt_obj
        IMPORTING
          object_name   TYPE sobj_name
          object_type   TYPE trobjtype
        RETURNING
          VALUE(result) TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.
ENDCLASS.



CLASS zcl_abaptags_tgobj_read_single IMPLEMENTATION.

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
    read_object_texts( ).

    result = get_result( ).
  ENDMETHOD.


  METHOD find_tags_of_object.
    SELECT DISTINCT
           tag~tag_id,
           tgobj~parent_tag_id,
           tag~owner,
           tag~name,
           parent~name AS parent_tag_name,
           tgobj~parent_object_name,
           tgobj~parent_object_type
      FROM zabaptags_tgobjn AS tgobj
        INNER JOIN zabaptags_tags AS tag
          ON tgobj~tag_id = tag~tag_id
        LEFT OUTER JOIN zabaptags_tags AS parent
          ON tgobj~parent_tag_id = parent~tag_id
      WHERE tgobj~object_name = @object_name
        AND tgobj~object_type = @tadir_type
        AND ( tag~owner = @sy-uname OR tag~owner = @space )
      ORDER BY tag~owner, tag~name
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD find_shared_tags_of_object.

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
           tgobj~parent_object_name,
           tgobj~parent_object_type
      FROM zabaptags_tgobjn AS tgobj
        INNER JOIN zabaptags_tags AS tag
          ON tgobj~tag_id = tag~tag_id
        LEFT OUTER JOIN zabaptags_tags AS parent
          ON tgobj~parent_tag_id = parent~tag_id
      WHERE tgobj~tag_id IN @shared_tags_of_logon_user
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

    result = current_usr_shared_tags.

    WHILE current_usr_shared_tags IS NOT INITIAL.
      SELECT tag_id
        FROM zabaptags_tags
        WHERE parent_tag_id IN @current_usr_shared_tags
        INTO TABLE @DATA(child_tags).

      CLEAR current_usr_shared_tags.

      LOOP AT child_tags ASSIGNING FIELD-SYMBOL(<child_tag>).
        current_usr_shared_tags = VALUE #(
          BASE current_usr_shared_tags ( sign = 'I' option = 'EQ' low = <child_tag>-tag_id ) ).
        result = VALUE #(
          BASE result ( sign = 'I' option = 'EQ' low = <child_tag>-tag_id ) ).
      ENDLOOP.
    ENDWHILE.

  ENDMETHOD.


  METHOD read_tags_of_object.
    tgobj_infos = VALUE #(
      ( LINES OF find_tags_of_object( ) )
      ( LINES OF find_shared_tags_of_object( ) ) ).
  ENDMETHOD.


  METHOD get_result.
    result = VALUE #(
      adt_obj_ref = VALUE #(
        name        = object_name
        description = VALUE #( texts[ object = tadir_type obj_name = object_name  ]-stext OPTIONAL )
        tadir_type  = object_type
        type        = COND #(
          WHEN object_type-subtype_wb <> space THEN |{ object_type-objtype_tr }/{ object_type-subtype_wb }| ELSE object_type )
        uri         = object_uri )
      tags = VALUE #(
        FOR tag IN tgobj_infos
        LET parent = get_adt_obj(
          object_name = tag-parent_object_name
          object_type = tag-parent_object_type ) IN
        ( tag_id        = tag-tag_id
          tag_name      = tag-full_hierarchy
          owner         = tag-owner
          parent_name   = tag-parent_object_name
          parent_type   = parent-type
          parent_tag_id = tag-parent_tag_id
          parent_uri    = parent-uri ) ) ).
  ENDMETHOD.


  METHOD read_object_texts.
    texts = VALUE #( BASE texts ( object = tadir_type obj_name = object_name ) ).

    SORT texts BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM texts COMPARING object obj_name.

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES
        obj_tab = texts.
  ENDMETHOD.


  METHOD read_parent_obj_infos.

    LOOP AT tgobj_infos ASSIGNING FIELD-SYMBOL(<tgobj_info>).
      IF <tgobj_info>-parent_object_name IS NOT INITIAL.
        " collect info text read
        texts = VALUE #( BASE texts ( object   = <tgobj_info>-parent_object_type
                                      obj_name = <tgobj_info>-parent_object_name ) ).
      ENDIF.

      IF <tgobj_info>-parent_tag_name IS NOT INITIAL.
        <tgobj_info>-full_hierarchy = |{ <tgobj_info>-name } < { <tgobj_info>-parent_tag_name }|.
      ELSE.
        <tgobj_info>-full_hierarchy = <tgobj_info>-name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_adt_obj.
    CHECK: object_name IS NOT INITIAL,
           object_type IS NOT INITIAL.

    result = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type(
      tadir_type = object_type
      name       = object_name ).
  ENDMETHOD.

ENDCLASS.
