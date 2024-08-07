"! <p class="shorttext synchronized">Reads Tagged Object information about a single object</p>
CLASS zcl_abaptags_tgobj_read_single DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        object_uri TYPE string.

    METHODS run
      RETURNING
        VALUE(result) TYPE zabaptags_tagged_object
      RAISING
        cx_adt_uri_mapping.

  PRIVATE SECTION.
    TYPES BEGIN OF ty_tgobj_info.
            INCLUDE TYPE zif_abaptags_ty_global=>ty_tag_info.
    TYPES   parent_tag_name     TYPE string.
    TYPES   parent_object_name  TYPE sobj_name.
    TYPES   parent_object_type  TYPE trobjtype.
    TYPES   parent_object_uri   TYPE string.
    TYPES   parent_alt_obj_name TYPE string.
    TYPES END OF ty_tgobj_info.

    TYPES ty_tgobj_infos TYPE TABLE OF ty_tgobj_info WITH EMPTY KEY.

    DATA object_uri TYPE string.
    DATA object_name TYPE string.
    DATA alt_object_name TYPE string.
    DATA object_type TYPE wbobjtype.
    DATA tadir_type TYPE trobjtype.
    DATA tgobj_infos TYPE TABLE OF ty_tgobj_info.
    DATA texts TYPE TABLE OF seu_objtxt.
    DATA cds_name_mapper TYPE REF TO zcl_abaptags_cds_name_mapper.

    METHODS find_tags_of_object
      RETURNING
        VALUE(result) TYPE ty_tgobj_infos.

    METHODS find_shared_tags_of_object
      RETURNING
        VALUE(result) TYPE ty_tgobj_infos.

    METHODS find_shared_tags_of_logon_user
      RETURNING
        VALUE(result) TYPE zif_abaptags_ty_global=>ty_tag_id_range.

    METHODS read_tags_of_object.

    METHODS get_result
      RETURNING
        VALUE(result) TYPE zabaptags_tagged_object.

    METHODS read_object_texts.
    METHODS read_parent_obj_infos.

    METHODS get_adt_obj
      IMPORTING
        object_name   TYPE sobj_name
        object_type   TYPE trobjtype
      RETURNING
        VALUE(result) TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.
ENDCLASS.


CLASS zcl_abaptags_tgobj_read_single IMPLEMENTATION.
  METHOD constructor.
    me->object_uri = object_uri.
    cds_name_mapper = NEW #( ).
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
    SELECT DISTINCT tag~tag_id,
                    tgobj~parent_tag_id,
                    tag~owner,
                    tag~name,
                    parent~name              AS parent_tag_name,
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
        AND tgobj~component_name = @space
      ORDER BY tag~owner,
               tag~name
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.

  METHOD find_shared_tags_of_object.
    DATA(shared_tags_of_logon_user) = find_shared_tags_of_logon_user( ).

    IF shared_tags_of_logon_user IS INITIAL.
      RETURN.
    ENDIF.

    SELECT DISTINCT tag~tag_id,
                    tgobj~parent_tag_id,
                    tag~owner,
                    tag~name,
                    parent~name              AS parent_tag_name,
                    tgobj~parent_object_name,
                    tgobj~parent_object_type
      FROM zabaptags_tgobjn AS tgobj
           INNER JOIN zabaptags_tags AS tag
             ON tgobj~tag_id = tag~tag_id
           LEFT OUTER JOIN zabaptags_tags AS parent
             ON tgobj~parent_tag_id = parent~tag_id
      WHERE tgobj~tag_id         IN @shared_tags_of_logon_user
        AND tgobj~object_name     = @object_name
        AND tgobj~object_type     = @tadir_type
        AND tag~owner            <> @sy-uname
        AND tag~owner            <> @space
        AND tgobj~component_name  = @space
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.

  METHOD find_shared_tags_of_logon_user.
    DATA current_usr_shared_tags TYPE RANGE OF zabaptags_tag_id.

    SELECT 'I'    AS sign,
           'EQ'   AS option,
           tag_id AS low
      FROM zabaptags_shtags
      WHERE shared_user = @sy-uname
      INTO CORRESPONDING FIELDS OF TABLE @current_usr_shared_tags.

    IF sy-subrc = 0.
      result = current_usr_shared_tags.

      " additionally collect all the child tags of the
      " found shared tags
      SELECT 'I'    AS sign,
             'EQ'   AS option,
             tag_id AS low
        FROM zabaptags_tagsrm
        WHERE root_tag_id IN @current_usr_shared_tags
        APPENDING CORRESPONDING FIELDS OF TABLE @result.
    ENDIF.
  ENDMETHOD.

  METHOD read_tags_of_object.
    DATA tgobj_for_mapping TYPE TABLE OF REF TO ty_tgobj_info.

    tgobj_infos = VALUE #( ( LINES OF find_tags_of_object( ) )
                           ( LINES OF find_shared_tags_of_object( ) ) ).

    cds_name_mapper->collect_entry( name = object_name
                                    type = object_type-objtype_tr ).

    LOOP AT tgobj_infos REFERENCE INTO DATA(tgobj_info).
      IF cds_name_mapper->collect_entry( name = CONV #( tgobj_info->parent_object_name )
                                         type = tgobj_info->parent_object_type ).
        tgobj_for_mapping = VALUE #( BASE tgobj_for_mapping ( tgobj_info ) ).
      ENDIF.
    ENDLOOP.

    IF cds_name_mapper->map_entries( ) = abap_false.
      RETURN.
    ENDIF.

    alt_object_name = cds_name_mapper->get_display_name( name = object_name
                                                         type = object_type-objtype_tr ).

    LOOP AT tgobj_for_mapping INTO tgobj_info.
      tgobj_info->parent_alt_obj_name = cds_name_mapper->get_display_name(
                                            name = CONV #( tgobj_info->parent_object_name )
                                            type = tgobj_info->parent_object_type ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_result.
    result = VALUE #( adt_obj_ref = VALUE #(
                          name        = object_name
                          alt_name    = alt_object_name
                          description = VALUE #( texts[ object = tadir_type obj_name = object_name  ]-stext OPTIONAL )
                          tadir_type  = object_type
                          type        = COND #(
                            WHEN object_type-subtype_wb <> space
                            THEN |{ object_type-objtype_tr }/{ object_type-subtype_wb }|
                            ELSE object_type )
                          uri         = object_uri )
                      tags        = VALUE #( FOR tag IN tgobj_infos
                                             LET parent = get_adt_obj( object_name = tag-parent_object_name
                                                                       object_type = tag-parent_object_type ) IN
                                             ( tag_id          = tag-tag_id
                                               tag_name        = tag-full_hierarchy
                                               owner           = tag-owner
                                               parent_name     = tag-parent_object_name
                                               parent_alt_name = tag-parent_alt_obj_name
                                               parent_type     = parent-type
                                               parent_tag_id   = tag-parent_tag_id
                                               parent_uri      = parent-uri ) ) ).
  ENDMETHOD.

  METHOD read_object_texts.
    texts = VALUE #( BASE texts ( object = tadir_type obj_name = object_name ) ).

    SORT texts BY object
                  obj_name.
    DELETE ADJACENT DUPLICATES FROM texts COMPARING object obj_name.

    CALL FUNCTION 'RS_SHORTTEXT_GET'
      TABLES obj_tab = texts.
  ENDMETHOD.

  METHOD read_parent_obj_infos.
    LOOP AT tgobj_infos ASSIGNING FIELD-SYMBOL(<tgobj_info>).
      IF <tgobj_info>-parent_object_name IS NOT INITIAL.
        " collect info text read
        texts = VALUE #( BASE texts
                         ( object   = <tgobj_info>-parent_object_type
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

    result = zcl_abaptags_adt_util=>get_adt_obj_ref_for_tadir_type( tadir_type = object_type
                                                                    name       = object_name ).
  ENDMETHOD.
ENDCLASS.
