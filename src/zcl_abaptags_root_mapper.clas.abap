CLASS zcl_abaptags_root_mapper DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        resolve_hierarchies TYPE abap_bool optional.

    METHODS collect_parent
      IMPORTING
        tag_id        TYPE zabaptags_tag_id
        parent_tag_id TYPE zabaptags_tag_id.

    METHODS map_tags_to_root.

  PRIVATE SECTION.
    TYPES ty_tag_ids TYPE STANDARD TABLE OF zabaptags_tag_id WITH EMPTY KEY.

    DATA resolve_hierarchies TYPE abap_bool.
    DATA tags_for_root_mapping TYPE zif_abaptags_ty_global=>ty_tag_with_parent_maps.
    DATA new_map_entries TYPE zif_abaptags_ty_global=>ty_db_tags_root_maps.

    METHODS build_and_collect_root_map
      IMPORTING
        root_tags  TYPE ty_tag_ids
        child_tags TYPE zabaptags_tag_data-child_tags.

    METHODS collect_new_entries.

    METHODS propagate_upper_parents.
ENDCLASS.


CLASS zcl_abaptags_root_mapper IMPLEMENTATION.
  METHOD constructor.
    me->resolve_hierarchies = resolve_hierarchies.
  ENDMETHOD.

  METHOD collect_parent.
    CHECK parent_tag_id IS NOT INITIAL.

    tags_for_root_mapping = VALUE #( BASE tags_for_root_mapping
                                     ( tag_id = tag_id parent_tag_id = parent_tag_id ) ).
  ENDMETHOD.

  METHOD map_tags_to_root.
    CHECK tags_for_root_mapping IS NOT INITIAL.

    propagate_upper_parents( ).
    collect_new_entries( ).

    IF new_map_entries IS NOT INITIAL.
      INSERT zabaptags_tagsrm FROM TABLE new_map_entries ACCEPTING DUPLICATE KEYS.
    ENDIF.
  ENDMETHOD.

  METHOD collect_new_entries.
    IF resolve_hierarchies = abap_true.
      LOOP AT zcl_abaptags_tag_util=>build_hierarchical_tags(
                  tags_flat              = CORRESPONDING #( tags_for_root_mapping )
                  create_missing_parents = abap_true  ) REFERENCE INTO DATA(root_tag).
        build_and_collect_root_map( root_tags  = VALUE #( ( root_tag->tag_id ) )
                                    child_tags = root_tag->child_tags ).
      ENDLOOP.
    ELSE.
      LOOP AT tags_for_root_mapping REFERENCE INTO DATA(tag_for_mapping).
        new_map_entries = VALUE #( BASE new_map_entries
                                   ( root_tag_id = tag_for_mapping->parent_tag_id
                                     tag_id      = tag_for_mapping->tag_id ) ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD build_and_collect_root_map.
    FIELD-SYMBOLS <child_tags> TYPE zabaptags_tag_data_t.

    CHECK child_tags IS NOT INITIAL.

    ASSIGN child_tags->* TO <child_tags>.

    LOOP AT <child_tags> REFERENCE INTO DATA(child_tag).

      LOOP AT root_tags INTO DATA(root_tag_id).
        new_map_entries = VALUE #( BASE new_map_entries
                                   ( root_tag_id = root_tag_id
                                     tag_id      = child_tag->tag_id ) ).
      ENDLOOP.

      build_and_collect_root_map( root_tags  = VALUE #( BASE root_tags
                                                        ( child_tag->tag_id ) )
                                  child_tags = child_tag->child_tags ).
    ENDLOOP.
  ENDMETHOD.

  METHOD propagate_upper_parents.
    SELECT tag_id,
           root_tag_id
      FROM zabaptags_tagsrm
      FOR ALL ENTRIES IN @tags_for_root_mapping
      WHERE tag_id = @tags_for_root_mapping-parent_tag_id
      INTO TABLE @DATA(upper_root_maps).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT tags_for_root_mapping REFERENCE INTO DATA(tag_for_mapping).
      " collect map entries in upper hierarchy levels
      LOOP AT upper_root_maps REFERENCE INTO DATA(upper_root_map) WHERE tag_id = tag_for_mapping->parent_tag_id.
        new_map_entries = VALUE #( BASE new_map_entries
                                   ( root_tag_id = upper_root_map->root_tag_id tag_id = tag_for_mapping->tag_id ) ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
