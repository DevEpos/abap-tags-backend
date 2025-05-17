CLASS zcl_abaptags_root_mapper DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS collect_parent
      IMPORTING
        tag_id        TYPE zabaptags_tag_id
        parent_tag_id TYPE zabaptags_tag_id.

    "! Creates mapping for
    METHODS map_tags_to_root.

  PRIVATE SECTION.
    DATA tags_for_root_mapping TYPE zif_abaptags_ty_global=>ty_tag_with_parent_maps.
ENDCLASS.


CLASS zcl_abaptags_root_mapper IMPLEMENTATION.
  METHOD collect_parent.
    CHECK parent_tag_id IS NOT INITIAL.

    tags_for_root_mapping = VALUE #( BASE tags_for_root_mapping
                                     ( tag_id = tag_id parent_tag_id = parent_tag_id ) ).
  ENDMETHOD.

  METHOD map_tags_to_root.
    DATA new_map_entries TYPE zif_abaptags_ty_global=>ty_db_tags_root_maps.

    CHECK tags_for_root_mapping IS NOT INITIAL.

    SELECT tag_id,
           root_tag_id
      FROM zabaptags_tagsrm
      FOR ALL ENTRIES IN @tags_for_root_mapping
      WHERE tag_id = @tags_for_root_mapping-parent_tag_id
      INTO TABLE @DATA(upper_root_maps).

    LOOP AT tags_for_root_mapping ASSIGNING FIELD-SYMBOL(<tag_for_mapping>).
      new_map_entries = VALUE #( BASE new_map_entries
                                 ( tag_id = <tag_for_mapping>-tag_id root_tag_id = <tag_for_mapping>-parent_tag_id ) ).

      " collect map entries in upper hierarchy levels
      LOOP AT upper_root_maps ASSIGNING FIELD-SYMBOL(<upper_root_map>) WHERE tag_id = <tag_for_mapping>-parent_tag_id.
        new_map_entries = VALUE #( BASE new_map_entries
                                   ( root_tag_id = <upper_root_map>-root_tag_id tag_id = <tag_for_mapping>-tag_id ) ).
      ENDLOOP.

    ENDLOOP.

    IF new_map_entries IS NOT INITIAL.
      INSERT zabaptags_tagsrm FROM TABLE new_map_entries ACCEPTING DUPLICATE KEYS.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
