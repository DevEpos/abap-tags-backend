"! <p class="shorttext synchronized" lang="en">Tag DB Access</p>
CLASS zcl_abaptags_tags_dac DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Returns current instance of Tag Repository</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_abaptags_tags_dac.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Converts user tags to global tags</p>
      convert_tags_to_global
        IMPORTING
          tag_ids TYPE zif_abaptags_ty_global=>ty_tag_id_range,
      "! <p class="shorttext synchronized" lang="en">Counts tags by given criteria</p>
      count_tags
        IMPORTING
          tag_id_range  TYPE zif_abaptags_ty_global=>ty_tag_id_range
        RETURNING
          VALUE(result) TYPE i,
      "! <p class="shorttext synchronized" lang="en">Unshares list of tags</p>
      delete_shared_tags_by_id
        IMPORTING
          tag_ids            TYPE zif_abaptags_ty_global=>ty_tag_id_range
          unshare_completely TYPE abap_bool OPTIONAL,
      "! <p class="shorttext synchronized" lang="en">Delete user/tag id combinations from shared tags DB</p>
      delete_shared_tags
        IMPORTING
          shared_tags_db TYPE zif_abaptags_ty_global=>ty_db_shared_tags OPTIONAL,
      "! <p class="shorttext synchronized" lang="en">Deletes tagged objects from DB</p>
      delete_tagged_objects
        IMPORTING
          tagged_objects TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      "! <p class="shorttext synchronized" lang="en">Deletes tag db entries by id range</p>
      delete_tag_by_id
        IMPORTING
          id_range TYPE zif_abaptags_ty_global=>ty_tag_id_range,
      "! <p class="shorttext synchronized" lang="en">Checks if tag exists for given parameters</p>
      exists_tag
        IMPORTING
          tag_id_range        TYPE zif_abaptags_ty_global=>ty_tag_id_range OPTIONAL
          parent_tag_id_range TYPE zif_abaptags_ty_global=>ty_tag_id_range OPTIONAL
          owner_range         TYPE zif_abaptags_ty_global=>ty_owner_range OPTIONAL
          name_upper_range    TYPE zif_abaptags_ty_global=>ty_tag_name_range OPTIONAL
        RETURNING
          VALUE(result)       TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Finds first matching global tag</p>
      find_first_global_tag
        IMPORTING
          name_upper_range TYPE zif_abaptags_ty_global=>ty_tag_name_range OPTIONAL
        RETURNING
          VALUE(result)    TYPE zabaptags_tag_data,
      "! <p class="shorttext synchronized" lang="en">Finds the first tag from the list of given tags</p>
      find_first_tag_by_tags
        IMPORTING
          tags          TYPE zif_abaptags_ty_global=>ty_db_tags
        RETURNING
          VALUE(result) TYPE zabaptags_tags,
      "! <p class="shorttext synchronized" lang="en">Finds shared tags</p>
      "! This method will return only tags that are shared for the current
      "! sy-uname user
      find_shared_tags
        RETURNING
          VALUE(result) TYPE zabaptags_tag_data_t,
      "! <p class="shorttext synchronized" lang="en">Finds users of a shared tag</p>
      find_shared_tag_users
        IMPORTING
          tag_id        TYPE zabaptags_tag_id
        RETURNING
          VALUE(result) TYPE zabaptags_user_t,
      "! <p class="shorttext synchronized" lang="en">Finds a list of tags that are shared</p>
      find_shared_tags_db
        IMPORTING
          tag_ids       TYPE zif_abaptags_ty_global=>ty_tag_id_range
        RETURNING
          VALUE(result) TYPE zif_abaptags_ty_global=>ty_db_shared_tags,
      "! <p class="shorttext synchronized" lang="en">Finds tags for the given filters</p>
      find_tags
        IMPORTING
          single_select       TYPE abap_bool OPTIONAL
          columns             TYPE string_table OPTIONAL
          owner_range         TYPE zif_abaptags_ty_global=>ty_owner_range OPTIONAL
          tag_id_range        TYPE zif_abaptags_ty_global=>ty_tag_id_range OPTIONAL
          name_upper_range    TYPE zif_abaptags_ty_global=>ty_tag_name_range OPTIONAL
          parent_tag_id_range TYPE zif_abaptags_ty_global=>ty_tag_id_range OPTIONAL
        RETURNING
          VALUE(result)       TYPE zabaptags_tag_data_t,
      "! <p class="shorttext synchronized" lang="en">Finds tags of tadir object</p>
      find_tags_of_object
        IMPORTING
          tadir_obj     TYPE zif_abaptags_ty_global=>ty_tadir_key
        RETURNING
          VALUE(result) TYPE zif_abaptags_ty_global=>ty_tag_infos,
      "! <p class="shorttext synchronized" lang="en">Finds shared tags of tadir object</p>
      find_shared_tags_of_object
        IMPORTING
          tadir_obj     TYPE zif_abaptags_ty_global=>ty_tadir_key
        RETURNING
          VALUE(result) TYPE zif_abaptags_ty_global=>ty_tag_infos,
      "! <p class="shorttext synchronized" lang="en">Finds list of tagged objects</p>
      find_tagged_objects
        IMPORTING
          only_matching_all_tag    TYPE abap_bool OPTIONAL
          tag_count                TYPE i OPTIONAL
          max_results              TYPE i DEFAULT 50
          tag_id_range             TYPE zif_abaptags_ty_global=>ty_tag_id_range OPTIONAL
          object_name_range        TYPE zif_abaptags_ty_global=>ty_obj_name_range OPTIONAL
          object_type_range        TYPE zif_abaptags_ty_global=>ty_obj_type_range OPTIONAL
          parent_object_name_range TYPE zif_abaptags_ty_global=>ty_obj_name_range OPTIONAL
          parent_object_type_range TYPE zif_abaptags_ty_global=>ty_obj_type_range OPTIONAL
        RETURNING
          VALUE(result)            TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      "! <p class="shorttext synchronized" lang="en">Get info about tagged objects</p>
      get_tagged_obj_info
        IMPORTING
          tagged_objects TYPE zif_abaptags_ty_global=>ty_db_tagged_objects
          tag_id_range   TYPE zif_abaptags_ty_global=>ty_tag_id_range OPTIONAL
        RETURNING
          VALUE(result)  TYPE zif_abaptags_ty_global=>ty_tgobj_infos,
      "! <p class="shorttext synchronized" lang="en">Get info about children of tagged objects</p>
      get_children_of_tagged_objects
        IMPORTING
          parent_tag_ids TYPE zif_abaptags_ty_global=>ty_tag_id_range
        RETURNING
          VALUE(result)  TYPE zif_abaptags_ty_global=>ty_tgobj_child_infos,
      "! <p class="shorttext synchronized" lang="en">Retrieves tag counts for object refs</p>
      get_tagged_obj_count
        IMPORTING
          tag_ids       TYPE zif_abaptags_ty_global=>ty_tag_id_range OPTIONAL
          object_refs   TYPE zabaptags_adt_obj_ref_t OPTIONAL
        RETURNING
          VALUE(result) TYPE zif_abaptags_ty_global=>ty_tag_counts,
      "! <p class="shorttext synchronized" lang="en">Inserts new tagged objects into DB</p>
      insert_tagged_objects
        IMPORTING
          new_tagged_objects TYPE zif_abaptags_ty_global=>ty_db_tagged_objects,
      "! <p class="shorttext synchronized" lang="en">Inserts new tags into DB</p>
      insert_tags
        IMPORTING
          tags          TYPE zif_abaptags_ty_global=>ty_db_tags
        RETURNING
          VALUE(result) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Modifies tags table from list of tag data</p>
      modify_tags
        IMPORTING
          tags TYPE zif_abaptags_ty_global=>ty_db_tags,
      "! <p class="shorttext synchronized" lang="en">Share the list of given tags</p>
      share_tags
        IMPORTING
          tags_to_share TYPE zif_abaptags_ty_global=>ty_db_shared_tags
          tag_ids       TYPE zif_abaptags_ty_global=>ty_tag_id_range OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_initial_tag_id TYPE zabaptags_tag_id VALUE 0,
      c_where_and      TYPE string VALUE ` AND `,
      c_where_or       TYPE string VALUE ` OR `.
    CLASS-DATA:
      instance TYPE REF TO zcl_abaptags_tags_dac.
ENDCLASS.



CLASS zcl_abaptags_tags_dac IMPLEMENTATION.


  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.
    result = instance.
  ENDMETHOD.


  METHOD convert_tags_to_global.
    DATA: changed_datetime TYPE timestampl.

    CHECK tag_ids IS NOT INITIAL.

    GET TIME STAMP FIELD changed_datetime.

    UPDATE zabaptags_tags
      SET owner             = @space,
          changed_by        = @sy-uname,
          changed_date_time = @changed_datetime
      WHERE tag_id IN @tag_ids.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD count_tags.
    SELECT COUNT(*)
      FROM zabaptags_tags
      WHERE tag_id IN @tag_id_range
      INTO @result.
  ENDMETHOD.


  METHOD delete_shared_tags_by_id.
    DATA: changed_date_time TYPE timestampl.

    CHECK tag_ids IS NOT INITIAL.

    DELETE FROM zabaptags_shtags WHERE tag_id IN @tag_ids.
    IF sy-subrc = 0.
      IF unshare_completely = abap_true.
        GET TIME STAMP FIELD changed_date_time.
        " remove 'is_shared' property of tags in masterdata table
        UPDATE zabaptags_tags
          SET is_shared = @abap_false,
              changed_date_time = @changed_date_time,
              changed_by = @sy-uname
          WHERE tag_id IN @tag_ids.
      ENDIF.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_shared_tags.
    CHECK shared_tags_db IS NOT INITIAL.

    DELETE zabaptags_shtags FROM TABLE shared_tags_db.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_tagged_objects.
    CHECK tagged_objects IS NOT INITIAL.
    DELETE zabaptags_tgobj FROM TABLE tagged_objects.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_tag_by_id.
    CHECK id_range IS NOT INITIAL.

    DELETE FROM zabaptags_tgobj WHERE tag_id IN id_range.
    DELETE FROM zabaptags_tags WHERE tag_id IN id_range.
    DELETE FROM zabaptags_shtags WHERE tag_id IN id_range.

    COMMIT WORK.
  ENDMETHOD.


  METHOD exists_tag.
    IF tag_id_range IS INITIAL
        AND owner_range IS INITIAL
        AND name_upper_range IS INITIAL
        AND parent_tag_id_range IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE @abap_true
      FROM zabaptags_tags
      WHERE tag_id IN @tag_id_range
        AND owner IN @owner_range
        AND name_upper IN @name_upper_range
        AND parent_tag_id IN @parent_tag_id_range
      INTO @result.
  ENDMETHOD.


  METHOD find_first_global_tag.
    SELECT SINGLE name
      FROM zabaptags_tags
      WHERE owner = @space
        AND parent_tag_id = @c_initial_tag_id
        AND name_upper IN @name_upper_range
      INTO CORRESPONDING FIELDS OF @result.
  ENDMETHOD.


  METHOD find_first_tag_by_tags.
    IF tags IS INITIAL.
      RETURN.
    ENDIF.

    SELECT tag_id,
           name,
           owner
      FROM zabaptags_tags
      FOR ALL ENTRIES IN @tags
      WHERE name_upper = @tags-name_upper
        AND owner      = @tags-owner
      INTO TABLE @DATA(single_result)
      UP TO 1 ROWS.

    IF sy-subrc = 0.
      result = CORRESPONDING #( single_result[ 1 ] ).
    ENDIF.
  ENDMETHOD.


  METHOD find_shared_tags.
    SELECT DISTINCT tag~*
      FROM zabaptags_shtags AS shared_tag
      INNER JOIN zabaptags_tags AS tag
        ON  shared_tag~tag_id = tag~tag_id
        AND shared_tag~shared_user = @sy-uname
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD find_shared_tag_users.
    SELECT shared_user
      FROM zabaptags_shtags
      WHERE tag_id = @tag_id
      INTO TABLE @result.
  ENDMETHOD.


  METHOD find_shared_tags_db.
    SELECT *
      FROM zabaptags_shtags
      WHERE tag_id IN @tag_ids
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD find_tags.
    DATA: single_result TYPE zabaptags_tag_data.

    DATA(l_columns) = COND string(
      WHEN columns IS INITIAL THEN `*`
      ELSE REDUCE #(
        INIT cols = `` sep = ``
        FOR col IN columns
        NEXT cols = cols && sep && col sep = `, ` ) ).

    IF single_select = abap_true.
      SELECT SINGLE (l_columns)
        FROM zabaptags_tags
        WHERE tag_id IN @tag_id_range
          AND owner IN @owner_range
          AND name_upper IN @name_upper_range
          AND parent_tag_id IN @parent_tag_id_range
        INTO CORRESPONDING FIELDS OF @single_result.
      IF sy-subrc = 0.
        result = VALUE #( ( single_result ) ).
      ENDIF.
    ELSE.
      SELECT (l_columns)
        FROM zabaptags_tags
        WHERE tag_id IN @tag_id_range
          AND owner IN @owner_range
          AND name_upper IN @name_upper_range
          AND parent_tag_id IN @parent_tag_id_range
        INTO CORRESPONDING FIELDS OF TABLE @result.
    ENDIF.
  ENDMETHOD.


  METHOD find_tags_of_object.
    SELECT DISTINCT
           tag~tag_id,
           tag~parent_tag_id,
           tag~owner,
           tag~name
      FROM zabaptags_tgobj AS tgobj
        INNER JOIN zabaptags_tags AS tag
          ON tgobj~tag_id = tag~tag_id
      WHERE tgobj~object_name = @tadir_obj-name
        AND tgobj~object_type = @tadir_obj-type
        AND ( tag~owner = @sy-uname OR tag~owner = @space )
      ORDER BY owner, name
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD find_shared_tags_of_object.
    SELECT DISTINCT
           tag~tag_id,
           tag~parent_tag_id,
           tag~owner,
           tag~name
      FROM zabaptags_tgobj AS tgobj
        INNER JOIN zabaptags_tags AS tag
          ON tgobj~tag_id = tag~tag_id
        INNER JOIN zabaptags_shtags AS shared_tag
          ON tag~tag_id = shared_tag~tag_id
      WHERE tgobj~object_name = @tadir_obj-name
        AND tgobj~object_type = @tadir_obj-type
        AND tag~owner <> @sy-uname
        AND tag~owner <> @space
        AND tag~is_shared = @abap_true
        AND shared_tag~shared_user = @sy-uname
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD find_tagged_objects.
    IF only_matching_all_tag = abap_true
         AND tag_count > 0.
      SELECT object_name, object_type
        FROM zabaptags_tgobj
        WHERE tag_id IN @tag_id_range
          AND object_name IN @object_name_range
          AND object_type IN @object_type_range
          AND parent_object_name IN @parent_object_name_range
          AND parent_object_type IN @parent_object_type_range
        GROUP BY object_name, object_type
        HAVING COUNT(*) = @tag_count
        ORDER BY object_type, object_name
        INTO CORRESPONDING FIELDS OF TABLE @result
        UP TO @max_results ROWS.
    ELSE.
      SELECT DISTINCT object_name, object_type
        FROM zabaptags_tgobj
        WHERE tag_id IN @tag_id_range
          AND object_name IN @object_name_range
          AND object_type IN @object_type_range
          AND parent_object_name IN @parent_object_name_range
          AND parent_object_type IN @parent_object_type_range
        ORDER BY object_type, object_name
        INTO CORRESPONDING FIELDS OF TABLE @result
        UP TO @max_results ROWS.
    ENDIF.
  ENDMETHOD.


  METHOD get_tagged_obj_info.
    CHECK tagged_objects IS NOT INITIAL.

    SELECT tagged_object~object_name,
           tagged_object~object_type,
           tag~tag_id,
           tag~name AS tag_name,
           tag~owner AS tag_owner
      FROM zabaptags_tags AS tag
        INNER JOIN zabaptags_tgobj AS tagged_object
          ON tag~tag_id = tagged_object~tag_id
      FOR ALL ENTRIES IN @tagged_objects
      WHERE tagged_object~object_type = @tagged_objects-object_type
        AND tagged_object~object_name = @tagged_objects-object_name
        AND tagged_object~tag_id IN @tag_id_range
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD get_children_of_tagged_objects.
    CHECK parent_tag_ids IS NOT INITIAL.

    SELECT DISTINCT
           tag~tag_id,
           tag~name AS tag_name,
           tag~parent_tag_id,
           tgobj~parent_object_name,
           tgobj~parent_object_type
      FROM zabaptags_tags AS tag
        INNER JOIN zabaptags_tgobj AS tgobj
          ON tag~tag_id = tgobj~tag_id
      WHERE parent_tag_id IN @parent_tag_ids
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD get_tagged_obj_count.
    DATA: where  TYPE TABLE OF string,
          log_op TYPE string.

    LOOP AT object_refs ASSIGNING FIELD-SYMBOL(<obj_ref>).
      where = VALUE #( BASE where
        ( |{ log_op }( OBJECT_TYPE = { cl_abap_dyn_prg=>quote( <obj_ref>-tadir_type ) }| &&
          |{ c_where_and }OBJECT_NAME = { cl_abap_dyn_prg=>quote( <obj_ref>-name ) } )| ) ).
      log_op = c_where_or.
    ENDLOOP.

    CLEAR log_op.

    SELECT tag_id, COUNT( * ) AS count
      FROM zabaptags_tgobj
      WHERE (where)
        AND tag_id IN @tag_ids
      GROUP BY tag_id
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.


  METHOD insert_tagged_objects.
    CHECK new_tagged_objects IS NOT INITIAL.

    INSERT zabaptags_tgobj FROM TABLE new_tagged_objects ACCEPTING DUPLICATE KEYS.
    IF sy-dbcnt > 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD insert_tags.
    CHECK tags IS NOT INITIAL.

    INSERT zabaptags_tags FROM TABLE tags.
    IF sy-subrc = 0.
      result = abap_true.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD modify_tags.
    CHECK tags IS NOT INITIAL.

    MODIFY zabaptags_tags FROM TABLE tags.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD share_tags.
    CHECK tags_to_share IS NOT INITIAL.

    INSERT zabaptags_shtags FROM TABLE tags_to_share.
    IF sy-subrc = 0.
      DATA(l_tag_ids) = tag_ids.
      IF l_tag_ids IS INITIAL.
        l_tag_ids = VALUE #( FOR shared_tag IN tags_to_share ( sign = 'I' option = 'EQ' low = shared_tag-tag_id ) ).
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM l_tag_ids.
      UPDATE zabaptags_tags
        SET is_shared = @abap_true
        WHERE tag_id IN @l_tag_ids.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
