"! <p class="shorttext synchronized" lang="en">Helper for mapping DDL name to STOB</p>
CLASS zcl_abaptags_ddl2stob_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_ddlname_range TYPE RANGE OF ddlname.

    METHODS:
      map_ddl_names
        IMPORTING
          ddl_name_range TYPE ty_ddlname_range
        RETURNING
          VALUE(result)  TYPE abap_bool,
      get_raw_stob_name
        IMPORTING
          ddlname       TYPE ddlname
        RETURNING
          VALUE(result) TYPE objectname.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_entity_name_map_entry,
        ddlname         TYPE ddlname,
        entity_name_raw TYPE objectname,
      END OF ty_entity_name_map_entry,

      ty_entity_name_map TYPE HASHED TABLE OF ty_entity_name_map_entry WITH UNIQUE KEY ddlname.

    DATA entity_name_map TYPE ty_entity_name_map.
ENDCLASS.



CLASS zcl_abaptags_ddl2stob_helper IMPLEMENTATION.

  METHOD map_ddl_names.

    CHECK ddl_name_range IS NOT INITIAL.

    SELECT dep~ddlname,
           stob~strucobjn_raw AS entity_name_raw
      FROM ddldependency AS dep
        INNER JOIN dd02b AS stob
          ON dep~objectname = stob~strucobjn
      WHERE ddlname IN @ddl_name_range
        AND objecttype = @zif_abaptags_c_global=>object_types-structured_object
        AND state = 'A'
      INTO CORRESPONDING FIELDS OF TABLE @entity_name_map.

    result = xsdbool( sy-dbcnt > 0 ).
  ENDMETHOD.


  METHOD get_raw_stob_name.
    DATA(entity_map_entry) = REF #( entity_name_map[ ddlname = ddlname ] OPTIONAL ).
    IF entity_name_map IS NOT INITIAL.
      result = entity_map_entry->entity_name_raw.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
