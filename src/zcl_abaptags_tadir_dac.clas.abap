"! <p class="shorttext synchronized">DB Access for TADIR</p>
CLASS zcl_abaptags_tadir_dac DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Returns current instance</p>
    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO zcl_abaptags_tadir_dac.

    "! <p class="shorttext synchronized">Retrieves tadir infos for given list of tadir keys</p>
    METHODS get_tadir_infos
      IMPORTING
        tadir_keys    TYPE zif_abaptags_ty_global=>ty_tadir_keys
      RETURNING
        VALUE(result) TYPE zif_abaptags_ty_global=>ty_tadir_infos.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO zcl_abaptags_tadir_dac.
ENDCLASS.


CLASS zcl_abaptags_tadir_dac IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.
    result = instance.
  ENDMETHOD.

  METHOD get_tadir_infos.
    CHECK tadir_keys IS NOT INITIAL.

    SELECT DISTINCT
           obj_name AS name,
           object AS type,
           devclass AS package_name,
           author
      FROM tadir
      FOR ALL ENTRIES IN @tadir_keys
      WHERE obj_name = @tadir_keys-name
        AND object   = @tadir_keys-type
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.
ENDCLASS.

