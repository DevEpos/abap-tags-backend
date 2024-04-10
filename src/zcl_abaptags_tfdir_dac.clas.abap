"! <p class="shorttext synchronized">DB Access for TFDIR</p>
CLASS zcl_abaptags_tfdir_dac DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Returns current instance</p>
    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO zcl_abaptags_tfdir_dac.

    "! <p class="shorttext synchronized">Finds function modules</p>
    METHODS find_func_modules
      IMPORTING
        !keys         TYPE zif_abaptags_ty_global=>ty_func_module_range
      RETURNING
        VALUE(result) TYPE zif_abaptags_ty_global=>ty_func_module_infos.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO zcl_abaptags_tfdir_dac.
ENDCLASS.


CLASS zcl_abaptags_tfdir_dac IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.
    result = instance.
  ENDMETHOD.

  METHOD find_func_modules.
    CHECK keys IS NOT INITIAL.

    SELECT funcname AS function,
           pname    AS program
      FROM tfdir
      WHERE funcname IN @keys
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.
ENDCLASS.

