*"* use this source file for your ABAP unit test classes
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES BEGIN OF ty_extended_key.
    INCLUDE TYPE zif_abaptags_ty_global=>ty_tadir_key.
    TYPES package TYPE devclass.
    TYPES END OF ty_extended_key.

    DATA:
      cut  TYPE REF TO zcl_abaptags_tadir,
      keys TYPE TABLE OF ty_extended_key.

    METHODS:
      setup,
      test_retrieve FOR TESTING.
ENDCLASS.



CLASS ltcl_abap_unit IMPLEMENTATION.


  METHOD setup.
    keys = VALUE #(
      ( name = 'CL_ABAP_TSTMP' type = 'CLAS' package = 'SABP_CONVERT' )
      ( name = 'RS_PROGNAME_SPLIT' type = 'FUNC' package = 'SEU' )
      ( name = 'LSENVF01' type = 'PROG' package = 'S_RRR_BCCUST' )
      ( name = 'DEMO_ABAP_UNIT' type = 'PROG' package = 'SABAPDEMOS' ) ).

    cut = NEW #( CORRESPONDING #( keys ) ).
  ENDMETHOD.


  METHOD test_retrieve.
    cut->determine_tadir_entries( ).

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).
      cl_abap_unit_assert=>assert_equals(
        act = cut->get_tadir_info(
          name   = <key>-name
          type   = <key>-type )-package_name
        exp = <key>-package ).
    ENDLOOP.

    TRY.
        cut->get_tadir_info( name = 'IF_REST_CLIENT' type = 'INTF' ).
      CATCH cx_sy_itab_line_not_found INTO DATA(error).
        cl_abap_unit_assert=>assert_bound( act = error ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
