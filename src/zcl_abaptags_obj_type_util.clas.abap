"! <p class="shorttext synchronized">Utility for ADT Object Type Handling</p>
CLASS zcl_abaptags_obj_type_util DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.

    CLASS-METHODS is_local_class_type
      IMPORTING
        obj_type      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_local_intf_type
      IMPORTING
        obj_type      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS is_local_class_or_intf_type
      IMPORTING
        obj_type      TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS adjust_wb_type
      IMPORTING
        uri           TYPE string
      CHANGING
        obj_type      TYPE clike

      RETURNING
        VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS c_prog_include_uri_prefix TYPE string VALUE '/sap/bc/adt/programs/includes*'.

    CLASS-DATA local_class_types TYPE RANGE OF string.
    CLASS-DATA local_interface_types TYPE RANGE OF string.
ENDCLASS.


CLASS zcl_abaptags_obj_type_util IMPLEMENTATION.
  METHOD class_constructor.
    local_class_types = VALUE #( sign   = 'I'
                                 option = 'EQ'
                                 ( low = zif_abaptags_c_global=>wb_object_types-class_local_class )
                                 ( low = zif_abaptags_c_global=>wb_object_types-fugr_local_class )
                                 (  low = zif_abaptags_c_global=>wb_object_types-prog_local_class ) ).
    local_interface_types = VALUE #( sign   = 'I'
                                     option = 'EQ'
                                     ( low = zif_abaptags_c_global=>wb_object_types-class_local_interface )
                                     ( low = zif_abaptags_c_global=>wb_object_types-fugr_local_interface )
                                     (  low = zif_abaptags_c_global=>wb_object_types-prog_local_interface ) ).
  ENDMETHOD.

  METHOD is_local_class_type.
    result = xsdbool( obj_type IN local_class_types ).
  ENDMETHOD.

  METHOD is_local_intf_type.
    result = xsdbool( obj_type IN local_interface_types ).
  ENDMETHOD.

  METHOD is_local_class_or_intf_type.
    result = xsdbool( obj_type IN local_class_types OR obj_type IN local_interface_types ).
  ENDMETHOD.

  METHOD adjust_wb_type.
    " If a program include resides inside a function group, ADT may determine FUGR/PL for a local class
    " so the type will be reset to PROG/PL
    IF     uri         CP c_prog_include_uri_prefix
       AND obj_type(4) <> zif_abaptags_c_global=>object_types-program.
      obj_type = zif_abaptags_c_global=>object_types-program && obj_type+4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
