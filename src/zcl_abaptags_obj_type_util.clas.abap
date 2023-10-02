"! <p class="shorttext synchronized">Utility for ADT Object Type Handling</p>
CLASS zcl_abaptags_obj_type_util DEFINITION
  PUBLIC
  FINAL
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

  PRIVATE SECTION.
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
ENDCLASS.
