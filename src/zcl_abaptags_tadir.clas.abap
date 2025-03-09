"! <p class="shorttext synchronized">API for TADIR access</p>
CLASS zcl_abaptags_tadir DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Creates new instance of TADIR access</p>
    METHODS constructor
      IMPORTING
        !keys TYPE zif_abaptags_ty_global=>ty_tadir_keys.

    "! <p class="shorttext synchronized">Determine TADIR information for key table</p>
    METHODS determine_tadir_entries
      RETURNING
        VALUE(result) TYPE REF TO zcl_abaptags_tadir.

    "! <p class="shorttext synchronized">Get single TADIR info for key</p>
    METHODS get_tadir_info
      IMPORTING
        !name         TYPE sobj_name
        !type         TYPE trobjtype
      RETURNING
        VALUE(result) TYPE zif_abaptags_ty_global=>ty_tadir_info
      RAISING
        cx_sy_itab_line_not_found.

  PRIVATE SECTION.
    TYPES ty_pack_range TYPE RANGE OF devclass.
    TYPES:
      BEGIN OF ty_fugr_include,
        include TYPE progname,
        group TYPE rs38l_area,
      END OF ty_fugr_include.

    DATA tadir_keys TYPE zif_abaptags_ty_global=>ty_tadir_keys.
    DATA tadir_infos TYPE zif_abaptags_ty_global=>ty_tadir_infos.
    DATA func_modules TYPE zif_abaptags_ty_global=>ty_func_module_infos.
    DATA fugr_includes TYPE SORTED TABLE OF ty_fugr_include WITH UNIQUE KEY include.

    METHODS find_func_module_info
      IMPORTING
        !keys TYPE zif_abaptags_ty_global=>ty_func_module_range.

    METHODS is_tadir_prog
      IMPORTING
        obj_name      TYPE tadir-obj_name
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS add_local_packages
      IMPORTING
        pack_range TYPE ty_pack_range.
ENDCLASS.


CLASS zcl_abaptags_tadir IMPLEMENTATION.
  METHOD constructor.
    tadir_keys = keys.
  ENDMETHOD.

  METHOD determine_tadir_entries.
    DATA func_module_range TYPE RANGE OF tfdir-funcname.
    DATA local_package_range TYPE ty_pack_range.

    LOOP AT tadir_keys ASSIGNING FIELD-SYMBOL(<key>).
      IF <key>-type = zif_abaptags_c_global=>object_types-function.
        func_module_range = VALUE #( BASE func_module_range
                                     ( sign = 'I' option = 'EQ' low = <key>-name ) ).
        DELETE tadir_keys.
      ELSEIF         <key>-type = zif_abaptags_c_global=>object_types-program
             AND NOT is_tadir_prog( <key>-name ).
        DELETE tadir_keys.
      ELSEIF     <key>-type  = zif_abaptags_c_global=>object_types-package
             AND <key>-name CP '$*'.
        local_package_range = VALUE #( BASE local_package_range ( sign = 'I' option = 'EQ' low = <key>-name ) ).
        DELETE tadir_keys.
      ENDIF.
    ENDLOOP.

    find_func_module_info( func_module_range ).

    tadir_infos = zcl_abaptags_tadir_dac=>get_instance( )->get_tadir_infos( tadir_keys ).

    add_local_packages( local_package_range ).

    result = me.
  ENDMETHOD.

  METHOD find_func_module_info.
    DATA group_namespace TYPE namespace.

    CHECK keys IS NOT INITIAL.

    func_modules = zcl_abaptags_tfdir_dac=>get_instance( )->find_func_modules( keys ).

    LOOP AT func_modules ASSIGNING FIELD-SYMBOL(<func_module>).
      CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
        EXPORTING  program   = <func_module>-program
        IMPORTING  group     = <func_module>-group
                   namespace = group_namespace
        EXCEPTIONS OTHERS    = 1.
      IF sy-subrc <> 0.
        DELETE func_modules.
      ELSE.
        IF group_namespace IS NOT INITIAL.
          <func_module>-group = group_namespace && <func_module>-group.
        ENDIF.
        INSERT VALUE #( name = <func_module>-group
                        type = zif_abaptags_c_global=>object_types-function_group ) INTO TABLE tadir_keys.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_tadir_prog.
    DATA is_fugr_include TYPE abap_bool.
    DATA function_group TYPE rs38l_area.
    DATA namespace TYPE namespace.

    CALL FUNCTION 'RS_PROGNAME_SPLIT'
      EXPORTING  progname_with_namespace = obj_name
      IMPORTING  fugr_is_include_name    = is_fugr_include
                 fugr_group              = function_group
                 namespace               = namespace
      EXCEPTIONS delimiter_error         = 0.

    IF is_fugr_include = abap_true.
      INSERT VALUE #( include = obj_name group = namespace && function_group ) INTO TABLE fugr_includes.
      INSERT VALUE #( name = namespace && function_group
                      type = zif_abaptags_c_global=>object_types-function_group ) INTO TABLE tadir_keys.
    ELSE.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_tadir_info.
    DATA(l_name) = name.
    DATA(l_type) = type.

    IF type = zif_abaptags_c_global=>object_types-function.
      ASSIGN func_modules[ KEY function
                           function = name ] TO FIELD-SYMBOL(<function>).
      IF sy-subrc = 0.
        l_name = <function>-group.
        l_type = zif_abaptags_c_global=>object_types-function_group.
      ENDIF.
    ELSEIF type = zif_abaptags_c_global=>object_types-program.
      ASSIGN fugr_includes[ include = name ] TO FIELD-SYMBOL(<fugr_include>).
      IF sy-subrc = 0.
        l_name = <fugr_include>-group.
        l_type = zif_abaptags_c_global=>object_types-function_group.
      ENDIF.
    ENDIF.

    result = tadir_infos[ KEY name_type
                          name = l_name
                          type = l_type ].
  ENDMETHOD.

  METHOD add_local_packages.
    CHECK pack_range IS NOT INITIAL.

    SELECT devclass AS name,
           'DEVC'   AS type,
           as4user  AS author,
           devclass AS package_name
      FROM tdevc
      WHERE devclass IN @pack_range
      APPENDING CORRESPONDING FIELDS OF TABLE @tadir_infos.
  ENDMETHOD.
ENDCLASS.
