"! <p class="shorttext synchronized">ADT Mapper for components in repository objects</p>
"! <strong>Note</strong>: Currently it only supports components in global classes<br/><br/>
"!
"! <strong>Additional remarks:</strong><br/>
"! If the statement of an interface/class declaration or class implementation spans multiple lines
"! and contains some line comments the current RegEx logic will not find this and therefore
"! the component will not be mapped and various UIs in the ADT Frontend of ABAP Tags will not show
"! the component although it was tagged and exists in the global class. <br/>
"! This issues may be resolved with a adjusted RegEx logic and some post processing of the matches
"! to consider all possible variations of declaration/implementation statements.
CLASS zcl_abaptags_comp_adt_mapper DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS reset.

    METHODS get_adt_object
      IMPORTING
        comp          TYPE zif_abaptags_ty_global=>ty_object_comp_info
      RETURNING
        VALUE(result) TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.

    METHODS add_components
      IMPORTING
        comps TYPE zif_abaptags_ty_global=>ty_local_adt_obj_infos.

    METHODS determine_components.

  PRIVATE SECTION.
    TYPES ty_main_comp_name TYPE c LENGTH 30.
    TYPES  BEGIN OF ty_comp_id.
             INCLUDE TYPE zif_abaptags_ty_global=>ty_object_comp_info.
    TYPES:   main_prog  TYPE program,
             include    TYPE program,
             line       TYPE i,
             offset     TYPE i,
             end_line   TYPE i,
             end_offset TYPE i,
             uri        TYPE string.
    TYPES  END OF ty_comp_id.

    TYPES:
      BEGIN OF ty_global_object,
        name      TYPE ty_main_comp_name,
        type      TYPE trobjtype,
        processed TYPE abap_bool,
      END OF ty_global_object.

    DATA main_objects TYPE SORTED TABLE OF ty_global_object WITH UNIQUE KEY name type.
    DATA components TYPE SORTED TABLE OF ty_comp_id
           WITH UNIQUE KEY object_name object_type component_name component_type.
    DATA mapped_components LIKE components.
    DATA adt_tools_factory TYPE REF TO if_adt_tools_core_factory.
    DATA found_class_impl TYPE SORTED TABLE OF seoclsname WITH UNIQUE KEY table_line.
    DATA uri_mapper TYPE REF TO if_adt_uri_mapper.

    METHODS process_class
      IMPORTING
        main_obj TYPE ty_global_object.

    METHODS process_function_include
      IMPORTING
        main_obj          TYPE ty_global_object
        alt_main_obj_type TYPE trobjtype OPTIONAL.

    METHODS process_program
      IMPORTING
        main_obj TYPE ty_global_object.

    METHODS search_include
      IMPORTING
        main_obj          TYPE ty_global_object
        alt_main_obj_type TYPE trobjtype OPTIONAL
        main_prog         TYPE program
        incl_name         TYPE program.

    METHODS find_intf_n_cls_impl
      IMPORTING
        main_obj          TYPE ty_global_object
        alt_main_obj_type TYPE trobjtype OPTIONAL
        source_code       TYPE string
        indexes           TYPE ty_line_indexes
        main_prog         TYPE program
        incl_name         TYPE program.

    METHODS find_cls_defs
      IMPORTING
        main_obj          TYPE ty_global_object
        alt_main_obj_type TYPE trobjtype OPTIONAL
        source_code       TYPE string
        indexes           TYPE ty_line_indexes
        main_prog         TYPE program
        incl_name         TYPE program.

    METHODS collect_found_component
      IMPORTING
        main_obj        TYPE ty_global_object
        main_prog       TYPE program
        indexes         TYPE ty_line_indexes
        incl_name       TYPE program
        comp_name_match TYPE submatch_result
        comp_name       TYPE string
        comp_type       TYPE swo_objtyp.

    METHODS get_local_class_type
      IMPORTING
        object_type   TYPE trobjtype
      RETURNING
        VALUE(result) TYPE swo_objtyp.

    METHODS get_local_interface_type
      IMPORTING
        object_type   TYPE trobjtype
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_abaptags_comp_adt_mapper IMPLEMENTATION.
  METHOD add_components.
    " Are duplicates possible???

    LOOP AT comps REFERENCE INTO DATA(comp).
      INSERT VALUE #( name = comp->object_name type = comp->object_type ) INTO TABLE main_objects.
      INSERT CORRESPONDING #( comp->* ) INTO TABLE components.
    ENDLOOP.

    " clear processing status
  ENDMETHOD.

  METHOD get_adt_object.
    DATA(mapped_comp) = REF #( mapped_components[ object_name    = comp-object_name
                                                  object_type    = comp-object_type
                                                  component_name = comp-component_name
                                                  component_type = comp-component_type ] OPTIONAL ).
    IF mapped_comp IS INITIAL OR mapped_comp->include IS INITIAL.
      RETURN.
    ENDIF.

    IF mapped_comp->uri IS NOT INITIAL.
      result = VALUE #( name        = mapped_comp->component_name
                        type        = mapped_comp->component_type
                        parent_name = mapped_comp->object_name
                        uri         = mapped_comp->uri ).
    ENDIF.

    IF adt_tools_factory IS INITIAL.
      adt_tools_factory = cl_adt_tools_core_factory=>get_instance( ).
      uri_mapper = adt_tools_factory->get_uri_mapper( ).
    ENDIF.

    TRY.
        DATA(obj_ref) = uri_mapper->map_include_to_objref(
            program     = mapped_comp->main_prog
            include     = COND #( WHEN mapped_comp->include <> mapped_comp->main_prog THEN mapped_comp->include )
            line        = mapped_comp->line
            line_offset = mapped_comp->offset
            end_line    = mapped_comp->end_line
            end_offset  = mapped_comp->end_offset ).

        mapped_comp->uri = obj_ref->ref_data-uri.

        result = VALUE #( name        = mapped_comp->component_name
                          type        = mapped_comp->component_type
                          parent_name = mapped_comp->object_name
                          uri         = obj_ref->ref_data-uri ).
      CATCH cx_adt_uri_mapping.
    ENDTRY.
  ENDMETHOD.

  METHOD determine_components.
    LOOP AT main_objects REFERENCE INTO DATA(main_obj) WHERE processed = abap_false.
      IF main_obj->type = zif_abaptags_c_global=>object_types-class.
        process_class( main_obj->* ).
      ELSEIF main_obj->type = zif_abaptags_c_global=>object_types-program.
        process_program( main_obj->* ).
**      ELSEIF main_obj->type = zif_abaptags_c_global=>object_types-function_group.
**        process_function_group( fugr_include = main_obj->main_object object_type = main_obj->type ).
      ENDIF.
      main_obj->processed = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD reset.
    CLEAR mapped_components.
  ENDMETHOD.

  METHOD process_class.
    DATA(main_prog) = cl_oo_classname_service=>get_classpool_name( main_obj-name ).

    " in case, class was created in older release
    search_include( main_obj  = main_obj
                    main_prog = main_prog
                    incl_name = cl_oo_classname_service=>get_cl_name( main_obj-name ) ).
    search_include( main_obj  = main_obj
                    main_prog = main_prog
                    incl_name = cl_oo_classname_service=>get_ccdef_name( main_obj-name ) ).
    search_include( main_obj  = main_obj
                    main_prog = main_prog
                    incl_name = cl_oo_classname_service=>get_ccimp_name( main_obj-name ) ).
    search_include( main_obj  = main_obj
                    main_prog = main_prog
                    incl_name = cl_oo_classname_service=>get_ccau_name( main_obj-name ) ).
  ENDMETHOD.

  METHOD process_program.
    DATA is_fugr_include TYPE abap_bool.

    DATA(progname_fuba) = CONV progname( main_obj-name ).

    CALL FUNCTION 'RS_PROGNAME_SPLIT'
      EXPORTING  progname_with_namespace = progname_fuba
      IMPORTING  fugr_is_include_name    = is_fugr_include
      EXCEPTIONS delimiter_error         = 0.

    IF is_fugr_include = abap_true.
      process_function_include( main_obj          = main_obj
                                alt_main_obj_type = zif_abaptags_c_global=>object_types-function_group ).
    ELSE.
      search_include( main_obj  = main_obj
                      main_prog = CONV #( main_obj-name )
                      incl_name = CONV #( main_obj-name ) ).
    ENDIF.
  ENDMETHOD.

  METHOD process_function_include.
    DATA fugr_group TYPE rs38l_area.

    DATA(include) = CONV progname( main_obj-name ).
    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      IMPORTING  group   = fugr_group
      CHANGING   include = include
      EXCEPTIONS OTHERS  = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    search_include( main_obj          = main_obj
                    alt_main_obj_type = alt_main_obj_type
                    main_prog         = CONV #( fugr_group )
                    incl_name         = CONV #( main_obj-name ) ).
  ENDMETHOD.

  METHOD search_include.
    DATA source_lines TYPE TABLE OF string.

    READ REPORT incl_name INTO source_lines.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lcl_source_code_util=>transform_to_string( EXPORTING source_table = source_lines
                                                         line_feed    = |\r\n|
                                               IMPORTING source_text  = DATA(multi_line_source)
                                                         indexes      = DATA(indexes) ).

    " First look for interfaces and class implementations
    " --> Link to implementation is necessary to execute only the selected unit test
    "     in ADT
    find_intf_n_cls_impl( main_obj          = main_obj
                          alt_main_obj_type = alt_main_obj_type
                          main_prog         = main_prog
                          source_code       = multi_line_source
                          indexes           = indexes
                          incl_name         = incl_name ).

    " Then find class declarations without implementation part
    find_cls_defs( main_obj          = main_obj
                   alt_main_obj_type = alt_main_obj_type
                   main_prog         = main_prog
                   source_code       = multi_line_source
                   indexes           = indexes
                   incl_name         = incl_name ).
  ENDMETHOD.

  METHOD find_cls_defs.
    FIND ALL OCCURRENCES OF REGEX `^\s*class\s+([\w\/]+)([\w\s\/]*)\.` IN source_code
         RESULTS DATA(matches) IGNORING CASE.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT matches INTO DATA(match).
      DATA(last_submatch) = match-submatches[ 2 ].
      IF find( val   = source_code
               off   = last_submatch-offset
               len   = last_submatch-length
               case  = abap_false
               regex = '(load|deferred)' ) <> -1.
        CONTINUE.
      ENDIF.

      DATA(name_submatch) = match-submatches[ 1 ].
      DATA(found_comp_name) = to_upper( source_code+name_submatch-offset(name_submatch-length) ).

      collect_found_component(
          main_obj        = main_obj
          main_prog       = main_prog
          indexes         = indexes
          incl_name       = incl_name
          comp_name_match = name_submatch
          comp_name       = found_comp_name
          comp_type       = get_local_class_type(
              COND #( WHEN alt_main_obj_type IS NOT INITIAL THEN alt_main_obj_type ELSE main_obj-type ) ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD find_intf_n_cls_impl.
    FIND ALL OCCURRENCES OF REGEX `^\s*(class|interface)\s+([\w\/]+)(\s+implementation)?\s*\.` IN source_code
         RESULTS DATA(matches) IGNORING CASE.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(obj_type_for_local_type) = COND #( WHEN alt_main_obj_type IS NOT INITIAL
                                            THEN alt_main_obj_type
                                            ELSE main_obj-type ).

    LOOP AT matches INTO DATA(match).
      DATA(type_submatch) = match-submatches[ 1 ].
      DATA(type_string) = to_upper( source_code+type_submatch-offset(type_submatch-length) ).
      DATA(name_submatch) = match-submatches[ 2 ].
      DATA(found_comp_name) = to_upper( source_code+name_submatch-offset(name_submatch-length) ).

      DATA(comp_type) = COND #(
        WHEN type_string = 'CLASS'
        THEN get_local_class_type( obj_type_for_local_type )
        ELSE get_local_interface_type( obj_type_for_local_type ) ).

      collect_found_component( main_obj        = main_obj
                               main_prog       = main_prog
                               indexes         = indexes
                               incl_name       = incl_name
                               comp_name_match = name_submatch
                               comp_name       = found_comp_name
                               comp_type       = comp_type ).
    ENDLOOP.
  ENDMETHOD.

  METHOD collect_found_component.
    DATA(unmapped_comp) = REF #(
      components[ object_name    = main_obj-name
                  object_type    = main_obj-type
                  component_name = comp_name
                  component_type = comp_type ] OPTIONAL ).

    IF unmapped_comp IS INITIAL.
      RETURN.
    ENDIF.

    DATA(mapped_comp) = unmapped_comp->*.
    mapped_comp-main_prog = main_prog.
    mapped_comp-include   = incl_name.

    " Determine correct line index
    DATA(line_index) = lcl_source_code_util=>get_line_index_by_offset( line_indexes = indexes
                                                                       offset       = comp_name_match-offset ).
    mapped_comp-line       = line_index-number.
    mapped_comp-offset     = comp_name_match-offset - line_index-offset.
    mapped_comp-end_line   = line_index-number.
    mapped_comp-end_offset = mapped_comp-offset + comp_name_match-length.
    INSERT mapped_comp INTO TABLE mapped_components.
  ENDMETHOD.

  METHOD get_local_class_type.
    result = SWITCH #( object_type
                       WHEN zif_abaptags_c_global=>object_types-class THEN
                         zif_abaptags_c_global=>wb_object_types-class_local_class
                       WHEN zif_abaptags_c_global=>object_types-program THEN
                         zif_abaptags_c_global=>wb_object_types-prog_local_class
                       WHEN zif_abaptags_c_global=>object_types-function_group THEN
                         zif_abaptags_c_global=>wb_object_types-fugr_local_class ).
  ENDMETHOD.

  METHOD get_local_interface_type.
    result = SWITCH #( object_type
                       WHEN zif_abaptags_c_global=>object_types-class THEN
                         zif_abaptags_c_global=>wb_object_types-class_local_interface
                       WHEN zif_abaptags_c_global=>object_types-program THEN
                         zif_abaptags_c_global=>wb_object_types-prog_local_interface
                       WHEN zif_abaptags_c_global=>object_types-function_group THEN
                         zif_abaptags_c_global=>wb_object_types-fugr_local_interface ).
  ENDMETHOD.
ENDCLASS.
