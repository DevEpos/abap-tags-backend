CLASS zcl_abaptags_adt_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_adt_obj_ref_info.
             INCLUDE TYPE sadt_object_reference.
             TYPES: parent_type TYPE string.
    TYPES: parent_name TYPE string.
    TYPES: END OF ty_adt_obj_ref_info.

    "! <p class="shorttext synchronized" lang="en">Retrieve adt object and names</p>
    CLASS-METHODS get_adt_objects_and_names
      IMPORTING
        !object_name    TYPE tadir-obj_name
        !object_type    TYPE tadir-object
        retrieve_parent TYPE abap_bool OPTIONAL
      EXPORTING
        !adt_uri_mapper TYPE REF TO if_adt_uri_mapper
        !adt_objectref  TYPE REF TO cl_adt_object_reference
        !program        TYPE progname
        !include        TYPE progname
      RAISING
        zcx_abaptags_exception .
    "! <p class="shorttext synchronized" lang="en">Retrieve ADT Object Reference for the given name/type</p>
    CLASS-METHODS get_adt_obj_ref
      IMPORTING
        !name               TYPE seu_objkey
        !wb_type            TYPE wbobjtype
        retrieve_parent     TYPE abap_bool OPTIONAL
        ignore_cache        TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(adt_obj_info) TYPE ty_adt_obj_ref_info .
    "! <p class="shorttext synchronized" lang="en">Retrieve ADT Object Ref for the given name/tadir type</p>
    CLASS-METHODS get_adt_obj_ref_for_tadir_type
      IMPORTING
        !tadir_type         TYPE tadir-object
        !name               TYPE sobj_name
        retrieve_parent     TYPE abap_bool OPTIONAL
        ignore_cache        TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(adt_obj_info) TYPE ty_adt_obj_ref_info .
    "! <p class="shorttext synchronized" lang="en">Maps wb object to ADT object reference</p>
    CLASS-METHODS map_tadir_obj_to_object_ref
      IMPORTING
        !name               TYPE seu_objkey
        !wb_type            TYPE wbobjtype
        retrieve_parent     TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(adt_obj_info) TYPE ty_adt_obj_ref_info .
    "! <p class="shorttext synchronized" lang="en">Maps the given URI to a workbench object</p>
    CLASS-METHODS map_uri_to_wb_object
      IMPORTING
        !VALUE(uri)        TYPE string
      EXPORTING
        VALUE(object_name) TYPE string
        VALUE(object_type) TYPE wbobjtype
        VALUE(tadir_type)  TYPE trobjtype
      RAISING
        cx_adt_uri_mapping .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_error_messages,
        wb_request_not_created TYPE string VALUE 'Workbench Request object could not be created' ##NO_TEXT,
      END OF c_error_messages.
    CONSTANTS c_segw_project_uri_pattern TYPE string VALUE '/sap/bc/adt/vit/gw/sb/project/'.
    CONSTANTS:
      BEGIN OF c_adt_types,
        function_module TYPE trobjtype VALUE 'FUNC',
        function_group  TYPE trobjtype VALUE 'FUGR',
        segw_project    TYPE trobjtype VALUE 'IWPR',
      END OF c_adt_types.
    TYPES:
      BEGIN OF ty_adt_object_uri_map,
        name TYPE seu_objkey,
        type TYPE wbobjtype,
        uri  TYPE string,
      END OF ty_adt_object_uri_map.
    TYPES:
      BEGIN OF ty_adt_object_info_map,
        name       TYPE sobj_name,
        type       TYPE trobjtype,
        adt_object TYPE ty_adt_obj_ref_info,
      END OF ty_adt_object_info_map.

    CLASS-DATA adt_obj_infos TYPE HASHED TABLE OF ty_adt_object_info_map WITH UNIQUE KEY name type.

    CLASS-METHODS resolve_parent_uri
      CHANGING
        adt_obj_info TYPE ty_adt_obj_ref_info.
    CLASS-METHODS adjust_object_reference
      CHANGING
        adt_obj_info TYPE zcl_abaptags_adt_util=>ty_adt_obj_ref_info.
ENDCLASS.



CLASS zcl_abaptags_adt_util IMPLEMENTATION.


  METHOD get_adt_objects_and_names.

    DATA(tr_obj_name) = CONV trobj_name( object_name ).

    cl_wb_object=>create_from_transport_key(
      EXPORTING
        p_object    = object_type
        p_obj_name  = tr_obj_name
      RECEIVING
        p_wb_object = DATA(wb_object)
      EXCEPTIONS
        OTHERS      = 1 ).
    IF sy-subrc <> 0.
      zcx_abaptags_exception=>raise( error_text = |Object with name { tr_obj_name } and type { object_type } does not exist| ).
    ENDIF.

    DATA(adt_tools_f) = cl_adt_tools_core_factory=>get_instance( ).

    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = wb_object
      RECEIVING
        p_wb_request      = DATA(wb_request)
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abaptags_exception=>raise( error_text = c_error_messages-wb_request_not_created ).
    ENDIF.

    DATA(vit_adt_mapper) = adt_tools_f->get_uri_mapper_vit( ).

    IF vit_adt_mapper->is_vit_wb_request( wb_request ).
      adt_objectref = vit_adt_mapper->map_wb_request_to_objref( wb_request = wb_request ).
    ELSE.
      adt_uri_mapper = adt_tools_f->get_uri_mapper( ).

      IF retrieve_parent = abap_true.
        DATA(mapping_options) = adt_tools_f->create_mapping_options( ).
        mapping_options->set_use_parent( abap_true ).
      ENDIF.
      adt_objectref = adt_uri_mapper->map_wb_request_to_objref(
          wb_request      = wb_request
          mapping_options = mapping_options
      ).
**      eo_adt_objectref = eo_adt_uri_mapper->map_wb_object_to_objref(
**          wb_object       = lo_wb_object
**          mapping_options = lo_mapping_options
**      ).

      IF program IS SUPPLIED.
        adt_uri_mapper->map_objref_to_include(
          EXPORTING
            uri                = adt_objectref->ref_data-uri
          IMPORTING
            program            = program
            include            = include
        ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_adt_obj_ref.
    IF ignore_cache = abap_false.
      ASSIGN adt_obj_infos[ name = name type = wb_type ] TO FIELD-SYMBOL(<adt_obj_info>).
    ENDIF.

    IF <adt_obj_info> IS NOT ASSIGNED OR ignore_cache = abap_true.
      DATA(object_info) = map_tadir_obj_to_object_ref(
         name            = name
         wb_type            = wb_type
         retrieve_parent = retrieve_parent
      ).
      IF object_info IS NOT INITIAL AND object_info-uri IS NOT INITIAL.
        adt_obj_info = object_info.

        IF ignore_cache = abap_false.
          INSERT VALUE #(
            name       = name
            type       = wb_type
            adt_object = object_info
          ) INTO TABLE adt_obj_infos.
        ENDIF.

      ENDIF.
    ELSE.
      adt_obj_info = <adt_obj_info>-adt_object.
    ENDIF.

  ENDMETHOD.


  METHOD get_adt_obj_ref_for_tadir_type.
    IF ignore_cache = abap_false.
      ASSIGN adt_obj_infos[ name = name type = tadir_type ] TO FIELD-SYMBOL(<adt_object_info>).
    ENDIF.

    IF <adt_object_info> IS NOT ASSIGNED OR ignore_cache = abap_true.

      TRY.
          get_adt_objects_and_names(
            EXPORTING
              object_name        = name
              object_type        = tadir_type
              retrieve_parent = retrieve_parent
            IMPORTING
              adt_objectref  = DATA(adt_objectref)
              adt_uri_mapper = DATA(uri_mapper)
          ).
          IF adt_objectref->ref_data-uri IS INITIAL.
            RETURN.
          ENDIF.
          adt_obj_info = CORRESPONDING #( adt_objectref->ref_data ).
          adjust_object_reference( CHANGING adt_obj_info = adt_obj_info ).
          IF adt_obj_info-parent_uri IS NOT INITIAL.
            resolve_parent_uri( CHANGING adt_obj_info = adt_obj_info ).
          ENDIF.
        CATCH zcx_abaptags_exception.
      ENDTRY.

      IF ignore_cache = abap_false.
        INSERT VALUE #(
          name       = name
          type       = tadir_type
          adt_object = adt_obj_info
        ) INTO TABLE adt_obj_infos.
      ENDIF.
    ELSE.
      adt_obj_info = <adt_object_info>-adt_object.
    ENDIF.

  ENDMETHOD.


  METHOD map_tadir_obj_to_object_ref.
    TRY.
        cl_wb_object=>create_from_global_type(
          EXPORTING
            p_object_type             = wb_type
            p_object_key              = name
          RECEIVING
            p_wb_object               = DATA(wb_object)
          EXCEPTIONS
            objecttype_not_existing   = 1
            input_data_not_sufficient = 2
            OTHERS                    = 3
        ).
        CHECK sy-subrc = 0.
        DATA(adt_tools_core_f) = cl_adt_tools_core_factory=>get_instance( ).
        DATA(uri_mapper) = adt_tools_core_f->get_uri_mapper( ).

        IF retrieve_parent = abap_true.
          DATA(mapping_options) = adt_tools_core_f->create_mapping_options( ).
          mapping_options->set_use_parent( abap_true ).
        ENDIF.

        DATA(adt_obj_ref) = uri_mapper->map_wb_object_to_objref(
            wb_object       = wb_object
            mapping_options = mapping_options
        ).
        CHECK adt_obj_ref IS BOUND.
        adt_obj_info = CORRESPONDING #( adt_obj_ref->ref_data ).

        adjust_object_reference( CHANGING adt_obj_info = adt_obj_info ).

        IF adt_obj_info-parent_uri IS NOT INITIAL.
          resolve_parent_uri(
            CHANGING adt_obj_info = adt_obj_info
          ).
        ENDIF.

      CATCH cx_adt_uri_mapping.
    ENDTRY.
  ENDMETHOD.


  METHOD map_uri_to_wb_object.
    DATA: uris TYPE TABLE OF string.

    CHECK uri IS NOT INITIAL.

    IF uri CA '#'. " Fragment part is not needed
      SPLIT uri AT '#' INTO TABLE uris.
      uri = uris[ 1 ].
    ENDIF.

    " custom handling for certain uris
    IF uri CS c_segw_project_uri_pattern.
      FIND REGEX |{ c_segw_project_uri_pattern }(.+)|
        IN uri IGNORING CASE SUBMATCHES object_name.
      object_name = cl_http_utility=>unescape_url( object_name ).
      object_type-objtype_tr =
      tadir_type = c_adt_types-segw_project.
      RETURN.
    ENDIF.

    DATA(adt_tools_core_f) = cl_adt_tools_core_factory=>get_instance( ).

    DATA(wb_object) = adt_tools_core_f->get_uri_mapper( )->map_objref_to_wb_object(
        uri = uri
    ).
    object_name = wb_object->get_display_name( ).
    wb_object->get_global_wb_key( IMPORTING p_object_type = object_type ).
    wb_object->get_object_type_ref( )->get_tadir_types( IMPORTING p_r3tr_object_type = tadir_type ).

    IF object_type-objtype_tr = c_adt_types-function_group.

      IF object_name CP 'SAPL*'.
        tadir_type = object_type-objtype_tr.
        object_type-subtype_wb = swbm_c_type_function_pool.
        object_name = object_name+4.
      ENDIF.

      IF object_type-subtype_wb = swbm_c_type_function.
        tadir_type = c_adt_types-function_module.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD resolve_parent_uri.
    map_uri_to_wb_object(
      EXPORTING
        uri         = adt_obj_info-parent_uri
      IMPORTING
        object_name = DATA(parent_name)
        object_type = DATA(parent_type)
    ).
    adt_obj_info-parent_name = parent_name.
    IF adt_obj_info-parent_name CP 'SAPL*'.
      adt_obj_info-parent_name = adt_obj_info-parent_name+4.
    ENDIF.
    adt_obj_info-parent_type = COND #(
      WHEN parent_type-subtype_wb IS NOT INITIAL THEN |{ parent_type-objtype_tr }/{ parent_type-subtype_wb }|
      ELSE                                               parent_type-objtype_tr
    ).
  ENDMETHOD.


  METHOD adjust_object_reference.
    IF adt_obj_info-type CP 'FUGR*' AND
       adt_obj_info-name CP 'SAPL*'.

      adt_obj_info-name = adt_obj_info-name+4.
      CLEAR adt_obj_info-parent_uri.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
