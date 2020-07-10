CLASS zcl_abaptags_adt_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_s_adt_object_ref.
             INCLUDE TYPE sadt_object_reference.
             TYPES: parent_type TYPE string.
    TYPES: parent_name TYPE string.
    TYPES: END OF ty_s_adt_object_ref.
    "! <p class="shorttext synchronized" lang="en">Retrieve adt object and names</p>
    CLASS-METHODS get_adt_objects_and_names
      IMPORTING
        !iv_obj_name       TYPE tadir-obj_name
        !iv_obj_type       TYPE tadir-object
        if_retrieve_parent TYPE abap_bool OPTIONAL
      EXPORTING
        !eo_adt_uri_mapper TYPE REF TO if_adt_uri_mapper
        !eo_adt_objectref  TYPE REF TO cl_adt_object_reference
        !ev_program        TYPE progname
        !ev_include        TYPE progname
      RAISING
        zcx_abaptags_exception .
    "! <p class="shorttext synchronized" lang="en">Retrieve ADT Object Reference for the given name/type</p>
    CLASS-METHODS get_adt_obj_ref
      IMPORTING
        !iv_name              TYPE seu_objkey
        !is_type              TYPE wbobjtype
        if_retrieve_parent    TYPE abap_bool OPTIONAL
        if_ignore_cache       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_adt_obj_ref) TYPE ty_s_adt_object_ref .
    "! <p class="shorttext synchronized" lang="en">Retrieve ADT Object Ref for the given name/tadir type</p>
    CLASS-METHODS get_adt_obj_ref_for_tadir_type
      IMPORTING
        !iv_tadir_type             TYPE tadir-object
        !iv_name                   TYPE sobj_name
        if_retrieve_parent         TYPE abap_bool OPTIONAL
        if_ignore_cache            TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_object_reference) TYPE ty_s_adt_object_ref .
    "! <p class="shorttext synchronized" lang="en">Maps wb object to ADT object reference</p>
    CLASS-METHODS map_tadir_obj_to_object_ref
      IMPORTING
        !iv_name             TYPE seu_objkey
        !is_type             TYPE wbobjtype
        if_retrieve_parent   TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(rs_object_ref) TYPE ty_s_adt_object_ref .
    "! <p class="shorttext synchronized" lang="en">Maps the given URI to a workbench object</p>
    CLASS-METHODS map_uri_to_wb_object
      IMPORTING
        !iv_uri               TYPE string
      EXPORTING
        VALUE(ev_object_name) TYPE string
        VALUE(es_object_type) TYPE wbobjtype
        VALUE(ev_tadir_type)  TYPE trobjtype
      RAISING
        cx_adt_uri_mapping .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_messages,
        wb_request_not_created TYPE string VALUE 'Workbench Request object could not be created' ##NO_TEXT,
      END OF c_messages.
    CONSTANTS c_segw_project_uri_pattern TYPE string VALUE '/sap/bc/adt/vit/gw/sb/project/'.
    CONSTANTS:
      BEGIN OF c_adt_types,
        function_module TYPE trobjtype VALUE 'FUNC',
        function_group  TYPE trobjtype VALUE 'FUGR',
        segw_project    TYPE trobjtype VALUE 'IWPR',
      END OF c_adt_types.
    TYPES:
      BEGIN OF ty_s_adt_object_uri_cache,
        name TYPE seu_objkey,
        type TYPE wbobjtype,
        uri  TYPE string,
      END OF ty_s_adt_object_uri_cache.
    TYPES:
      BEGIN OF ty_s_adt_object_ref_cache,
        name       TYPE sobj_name,
        type       TYPE trobjtype,
        adt_object TYPE ty_s_adt_object_ref,
      END OF ty_s_adt_object_ref_cache.

    CLASS-DATA gt_adt_obj_ref_uri_map TYPE HASHED TABLE OF ty_s_adt_object_uri_cache WITH UNIQUE KEY name type.
    CLASS-DATA gt_adt_obj_ref_map TYPE HASHED TABLE OF ty_s_adt_object_ref_cache WITH UNIQUE KEY name type.

    CLASS-METHODS resolve_parent_uri
      CHANGING
        cs_object_ref TYPE ty_s_adt_object_ref.
    CLASS-METHODS adjust_object_reference
      CHANGING
        cs_object_ref TYPE zcl_abaptags_adt_util=>ty_s_adt_object_ref.
ENDCLASS.



CLASS zcl_abaptags_adt_util IMPLEMENTATION.


  METHOD get_adt_objects_and_names.
    DATA lv_obj_type       TYPE trobjtype.
    DATA lv_obj_name       TYPE trobj_name.
    FIELD-SYMBOLS <lv_uri> TYPE string.

    lv_obj_name = iv_obj_name.
    lv_obj_type = iv_obj_type.

    cl_wb_object=>create_from_transport_key(
      EXPORTING
        p_object    = lv_obj_type
        p_obj_name  = lv_obj_name
      RECEIVING
        p_wb_object = DATA(lo_wb_object)
      EXCEPTIONS
        OTHERS      = 1 ).
    IF sy-subrc <> 0.
      zcx_abaptags_exception=>raise( iv_text = |Object with name { lv_obj_name } and type { lv_obj_type } does not exist| ).
    ENDIF.

    DATA(lo_adt_tools) = cl_adt_tools_core_factory=>get_instance( ).

    cl_wb_request=>create_from_object_ref(
      EXPORTING
        p_wb_object       = lo_wb_object
      RECEIVING
        p_wb_request      = DATA(lo_wb_request)
      EXCEPTIONS
        illegal_operation = 1
        cancelled         = 2
        OTHERS            = 3 ).

    IF sy-subrc <> 0.
      zcx_abaptags_exception=>raise( iv_text = c_messages-wb_request_not_created ).
    ENDIF.

    DATA(lo_vit_adt_mapper) = lo_adt_tools->get_uri_mapper_vit( ).

    IF lo_vit_adt_mapper->is_vit_wb_request( lo_wb_request ).
      eo_adt_objectref = lo_vit_adt_mapper->map_wb_request_to_objref( wb_request = lo_wb_request ).
    ELSE.
      eo_adt_uri_mapper = lo_adt_tools->get_uri_mapper( ).

      IF if_retrieve_parent = abap_true.
        DATA(lo_mapping_options) = lo_adt_tools->create_mapping_options( ).
        lo_mapping_options->set_use_parent( abap_true ).
      ENDIF.
      eo_adt_objectref = eo_adt_uri_Mapper->map_wb_request_to_objref(
          wb_request      = lo_wb_request
          mapping_options = lo_mapping_options
      ).
**      eo_adt_objectref = eo_adt_uri_mapper->map_wb_object_to_objref(
**          wb_object       = lo_wb_object
**          mapping_options = lo_mapping_options
**      ).

      IF ev_program IS SUPPLIED.
        eo_adt_uri_mapper->map_objref_to_include(
          EXPORTING
            uri                = eo_adt_objectref->ref_data-uri
          IMPORTING
            program            = ev_program
            include            = ev_include
        ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_adt_obj_ref.
    IF if_ignore_cache = abap_false.
      ASSIGN gt_adt_obj_ref_map[ name = iv_name type = is_type ] TO FIELD-SYMBOL(<ls_obj_ref>).
    ENDIF.

    IF <ls_obj_ref> IS NOT ASSIGNED OR if_ignore_cache = abap_true.
      DATA(ls_obj_ref) = map_tadir_obj_to_object_ref(
         iv_name            = iv_name
         is_type            = is_type
         if_retrieve_parent = if_retrieve_parent
      ).
      IF ls_obj_ref IS NOT INITIAL AND ls_obj_ref-uri IS NOT INITIAL.
        rs_adt_obj_ref = ls_obj_ref.

        IF if_ignore_cache = abap_false.
          INSERT VALUE #(
            name       = iv_name
            type       = is_type
            adt_object = ls_obj_ref
          ) INTO TABLE gt_adt_obj_ref_map.
        ENDIF.

      ENDIF.
    ELSE.
      rs_adt_obj_ref = <ls_obj_ref>-adt_object.
    ENDIF.

  ENDMETHOD.


  METHOD get_adt_obj_ref_for_tadir_type.
    DATA: lv_obj_type LIKE iv_tadir_type.

    IF if_ignore_cache = abap_false.
      ASSIGN gt_adt_obj_ref_map[ name = iv_name type = iv_tadir_type ] TO FIELD-SYMBOL(<ls_adt_object>).
    ENDIF.

    IF <ls_adt_object> IS NOT ASSIGNED OR if_ignore_cache = abap_true.

      TRY.
          get_adt_objects_and_names(
            EXPORTING
              iv_obj_name        = iv_name
              iv_obj_type        = iv_tadir_type
              if_retrieve_parent = if_retrieve_parent
            IMPORTING
              eo_adt_objectref  = DATA(lr_adt_objectref)
              eo_adt_uri_mapper = DATA(lo_uri_mapper)
          ).
          IF lr_adt_objectref->ref_data-uri IS INITIAL.
            RETURN.
          ENDIF.
          rs_object_reference = CORRESPONDING #( lr_adt_objectref->ref_data ).
          adjust_object_reference( CHANGING cs_object_ref = rs_object_reference ).
          IF rs_object_reference-parent_uri IS NOT INITIAL.
            resolve_parent_uri( CHANGING cs_object_ref = rs_object_reference ).
          ENDIF.
        CATCH zcx_abaptags_exception.
      ENDTRY.

      IF if_ignore_cache = abap_false.
        INSERT VALUE #(
          name       = iv_name
          type       = iv_tadir_type
          adt_object = rs_object_reference
        ) INTO TABLE gt_adt_obj_ref_map.
      ENDIF.
    ELSE.
      rs_object_reference = <ls_adt_object>-adt_object.
    ENDIF.

  ENDMETHOD.


  METHOD map_tadir_obj_to_object_ref.
    TRY.
        cl_wb_object=>create_from_global_type(
          EXPORTING
            p_object_type             = is_type
            p_object_key              = iv_name
          RECEIVING
            p_wb_object               = DATA(lo_wb_object)
          EXCEPTIONS
            objecttype_not_existing   = 1
            input_data_not_sufficient = 2
            OTHERS                    = 3
        ).
        CHECK sy-subrc = 0.
        DATA(lo_adt_tools_core_f) = cl_adt_tools_core_factory=>get_instance( ).
        DATA(lo_uri_mapper) = lo_adt_tools_core_f->get_uri_mapper( ).

        IF if_retrieve_parent = abap_true.
          DATA(lo_mapping_options) = lo_adt_tools_core_f->create_mapping_options( ).
          lo_mapping_options->set_use_parent( abap_true ).
        ENDIF.

        DATA(lo_object_ref) = lo_uri_mapper->map_wb_object_to_objref(
            wb_object       = lo_wb_object
            mapping_options = lo_mapping_options
        ).
        CHECK lo_object_ref IS BOUND.
        rs_object_ref = CORRESPONDING #( lo_object_ref->ref_data ).

        adjust_object_reference( CHANGING cs_object_ref = rs_object_ref ).

        IF rs_object_ref-parent_uri IS NOT INITIAL.
          resolve_parent_uri(
            CHANGING cs_object_ref = rs_object_ref
          ).
        ENDIF.

      CATCH cx_adt_uri_mapping.
    ENDTRY.
  ENDMETHOD.


  METHOD map_uri_to_wb_object.
    DATA: lt_uri       TYPE TABLE OF string,
          lo_wb_object TYPE REF TO cl_wb_object.
    CHECK iv_uri IS NOT INITIAL.

    DATA(lv_uri) = iv_uri.
    IF lv_uri CA '#'. " Fragment part is not needed
      SPLIT lv_uri AT '#' INTO TABLE lt_uri.
      lv_uri = lt_uri[ 1 ].
    ENDIF.

    " custom handling for certain uris
    IF lv_uri CS c_segw_project_uri_pattern.
      FIND REGEX |{ c_segw_project_uri_pattern }(.+)|
        IN lv_uri IGNORING CASE SUBMATCHES ev_object_name.
      ev_object_name = cl_http_utility=>unescape_url( ev_object_name ).
      es_object_type-objtype_tr =
      ev_tadir_type = c_adt_types-segw_project.
      RETURN.
    ENDIF.

    DATA(lo_adt_tools_core_f) = cl_adt_tools_core_factory=>get_instance( ).

    lo_wb_object = lo_adt_tools_core_f->get_uri_mapper( )->map_objref_to_wb_object(
        uri             = lv_uri
    ).
    ev_object_name = lo_wb_object->get_display_name( ).
    lo_wb_object->get_global_wb_key( IMPORTING p_object_type = es_object_type p_object_name = DATA(lv_obj_name) ).
    lo_wb_object->get_object_type_ref( )->get_tadir_types( IMPORTING p_r3tr_object_type = ev_tadir_type ).

    IF es_object_type-objtype_tr = c_adt_types-function_group.

      IF ev_object_name CP 'SAPL*'.
        ev_tadir_type = es_object_type-objtype_tr.
        es_object_type-subtype_wb = swbm_c_type_function_pool.
        ev_object_name = ev_object_name+4.
      ENDIF.

      IF es_object_type-subtype_wb = swbm_c_type_function.
        ev_tadir_type = c_adt_types-function_module.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD resolve_parent_uri.
    map_uri_to_wb_object(
      EXPORTING
        iv_uri         = cs_object_ref-parent_uri
      IMPORTING
        ev_object_name = DATA(lv_parent_name)
        es_object_type = DATA(ls_parent_type)
    ).
    cs_object_ref-parent_name = lv_parent_name.
    IF cs_object_ref-parent_name CP 'SAPL*'.
      cs_object_ref-parent_name = cs_object_ref-parent_name+4.
    ENDIF.
    cs_object_ref-parent_type = COND #(
      WHEN ls_parent_type-subtype_wb IS NOT INITIAL THEN |{ ls_parent_type-objtype_tr }/{ ls_parent_type-subtype_wb }|
      ELSE                                               ls_parent_type-objtype_tr
    ).
  ENDMETHOD.


  METHOD adjust_object_reference.
    IF cs_object_ref-type CP 'FUGR*' AND
       cs_object_ref-name CP 'SAPL*'.

      cs_object_ref-name = cs_object_ref-name+4.
      CLEAR cs_object_ref-parent_uri.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
