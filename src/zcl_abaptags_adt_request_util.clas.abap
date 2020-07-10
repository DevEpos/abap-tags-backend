"! <p class="shorttext synchronized" lang="en">Request Util for ADT</p>
CLASS zcl_abaptags_adt_request_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Retrieve values of request parameter</p>
    CLASS-METHODS get_request_param_values
      IMPORTING
        iv_param_name     TYPE string
        it_default_values TYPE string_table OPTIONAL
        if_mandatory      TYPE abap_bool OPTIONAL
        if_upper_case     TYPE abap_bool OPTIONAL
        io_request        TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(rt_values)  TYPE string_table
      RAISING
        cx_adt_rest.
    "! <p class="shorttext synchronized" lang="en">Retrieve value of request parameter</p>
    CLASS-METHODS get_request_param_value
      IMPORTING
        iv_param_name    TYPE string
        iv_default_value TYPE any OPTIONAL
        if_mandatory     TYPE abap_bool OPTIONAL
        if_upper_case    TYPE abap_bool OPTIONAL
        io_request       TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(rv_value)  TYPE string
      RAISING
        cx_adt_rest.
    "! <p class="shorttext synchronized" lang="en">Retrieve boolean parameter value from request</p>
    CLASS-METHODS get_boolean_req_param
      IMPORTING
        iv_param_name    TYPE string
        if_default_value TYPE abap_bool OPTIONAL
        io_request       TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(rf_value)  TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abaptags_adt_request_util IMPLEMENTATION.

  METHOD get_request_param_value.
    IF if_mandatory = abap_true.
      io_request->get_uri_query_parameter(
        EXPORTING name      = iv_param_name
                  mandatory = abap_true
        IMPORTING value     = rv_value
      ).
    ELSE.
      io_request->get_uri_query_parameter(
        EXPORTING name      = iv_param_name
                  default   = iv_default_value
        IMPORTING value     = rv_value
      ).
    ENDIF.

    IF if_upper_case = abap_true.
      TRANSLATE rv_value TO UPPER CASE.
    ENDIF.
  ENDMETHOD.

  METHOD get_request_param_values.
    IF if_mandatory = abap_true.
      io_request->get_uri_query_parameter_values(
        EXPORTING name      = iv_param_name
                  mandatory = abap_true
        IMPORTING values    = rt_values
      ).
    ELSE.
      io_request->get_uri_query_parameter_values(
        EXPORTING name      = iv_param_name
                  default   = it_default_values
                  mandatory = if_mandatory
        IMPORTING values    = rt_values
      ).
    ENDIF.

    IF if_upper_case = abap_true.
      LOOP AT rt_values ASSIGNING FIELD-SYMBOL(<lv_value>).
        TRANSLATE <lv_value> TO UPPER CASE.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_boolean_req_param.
    TRY.
        DATA(lv_value) = get_request_param_value(
           iv_param_name = iv_param_name
           io_request    = io_request
         ).
        IF lv_value IS NOT INITIAL.
          lv_value = to_lower( lv_value ).
          rf_value = COND #( WHEN lv_value = 'true' OR lv_value = 'x' THEN abap_true ).
        ELSE.
          rf_value = if_default_value.
        ENDIF.
      CATCH cx_adt_rest.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
