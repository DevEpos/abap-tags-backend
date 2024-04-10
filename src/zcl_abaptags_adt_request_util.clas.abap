"! <p class="shorttext synchronized">Request Util for ADT</p>
CLASS zcl_abaptags_adt_request_util DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Retrieves UUID uri attribute</p>
    CLASS-METHODS get_uuid_uri_attribute
      IMPORTING
        !name         TYPE string
        mandatory     TYPE abap_bool OPTIONAL
        !request      TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(result) TYPE uuid
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Retrieve values of request parameter</p>
    CLASS-METHODS get_request_param_values
      IMPORTING
        param_name     TYPE string
        default_values TYPE string_table OPTIONAL
        mandatory      TYPE abap_bool    OPTIONAL
        upper_case     TYPE abap_bool    OPTIONAL
        !request       TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(results) TYPE string_table
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Retrieve value of request parameter</p>
    CLASS-METHODS get_request_param_value
      IMPORTING
        param_name    TYPE string
        default_value TYPE any       OPTIONAL
        mandatory     TYPE abap_bool OPTIONAL
        upper_case    TYPE abap_bool OPTIONAL
        !request      TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(result) TYPE string
      RAISING
        cx_adt_rest.

    "! <p class="shorttext synchronized">Retrieve boolean parameter value from request</p>
    CLASS-METHODS get_boolean_req_param
      IMPORTING
        param_name    TYPE string
        default_value TYPE abap_bool OPTIONAL
        !request      TYPE REF TO if_adt_rest_request
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS zcl_abaptags_adt_request_util IMPLEMENTATION.
  METHOD get_uuid_uri_attribute.
    DATA uuid_c36_string TYPE string.

    request->get_uri_attribute( EXPORTING name      = name
                                          mandatory = abap_false
                                IMPORTING value     = uuid_c36_string ).

    IF uuid_c36_string IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        REPLACE ALL OCCURRENCES OF '-' IN uuid_c36_string WITH space.
        cl_system_uuid=>convert_uuid_c32_static( EXPORTING uuid     = to_upper( uuid_c36_string )
                                                 IMPORTING uuid_x16 = result ).
      CATCH cx_uuid_error INTO DATA(conversion_error).
        RAISE EXCEPTION TYPE zcx_abaptags_adt_error
          EXPORTING previous = conversion_error.
    ENDTRY.
  ENDMETHOD.

  METHOD get_request_param_value.
    IF mandatory = abap_true.
      request->get_uri_query_parameter( EXPORTING name      = param_name
                                                  mandatory = abap_true
                                        IMPORTING value     = result ).
    ELSE.
      request->get_uri_query_parameter( EXPORTING name    = param_name
                                                  default = default_value
                                        IMPORTING value   = result ).
    ENDIF.

    IF upper_case = abap_true.
      result = to_upper( result ).
    ENDIF.
  ENDMETHOD.

  METHOD get_request_param_values.
    IF mandatory = abap_true.
      request->get_uri_query_parameter_values( EXPORTING name      = param_name
                                                         mandatory = abap_true
                                               IMPORTING values    = results ).
    ELSE.
      request->get_uri_query_parameter_values( EXPORTING name      = param_name
                                                         default   = default_values
                                                         mandatory = mandatory
                                               IMPORTING values    = results ).
    ENDIF.

    IF upper_case = abap_true.

      LOOP AT results ASSIGNING FIELD-SYMBOL(<value>).
        <value> = to_upper( <value> ).
      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD get_boolean_req_param.
    TRY.
        DATA(value) = get_request_param_value( param_name = param_name
                                               request    = request ).
        IF value IS NOT INITIAL.
          value = to_lower( value ).
          result = COND #( WHEN value = 'true' OR value = 'x' THEN abap_true ).
        ELSE.
          result = default_value.
        ENDIF.
      CATCH cx_adt_rest.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
