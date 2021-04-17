"! <p class="shorttext synchronized" lang="en">Access to users of SAP system</p>
CLASS zcl_abaptags_user_dac DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieve instance of Users DAC</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_abaptags_user_dac.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Finds users for user id range</p>
      find_users
        IMPORTING
          id_range      TYPE zif_abaptags_ty_global=>ty_owner_range
        RETURNING
          VALUE(result) TYPE zabaptags_user_t.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      instance TYPE REF TO zcl_abaptags_user_dac.
ENDCLASS.



CLASS zcl_abaptags_user_dac IMPLEMENTATION.


  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.

    result = instance.
  ENDMETHOD.


  METHOD find_users.
    CHECK id_range IS NOT INITIAL.

    SELECT DISTINCT
           usr02~bname AS name,
           name_first,
           name_last,
           name_textc
      FROM usr02 LEFT OUTER JOIN user_addr
        ON usr02~bname = user_addr~bname
      WHERE usr02~bname IN @id_range ORDER BY usr02~bname
      INTO TABLE @DATA(users).

    IF sy-subrc = 0.
      LOOP AT users ASSIGNING FIELD-SYMBOL(<user>).
        APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<user_result>).
        <user_result>-name = <user>-name.
        IF <user>-name_textc IS NOT INITIAL.
          <user_result>-text = <user>-name_textc.
        ELSEIF <user>-name_last IS NOT INITIAL AND <user>-name_first IS NOT INITIAL.
          <user_result>-text = |{ <user>-name_last }, { <user>-name_first } |.
        ELSE.
          <user_result>-text = <user>-name.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
