CLASS zcl_abaptags_message_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      set_msg_vars_for_clike
        IMPORTING
          iv_text TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      split_text
        IMPORTING
          iv_text  TYPE string
        EXPORTING
          ev_msgv1 TYPE sy-msgv1
          ev_msgv2 TYPE sy-msgv2
          ev_msgv3 TYPE sy-msgv3
          ev_msgv4 TYPE sy-msgv4.
ENDCLASS.



CLASS zcl_abaptags_message_helper IMPLEMENTATION.

  METHOD set_msg_vars_for_clike.

    split_text(
      EXPORTING iv_text  = iv_text
      IMPORTING ev_msgv1 = DATA(lv_msgv1)
                ev_msgv2 = DATA(lv_msgv2)
                ev_msgv3 = DATA(lv_msgv3)
                ev_msgv4 = DATA(lv_msgv4)
    ).

    MESSAGE e001(00) WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4
                     INTO DATA(lv_dummy) ##needed.
  ENDMETHOD.

  METHOD split_text.

    CONSTANTS:
      lc_length_of_msgv           TYPE i VALUE 50,
      lc_offset_of_last_character TYPE i VALUE 49.

    TYPES:
      ty_char200 TYPE c LENGTH 200.

    DATA:
      lv_text    TYPE ty_char200,
      lv_msg_var TYPE c LENGTH lc_length_of_msgv,
      lv_rest    TYPE ty_char200,
      lv_index   TYPE syst-index.

    lv_text = iv_text.

    DO 4 TIMES.

      lv_index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = lc_length_of_msgv
          text   = lv_text
        IMPORTING
          line   = lv_msg_var
          rest   = lv_rest.

      IF lv_msg_var+lc_offset_of_last_character = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        lv_rest = | { lv_rest }|.
      ENDIF.

      lv_text = lv_rest.

      CASE lv_index.
        WHEN 1.
          ev_msgv1 = lv_msg_var.
        WHEN 2.
          ev_msgv2 = lv_msg_var.
        WHEN 3.
          ev_msgv3 = lv_msg_var.
        WHEN 4.
          ev_msgv4 = lv_msg_var.
      ENDCASE.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
