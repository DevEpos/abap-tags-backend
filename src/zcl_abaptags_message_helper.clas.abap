CLASS zcl_abaptags_message_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      set_msg_vars_for_clike
        IMPORTING
          text TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      ty_message TYPE c LENGTH 200.

    CONSTANTS:
      c_length_of_msgv           TYPE i VALUE 50,
      c_offset_of_last_character TYPE i VALUE 49.

    CLASS-METHODS:
      split_text
        IMPORTING
          text         TYPE string
        EXPORTING
          VALUE(msgv1) TYPE sy-msgv1
          VALUE(msgv2) TYPE sy-msgv2
          VALUE(msgv3) TYPE sy-msgv3
          VALUE(msgv4) TYPE sy-msgv4.
ENDCLASS.



CLASS zcl_abaptags_message_helper IMPLEMENTATION.


  METHOD set_msg_vars_for_clike.

    split_text(
      EXPORTING text  = text
      IMPORTING msgv1 = DATA(msgv1)
                msgv2 = DATA(msgv2)
                msgv3 = DATA(msgv3)
                msgv4 = DATA(msgv4)
    ).

    MESSAGE e001(00) WITH msgv1 msgv2 msgv3 msgv4
                     INTO DATA(dummy) ##needed.
  ENDMETHOD.


  METHOD split_text.
    DATA: tmp_text TYPE ty_message,
          msg_var  TYPE c LENGTH c_length_of_msgv,
          rest     TYPE ty_message,
          index    TYPE syst-index.

    tmp_text = text.

    DO 4 TIMES.

      index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = c_length_of_msgv
          text   = tmp_text
        IMPORTING
          line   = msg_var
          rest   = rest.

      IF msg_var+c_offset_of_last_character = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        rest = | { rest }|.
      ENDIF.

      tmp_text = rest.

      CASE index.
        WHEN 1.
          msgv1 = msg_var.
        WHEN 2.
          msgv2 = msg_var.
        WHEN 3.
          msgv3 = msg_var.
        WHEN 4.
          msgv4 = msg_var.
      ENDCASE.

    ENDDO.

  ENDMETHOD.


ENDCLASS.
