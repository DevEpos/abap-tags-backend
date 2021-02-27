"! <p class="shorttext synchronized" lang="en">General Exception in ABAP Tags API</p>
CLASS zcx_abaptags_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    DATA msgv1 TYPE sy-msgv1 .
    DATA msgv2 TYPE sy-msgv2 .
    DATA msgv3 TYPE sy-msgv3 .
    DATA msgv4 TYPE sy-msgv4 .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE sy-msgv1 OPTIONAL
        !msgv2    TYPE sy-msgv2 OPTIONAL
        !msgv3    TYPE sy-msgv3 OPTIONAL
        !msgv4    TYPE sy-msgv4 OPTIONAL .

    "! <p class="shorttext synchronized" lang="en">Raise exception with text</p>
    "! @parameter error_text | <p class="shorttext synchronized" lang="en">Text</p>
    "! @parameter previous_exc | <p class="shorttext synchronized" lang="en">Previous exception</p>
    "! @raising zcx_abaptags_exception | <p class="shorttext synchronized" lang="en">Exception</p>
    CLASS-METHODS raise
      IMPORTING
        !error_text     TYPE string optional
        !previous_exc TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_abaptags_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
   CONSTANTS:
      c_generic_error_msg TYPE string VALUE `An error occured (ZCX_ABAPTAGS_EXCEPTION)` ##NO_TEXT.
ENDCLASS.



CLASS zcx_abaptags_exception IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->msgv1 = msgv1 .
    me->msgv2 = msgv2 .
    me->msgv3 = msgv3 .
    me->msgv4 = msgv4 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD raise.
    DATA: text TYPE string.

    IF error_text IS INITIAL.
      text = c_generic_error_msg.
    ELSE.
      text = error_text.
    ENDIF.

    zcl_abaptags_message_helper=>set_msg_vars_for_clike( text = text ).

    RAISE EXCEPTION TYPE zcx_abaptags_exception
      EXPORTING
        textid   = VALUE #(
          msgid = sy-msgid
          msgno = sy-msgno
          attr1 = 'MSGV1'
          attr2 = 'MSGV2'
          attr3 = 'MSGV3'
          attr4 = 'MSGV4'
        )
        msgv1    = sy-msgv1
        msgv2    = sy-msgv2
        msgv3    = sy-msgv3
        msgv4    = sy-msgv4
        previous = previous_exc.
  ENDMETHOD.
ENDCLASS.
