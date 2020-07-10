"! <p class="shorttext synchronized" lang="en">Serializer for child tags of ABAP Tag</p>
CLASS zcl_abaptags_adt_tags_serlzer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Deserializes child tags of tag</p>
    CLASS-METHODS deserialize
      IMPORTING
        io_reader           TYPE REF TO if_sxml_reader
        transformation_name TYPE string
      EXPORTING
        tag                 TYPE zabaptags_tag_data.
    "! <p class="shorttext synchronized" lang="en">Serializes child tags of tag</p>
    CLASS-METHODS serialize
      IMPORTING
        io_writer           TYPE REF TO if_sxml_writer
        tag                 TYPE zabaptags_tag_data
        transformation_name TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abaptags_adt_tags_serlzer IMPLEMENTATION.
  METHOD deserialize.
    DATA: ls_child TYPE zabaptags_tag_data.
    FIELD-SYMBOLS: <lt_children> TYPE zabaptags_tag_data_t.

    io_reader->current_node( ).
    DATA(lv_type) = io_reader->read_next_node( )->type.
    io_reader->push_back( ).

    IF lv_type = if_sxml_node=>co_nt_element_close.
      RETURN.
    ENDIF.

    IF tag-child_tags IS INITIAL.
      tag-child_tags = NEW zabaptags_tag_data_t( ).
    ENDIF.
    ASSIGN tag-child_tags->* TO <lt_children>.

    TRY.
        WHILE abap_true = abap_true.
          CLEAR ls_child.

          lv_type = io_reader->read_next_node( )->type.
          io_reader->push_back( ).
          IF lv_type = if_sxml_node=>co_nt_element_close.
            EXIT.
          ENDIF.
          CALL TRANSFORMATION (transformation_name)
            SOURCE XML io_reader
            RESULT tag = ls_child.
          APPEND ls_child TO <lt_children>.
        ENDWHILE.
      CATCH cx_sxml_parse_error ##NO_HANDLER.
        "silently ignored
    ENDTRY.
  ENDMETHOD.

  METHOD serialize.
    FIELD-SYMBOLS: <lt_children> TYPE zabaptags_tag_data_t.

    CHECK tag-child_tags IS BOUND.

    ASSIGN tag-child_tags->* TO <lt_children>.

    IF <lt_children> IS ASSIGNED.

      LOOP AT <lt_children> ASSIGNING FIELD-SYMBOL(<ls_child>).
        CALL TRANSFORMATION (transformation_name)
         SOURCE tag = <ls_child>
         RESULT XML io_writer.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
