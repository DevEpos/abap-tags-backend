"! <p class="shorttext synchronized">Serializer for child tags of ABAP Tag</p>
CLASS zcl_abaptags_adt_tags_serlzer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Deserializes child tags of tag</p>
    CLASS-METHODS deserialize
      IMPORTING
        !reader             TYPE REF TO if_sxml_reader
        transformation_name TYPE string
      EXPORTING
        tag                 TYPE zabaptags_tag_data.

    "! <p class="shorttext synchronized">Serializes child tags of tag</p>
    CLASS-METHODS serialize
      IMPORTING
        !writer             TYPE REF TO if_sxml_writer
        tag                 TYPE zabaptags_tag_data
        transformation_name TYPE string.
ENDCLASS.


CLASS zcl_abaptags_adt_tags_serlzer IMPLEMENTATION.
  METHOD deserialize.
    DATA child TYPE zabaptags_tag_data.
    FIELD-SYMBOLS <children> TYPE zabaptags_tag_data_t.

    reader->current_node( ).
    DATA(type) = reader->read_next_node( )->type.
    reader->push_back( ).

    IF type = if_sxml_node=>co_nt_element_close.
      RETURN.
    ENDIF.

    IF tag-child_tags IS INITIAL.
      tag-child_tags = NEW zabaptags_tag_data_t( ).
    ENDIF.
    ASSIGN tag-child_tags->* TO <children>.

    TRY.
        WHILE abap_true = abap_true.
          CLEAR child.

          type = reader->read_next_node( )->type.
          reader->push_back( ).
          IF type = if_sxml_node=>co_nt_element_close.
            EXIT.
          ENDIF.
          CALL TRANSFORMATION (transformation_name)
               SOURCE XML reader
               RESULT tag = child.
          APPEND child TO <children>.
        ENDWHILE.
      CATCH cx_sxml_parse_error ##NO_HANDLER.
        " silently ignored
    ENDTRY.
  ENDMETHOD.

  METHOD serialize.
    FIELD-SYMBOLS <children> TYPE zabaptags_tag_data_t.

    CHECK tag-child_tags IS BOUND.

    ASSIGN tag-child_tags->* TO <children>.

    IF <children> IS ASSIGNED.

      LOOP AT <children> ASSIGNING FIELD-SYMBOL(<child>).
        CALL TRANSFORMATION (transformation_name)
             SOURCE tag = <child>
             RESULT XML writer.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
