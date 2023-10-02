*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_source_code_util IMPLEMENTATION.
  METHOD determine_line_indexes.
    DATA l_source_table TYPE TABLE OF string.

    DATA(line_offset) = 0.

    DATA(line_feed_length) = strlen( line_feed ).

    IF source_text IS NOT INITIAL.
      SPLIT source_text AT line_feed INTO TABLE l_source_table.
    ELSE.
      l_source_table = source_table.
    ENDIF.

    LOOP AT l_source_table ASSIGNING FIELD-SYMBOL(<code_line>).
      DATA(line_length) = strlen( <code_line> ).
      result = VALUE #( BASE result
                        ( number = sy-tabix
                          offset = line_offset
                          length = line_length ) ).

      line_offset = line_offset + line_length + line_feed_length.
    ENDLOOP.
  ENDMETHOD.

  METHOD transform_to_string.
    CLEAR: source_text,
           indexes.

    indexes = determine_line_indexes( source_table = source_table
                                      line_feed    = line_feed ).

    source_text = concat_lines_of( table = source_table sep = line_feed ).
  ENDMETHOD.

  METHOD get_line_index_by_offset.
    LOOP AT line_indexes REFERENCE INTO DATA(index) WHERE offset > offset.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      result = line_indexes[ index->number - 1 ].
    ELSE.
      " offset must be in the last row
      result = line_indexes[ lines( line_indexes ) ].
    ENDIF.
  ENDMETHOD.
ENDCLASS.
