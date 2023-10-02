*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
TYPES:
  BEGIN OF ty_line_index,
    number TYPE i,
    offset TYPE i,
    length TYPE i,
  END OF ty_line_index,
  ty_line_indexes TYPE TABLE OF ty_line_index WITH KEY number
                                              WITH UNIQUE HASHED KEY offset COMPONENTS offset.

CLASS lcl_source_code_util DEFINITION.
  PUBLIC SECTION.
    "! Transforms tabular source into single string
    CLASS-METHODS transform_to_string
      IMPORTING
        source_table TYPE string_table
        line_feed    TYPE string
      EXPORTING
        source_text  TYPE string
        indexes      TYPE ty_line_indexes.

    "! Determines the line indexes in the source
    CLASS-METHODS determine_line_indexes
      IMPORTING
        source_text   TYPE string       OPTIONAL
        source_table  TYPE string_table OPTIONAL
        line_feed     TYPE string
      RETURNING
        VALUE(result) TYPE ty_line_indexes.

    "! Retrieves Line index by a given multi-line source offset
    CLASS-METHODS get_line_index_by_offset
      IMPORTING
        line_indexes  TYPE ty_line_indexes
        !offset       TYPE match_result-offset
      RETURNING
        VALUE(result) TYPE ty_line_index.
ENDCLASS.
