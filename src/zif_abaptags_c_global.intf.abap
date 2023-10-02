"! <p class="shorttext synchronized">Global constants for ABAP Tags</p>
INTERFACE zif_abaptags_c_global
  PUBLIC.
  CONSTANTS:
    BEGIN OF message_types,
      error   TYPE string VALUE 'ERROR',
      warning TYPE string VALUE 'WARNING',
      info    TYPE string VALUE 'INFO',
    END OF message_types,

    "! The type of an ABAP Tag
    BEGIN OF tag_type,
      global TYPE string VALUE 'GLOBAL',
      user   TYPE string VALUE 'USER',
      shared TYPE string VALUE 'SHARED',
    END OF tag_type,

    "! Possible scopes for reading tags or tagged objects
    BEGIN OF scopes,
      all    TYPE string VALUE 'all',
      global TYPE string VALUE 'global',
      user   TYPE string VALUE 'user',
      shared TYPE string VALUE 'shared',
    END OF scopes,

    "! Possible types for reading tag info of tagged objects
    BEGIN OF tag_info_types,
      "! All tags that are assigned to an object
      all          TYPE string VALUE 'all',
      "! Only tags are returned that are included as part of a search query
      search_focus TYPE string VALUE 'searchFocus',
      "! Information about tagged child objects
      children     TYPE string VALUE 'children',
    END OF tag_info_types,

    "! Grouping level of tagged object search
    BEGIN OF search_result_group_level,
      by_object         TYPE string VALUE 'byObject',
      by_tag_and_object TYPE string VALUE 'byTagAndObject',
    END OF search_result_group_level,

    "! Possible types of query for a tag search
    BEGIN OF tag_query_types,
      object_name            TYPE string VALUE 'objectName',
      object_uri             TYPE string VALUE 'objectUri',
      object_name_type_combo TYPE string VALUE 'objectNameTypeCombo',
    END OF tag_query_types,

    "! Possible value for the focus of the tag query string
    BEGIN OF tag_query_focus,
      object        TYPE string VALUE 'object',
      parent_object TYPE string VALUE 'parentObject',
    END OF tag_query_focus,

    BEGIN OF object_types,
      function            TYPE trobjtype VALUE 'FUNC',
      function_group      TYPE trobjtype VALUE 'FUGR',
      program             TYPE trobjtype VALUE 'PROG',
      package             TYPE trobjtype VALUE 'DEVC',
      class               TYPE trobjtype VALUE 'CLAS',
      data_definition     TYPE trobjtype VALUE 'DDLS',
      behavior_definition TYPE trobjtype VALUE 'BDEF',
      structured_object   TYPE trobjtype VALUE 'STOB',
    END OF object_types,

    BEGIN OF wb_object_types,
      class_local_class     TYPE swo_objtyp VALUE 'CLAS/OCL',
      class_local_interface TYPE swo_objtyp VALUE 'CLAS/ON',
      prog_local_class      TYPE swo_objtyp VALUE 'PROG/PL',
      prog_local_interface  TYPE swo_objtyp VALUE 'PROG/PN',
      fugr_local_class      TYPE swo_objtyp VALUE 'FUGR/PL',
      fugr_local_interface  TYPE swo_objtyp VALUE 'FUGR/PN',
      function              TYPE swo_objtyp VALUE 'FUGR/FF',
      data_definition       TYPE swo_objtyp VALUE 'DDLS/DF',
    END OF wb_object_types.

ENDINTERFACE.
