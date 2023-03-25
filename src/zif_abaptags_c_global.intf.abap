"! <p class="shorttext synchronized" lang="en">Global constants for ABAP Tags</p>
INTERFACE zif_abaptags_c_global
  PUBLIC .
  CONSTANTS:
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
      function       TYPE trobjtype VALUE 'FUNC',
      function_group TYPE trobjtype VALUE 'FUGR',
      program        TYPE trobjtype VALUE 'PROG',
      package        TYPE trobjtype VALUE 'DEVC',
      class          TYPE trobjtype VALUE 'CLAS',
    END OF object_types,

    BEGIN OF wb_object_types,
      local_class     TYPE swo_objtyp VALUE 'CLAS/OCL',
      local_interface TYPE swo_objtyp VALUE 'CLAS/ON',
    END OF wb_object_types.

ENDINTERFACE.
