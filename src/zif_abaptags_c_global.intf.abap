"! <p class="shorttext synchronized" lang="en">Global constants for ABAP Tags</p>
INTERFACE zif_abaptags_c_global
  PUBLIC .
  CONSTANTS:
    "! Possible scopes for reading tags or tagged objects
    BEGIN OF c_scopes,
      all    TYPE string VALUE 'all',
      global TYPE string VALUE 'global',
      user   TYPE string VALUE 'user',
    END OF c_scopes,

    "! Possible types for reading tag info of tagged objects
    BEGIN OF c_tag_info_types,
      all      TYPE string VALUE 'all',
      children TYPE string VALUE 'children',
    END OF c_tag_info_types,

    "! Possible types of query for a tag search
    BEGIN OF c_tag_query_types,
      object_name            TYPE string VALUE 'objectName',
      object_uri             TYPE string VALUE 'objectUri',
      object_name_type_combo TYPE string VALUE 'objectNameTypeCombo',
    END OF c_tag_query_types,

    "! Possible value for the focus of the tag query string
    BEGIN OF c_tag_query_focus,
      object        TYPE string VALUE 'object',
      parent_object TYPE string VALUE 'parentObject',
    END OF c_tag_query_focus.

ENDINTERFACE.
