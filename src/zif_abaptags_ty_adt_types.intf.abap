"! <p class="shorttext synchronized">ADT Types for ABAP Tags</p>
INTERFACE zif_abaptags_ty_adt_types
  PUBLIC.
  TYPES:
    "! Describes a feature of an ADT plugin
    BEGIN OF ty_adt_plugin_feature,
      "! Name of a feature.
      name        TYPE string,
      "! URI endpoint where feature is used
      endpoint    TYPE string,
      "! The type of the feature. <br>
      "! e.g. Boolean, String, List
      type        TYPE string,
      "! Indicates if the feature is enabled or not
      enabled     TYPE abap_bool,
      "! Category of the feature. <br>
      "! e.g. Attribute, Parameter
      category    TYPE string,
      "! Optional description of the feature
      description TYPE string,
    END OF ty_adt_plugin_feature,

    "! List of ADT plugin features
    ty_adt_plugin_features TYPE STANDARD TABLE OF ty_adt_plugin_feature WITH EMPTY KEY.
ENDINTERFACE.
