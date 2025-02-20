"! <p class="shorttext synchronized">Provides available features</p>
CLASS zcl_abaptags_adt_res_features DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_feature_value_type,
        boolean TYPE string VALUE 'Boolean',
        string  TYPE string VALUE 'String',
      END OF c_feature_value_type.

    CONSTANTS:
      BEGIN OF c_feature_categories,
        request_attribute TYPE string VALUE 'RequestAttribute',
      END OF c_feature_categories.
ENDCLASS.


CLASS zcl_abaptags_adt_res_features IMPLEMENTATION.
  METHOD get.
    DATA(features) = VALUE zif_abaptags_ty_adt_types=>ty_adt_plugin_features(
                               category = c_feature_categories-request_attribute
                               ( endpoint    = zcl_abaptags_adt_disc_app=>uris-tagged_object_info_list
                                 name        = 'considerOnlyDeletedObjects'
                                 type        = c_feature_value_type-boolean
                                 enabled     = abap_true
                                 description = 'Flag to return only deleted TADIR objects' ) ).

    response->set_body_data(
        content_handler = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZABAPTAGS_ADT_PLUGIN_FEATURES'
                                                                   root_name = 'PLUGIN_FEATURES' )
        data            = features ).
  ENDMETHOD.
ENDCLASS.
