"! <p class="shorttext synchronized">Router for ABAP Tags</p>
CLASS zcl_abaptags_adt_disc_app DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-DATA:
      BEGIN OF uris,
        tagged_object_info_list TYPE string,
        tags                    TYPE string,
      END OF uris.

    CONSTANTS c_root_scheme TYPE string VALUE 'http://www.devepos.com/adt/atm'.
    CONSTANTS c_root_rel_scheme TYPE string VALUE 'http://www.devepos.com/adt/relations/atm'.
    CONSTANTS c_tags_uri TYPE string VALUE '/tags'.
    CONSTANTS c_tags_deletion_check_uri TYPE string VALUE '/tags/deletion/check'.
    CONSTANTS c_tags_share_uri TYPE string VALUE '/tags/share'.
    CONSTANTS c_object_tagging_uri TYPE string VALUE '/taggedobjects'.
    CONSTANTS c_del_tagged_objects_uri TYPE string VALUE '/taggedobjects/delete'.
    CONSTANTS c_tagged_object_search_uri TYPE string VALUE '/taggedobjects/search'.
    CONSTANTS c_tagged_object_info_list_uri TYPE string VALUE '/taggedobjects/infos'.
    CONSTANTS c_tagged_object_del_check_uri TYPE string VALUE '/taggedobjects/deletion/check'.
    CONSTANTS c_tagged_object_tree_srv_uri TYPE string VALUE '/taggedobjects/tree/contents'.
    CONSTANTS c_tag_export_uri TYPE string VALUE '/tags/export'.
    CONSTANTS c_tag_import_uri TYPE string VALUE '/tags/import'.
    CONSTANTS c_static_uri TYPE string VALUE '/devepos/adt/atm'.

    CLASS-METHODS class_constructor.

    METHODS if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.

  PROTECTED SECTION.
    METHODS get_application_title REDEFINITION.
    METHODS register_resources    REDEFINITION.
    METHODS fill_router           REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_app_title TYPE string VALUE 'ADT Discovery for ABAP Tags'.
    CONSTANTS c_tags_management_handler TYPE string VALUE 'ZCL_ABAPTAGS_ADT_RES_TAGS'.

    "! <p class="shorttext synchronized">Register resources for managing abap tags</p>
    METHODS register_tag_management
      IMPORTING
        registry TYPE REF TO if_adt_disc_rest_rc_registry.

    "! <p class="shorttext synchronized">Register resources for abap tag search</p>
    METHODS register_tag_search
      IMPORTING
        registry TYPE REF TO if_adt_disc_rest_rc_registry.

    "! <p class="shorttext synchronized">Register resources for tagging objects</p>
    METHODS register_object_tagging_res
      IMPORTING
        registry TYPE REF TO if_adt_disc_rest_rc_registry.

    METHODS register_tag_tree_services
      IMPORTING
        registry TYPE REF TO if_adt_disc_rest_rc_registry.

    METHODS register_plugin_features
      IMPORTING
        registry TYPE REF TO if_adt_disc_rest_rc_registry.

    METHODS register_import_export
      IMPORTING
        registry TYPE REF TO if_adt_disc_rest_rc_registry.
ENDCLASS.


CLASS zcl_abaptags_adt_disc_app IMPLEMENTATION.
  METHOD class_constructor.
    uris-tagged_object_info_list = |{ c_static_uri }{ c_tagged_object_info_list_uri }|.
    uris-tags                    = |{ c_static_uri }{ c_tags_uri }|.
  ENDMETHOD.

  METHOD fill_router.
    super->fill_router( CHANGING router = router ).
    router->attach( iv_template      = '/discovery'
                    iv_handler_class = cl_adt_res_discovery=>co_class_name ).
  ENDMETHOD.

  METHOD get_application_title.
    result = c_app_title.
  ENDMETHOD.

  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = c_static_uri.
  ENDMETHOD.

  METHOD register_resources.
    register_tag_management( registry ).
    register_tag_search( registry ).
    register_tag_tree_services( registry ).
    register_object_tagging_res( registry ).
    register_import_export( registry ).
    register_plugin_features( registry ).
  ENDMETHOD.

  METHOD register_object_tagging_res.
    CONSTANTS c_previewinfo_uri_part TYPE string VALUE 'previewInfo'.

    DATA(collection) = registry->register_discoverable_resource( url             = c_object_tagging_uri
                                                                 handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TGOBJ'
                                                                 description     = 'Object Tagging'
                                                                 category_scheme = c_root_scheme
                                                                 category_term   = 'objecttagging' ).

    registry->register_resource( template      = |{ c_object_tagging_uri }/\{objectUri\}|
                                 handler_class = 'ZCL_ABAPTAGS_ADT_RES_TGOBJ' ).

    collection->register_disc_res_w_template(
        relation      = |{ c_root_rel_scheme }{ c_object_tagging_uri }/{ c_previewinfo_uri_part }|
        template      = |{ c_object_tagging_uri }/{ c_previewinfo_uri_part }|
        handler_class = 'ZCL_ABAPTAGS_ADT_RES_TAGPREV' ).

    registry->register_discoverable_resource( url             = c_tagged_object_del_check_uri
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TGOBJDCHK'
                                              description     = 'Tagged Object Deletion Check'
                                              category_scheme = c_root_scheme
                                              category_term   = 'taggedObjectDeletionCheck' ).

    registry->register_discoverable_resource( url             = c_del_tagged_objects_uri
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TGOBJDEL'
                                              description     = 'Tagged Object Deletion'
                                              category_scheme = c_root_scheme
                                              category_term   = 'taggedObjectDeletion' ).
  ENDMETHOD.

  METHOD register_tag_management.
    DATA collection TYPE REF TO if_adt_discovery_collection.

    registry->register_discoverable_resource( url             = c_tags_uri
                                              handler_class   = c_tags_management_handler
                                              description     = 'Tags'
                                              category_scheme = c_root_scheme
                                              category_term   = 'tags' ).

    registry->register_discoverable_resource( url             = c_tags_deletion_check_uri
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TAGDELCHK'
                                              description     = 'Tag Deletion Check'
                                              category_scheme = c_root_scheme
                                              category_term   = 'tagsDeletionCheck' ).

    collection = registry->register_discoverable_resource( url             = c_tags_share_uri
                                                           handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TAGSSHARE'
                                                           description     = 'Share Tags'
                                                           category_scheme = c_root_scheme
                                                           category_term   = 'shareTags' ).

    collection->register_disc_res_w_template( relation      = |{ c_root_rel_scheme }{ c_tags_share_uri }/info|
                                              template      = |{ c_tags_share_uri }/info/\{tagId\}|
                                              description   = 'Details of Shared Tag'
                                              handler_class = 'ZCL_ABAPTAGS_ADT_RES_TAGSSHARE' ).
  ENDMETHOD.

  METHOD register_tag_search.
    registry->register_discoverable_resource( url             = c_tagged_object_search_uri
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TGOBJSRCH'
                                              description     = 'Tagged Object Search'
                                              category_scheme = c_root_scheme
                                              category_term   = 'taggedobjectsearch' ).

    registry->register_discoverable_resource( url             = c_tagged_object_info_list_uri
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TGOBJLIST'
                                              description     = 'Tagged Object Info Retriever'
                                              category_scheme = c_root_scheme
                                              category_term   = 'taggedObjectInfos' ).
  ENDMETHOD.

  METHOD register_tag_tree_services.
    registry->register_discoverable_resource( url             = c_tagged_object_tree_srv_uri
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TGOBJTSRV'
                                              description     = 'Tagged Object Tree Contents'
                                              category_scheme = c_root_scheme
                                              category_term   = 'taggedobjecttree' ).
  ENDMETHOD.

  METHOD register_plugin_features.
    registry->register_discoverable_resource( url             = '/pluginfeatures'
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_FEATURES'
                                              description     = 'Available Plugin features'
                                              category_scheme = c_root_scheme
                                              category_term   = 'pluginFeatures' ).
  ENDMETHOD.

  METHOD register_import_export.
    registry->register_discoverable_resource( url             = c_tag_export_uri
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TAGEXPORT'
                                              description     = 'Exported ABAP Tags & Tagged Objects'
                                              category_scheme = c_root_scheme
                                              category_term   = 'tagExport' ).
    registry->register_discoverable_resource( url             = c_tag_import_uri
                                              handler_class   = 'ZCL_ABAPTAGS_ADT_RES_TAGIMPORT'
                                              description     = 'Importing ABAP Tags & Tagged Objects'
                                              category_scheme = c_root_scheme
                                              category_term   = 'tagImport' ).
  ENDMETHOD.
ENDCLASS.
