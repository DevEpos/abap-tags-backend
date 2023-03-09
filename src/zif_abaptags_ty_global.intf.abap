"! <p class="shorttext synchronized" lang="en">Global Types for ABAP Tags</p>
INTERFACE zif_abaptags_ty_global
  PUBLIC .

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Range of Tag Ids</p>
    ty_tag_id_range      TYPE RANGE OF zabaptags_tag_id,

    "! <p class="shorttext synchronized" lang="en">Range of Tag Name</p>
    ty_tag_name_range    TYPE RANGE OF zabaptags_tag_name,

    "! <p class="shorttext synchronized" lang="en">Range of Owner</p>
    ty_owner_range       TYPE RANGE OF uname,

    "! <p class="shorttext synchronized" lang="en">Range of object name</p>
    ty_obj_name_range    TYPE RANGE OF sobj_name,

    "! <p class="shorttext synchronized" lang="en">Range of object type</p>
    ty_obj_type_range    TYPE RANGE OF trobjtype,

    "! <p class="shorttext synchronized" lang="en">List of Shared Tags DB entries</p>
    ty_db_shared_tags    TYPE STANDARD TABLE OF zabaptags_shtags WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">List of Tag masterdata</p>
    ty_db_tags           TYPE STANDARD TABLE OF zabaptags_tags WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">List of tagged objects (DB)</p>
    ty_db_tagged_objects TYPE STANDARD TABLE OF zabaptags_tgobjn WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Range of TADIR object name</p>
    ty_wb_objname_range  TYPE RANGE OF tadir-obj_name,

    "! <p class="shorttext synchronized" lang="en">Range of TADIR object type</p>
    ty_wb_objtype_range  TYPE RANGE OF tadir-object,

    "! <p class="shorttext synchronized" lang="en">Range of function module name</p>
    ty_func_module_range TYPE RANGE OF tfdir-funcname,

    "! <p class="shorttext synchronized" lang="en">Function module info</p>
    BEGIN OF ty_func_module_info,
      function TYPE tfdir-funcname,
      program  TYPE tfdir-pname,
      group    TYPE rs38l_area,
    END OF ty_func_module_info,

    "! <p class="shorttext synchronized" lang="en">List of function module infos</p>
    ty_func_module_infos TYPE STANDARD TABLE OF ty_func_module_info
      WITH EMPTY KEY
      WITH UNIQUE SORTED KEY function COMPONENTS function,

    "! <p class="shorttext synchronized" lang="en">TADIR key</p>
    BEGIN OF ty_tadir_key,
      type TYPE tadir-object,
      name TYPE tadir-obj_name,
    END OF ty_tadir_key,

    "! <p class="shorttext synchronized" lang="en">List of TADIR key entries</p>
    ty_tadir_keys TYPE STANDARD TABLE OF ty_tadir_key WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Extract of table TADIR</p>
    BEGIN OF ty_tadir_info,
      name         TYPE tadir-obj_name,
      type         TYPE tadir-object,
      package_name TYPE tadir-devclass,
      author       TYPE tadir-author,
    END OF ty_tadir_info,

    "! <p class="shorttext synchronized" lang="en">List of simple TADIR infos</p>
    ty_tadir_infos TYPE STANDARD TABLE OF ty_tadir_info WITH EMPTY KEY
      WITH NON-UNIQUE SORTED KEY name_type COMPONENTS type name,

    "! <p class="shorttext synchronized" lang="en">Count of tagged objects for tag id</p>
    BEGIN OF ty_tag_count,
      tag_id TYPE zabaptags_tag_id,
      count  TYPE sy-tabix,
    END OF ty_tag_count,

    "! <p class="shorttext synchronized" lang="en">List of tagged objects for tag id</p>
    ty_tag_counts TYPE HASHED TABLE OF ty_tag_count WITH UNIQUE KEY tag_id,

    "! <p class="shorttext synchronized" lang="en">Ref Tag data for tag id</p>
    BEGIN OF ty_tag_data,
      tag_id TYPE zabaptags_tag_id,
      data   TYPE REF TO zabaptags_tag_data,
    END OF ty_tag_data,

    "! <p class="shorttext synchronized" lang="en">List of Tag data accessible via tag id</p>
    ty_tag_data_map TYPE HASHED TABLE OF ty_tag_data WITH UNIQUE KEY tag_id,

    "! <p class="shorttext synchronized" lang="en">Structure represenation of tag id</p>
    BEGIN OF ty_tag_id,
      tag_id TYPE zabaptags_tag_id,
    END OF ty_tag_id,

    "! <p class="shorttext synchronized" lang="en">List of tag id structure</p>
    ty_tag_ids TYPE TABLE OF ty_tag_id,

    "! <p class="shorttext synchronized" lang="en">Information about tag</p>
    BEGIN OF ty_tag_info,
      tag_id         TYPE zabaptags_tag_id,
      name           TYPE zabaptags_tag_name,
      parent_tag_id  TYPE zabaptags_tag_id,
      owner          TYPE responsibl,
      full_hierarchy TYPE string,
    END OF ty_tag_info,

    "! <p class="shorttext synchronized" lang="en">List of tag infos</p>
    ty_tag_infos TYPE STANDARD TABLE OF ty_tag_info WITH EMPTY KEY,

    "! <p class="shorttext synchronized" lang="en">Information about tagged object</p>
    BEGIN OF ty_tgobj_info,
      object_name TYPE sobj_name,
      object_type TYPE trobjtype,
      tag_id      TYPE zabaptags_tag_id,
      tag_name    TYPE zabaptags_tag_name,
      tag_owner   TYPE responsibl,
    END OF ty_tgobj_info,

    "! <p class="shorttext synchronized" lang="en">Information about child tag of tagged object</p>
    BEGIN OF ty_tgobj_child_info,
      tag_id             TYPE zabaptags_tag_id,
      tag_name           TYPE zabaptags_tag_name,
      parent_object_name TYPE sobj_name,
      parent_object_type TYPE trobjtype,
      parent_tag_id      TYPE zabaptags_tag_id,
    END OF ty_tgobj_child_info,

    "! <p class="shorttext synchronized" lang="en">List of tagged object information</p>
    ty_tgobj_infos       TYPE SORTED TABLE OF ty_tgobj_info WITH UNIQUE KEY object_name object_type tag_id,
    "! <p class="shorttext synchronized" lang="en">List of infos about child tag of tagged object</p>
    ty_tgobj_child_infos TYPE SORTED TABLE OF ty_tgobj_child_info
      WITH UNIQUE KEY tag_id parent_tag_id parent_object_type parent_object_name,

    "! <p class="shorttext synchronized" lang="en">Mesh to connect tagged object with child tagged object</p>
    BEGIN OF MESH ty_tgobj_info_mesh,
      objects       TYPE ty_tgobj_infos
        ASSOCIATION children TO child_objects ON  parent_object_name = object_name
                                              AND parent_object_type = object_type
                                              AND parent_tag_id = tag_id,
      child_objects TYPE ty_tgobj_child_infos
        ASSOCIATION parent TO objects ON  object_name = parent_object_name
                                      AND object_type = parent_object_type
                                      AND tag_id      = parent_tag_id,
    END OF MESH ty_tgobj_info_mesh.

ENDINTERFACE.
