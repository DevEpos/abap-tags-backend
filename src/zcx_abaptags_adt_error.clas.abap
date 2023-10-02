"! <p class="shorttext synchronized">General Error in ADT API of ABAP Tags</p>
CLASS zcx_abaptags_adt_error DEFINITION
  PUBLIC
  INHERITING FROM cx_adt_rest
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF unknown_tgobj_action,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '013',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unknown_tgobj_action,

      BEGIN OF no_tgobj_id_supplied,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_tgobj_id_supplied,

      BEGIN OF unknown_tags_action,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unknown_tags_action,

      BEGIN OF chosen_tags_no_longer_exist,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF chosen_tags_no_longer_exist,

      BEGIN OF parents_of_chs_tags_deleted,
        msgid TYPE symsgid VALUE 'ZABPATAGS',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF parents_of_chs_tags_deleted,

      BEGIN OF tag_parent_tag_already_exists,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE 'MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF tag_parent_tag_already_exists,

      BEGIN OF tag_no_longer_exists,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF tag_no_longer_exists,

      BEGIN OF parent_tag_no_longer_exists,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF parent_tag_no_longer_exists,

      BEGIN OF global_tag_already_exists,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF global_tag_already_exists,

      BEGIN OF tags_persisting_failure,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF tags_persisting_failure,

      BEGIN OF tag_with_name_not_found,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF tag_with_name_not_found,

      BEGIN OF tag_already_exists,
        msgid TYPE symsgid VALUE 'ZABAPTAGS',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MSGV1',
        attr2 TYPE scx_attrname VALUE 'MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF tag_already_exists.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        textid      LIKE if_t100_message=>t100key           OPTIONAL
        !previous   LIKE previous                           OPTIONAL
        subtype     TYPE sadt_exc_type                      OPTIONAL
        msgv1       TYPE symsgv                             DEFAULT sy-msgv1
        msgv2       TYPE symsgv                             DEFAULT sy-msgv2
        msgv3       TYPE symsgv                             DEFAULT sy-msgv3
        msgv4       TYPE symsgv                             DEFAULT sy-msgv4
        !properties TYPE REF TO if_adt_exception_properties OPTIONAL.

    METHODS get_http_status REDEFINITION.
    METHODS get_namespace   REDEFINITION.
    METHODS get_type        REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_abaptags_adt_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous   = previous
                        subtype    = subtype
                        msgv1      = msgv1
                        msgv2      = msgv2
                        msgv3      = msgv3
                        msgv4      = msgv4
                        properties = properties ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

  METHOD get_http_status.
    result = cl_rest_status_code=>gc_server_error_internal.
  ENDMETHOD.

  METHOD get_namespace.
    result = 'com.devepos.adt'.
  ENDMETHOD.

  METHOD get_type.
    result = 'ABAPTagsFailure'.
  ENDMETHOD.
ENDCLASS.
