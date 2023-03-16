"! <p class="shorttext synchronized" lang="en">Hold release independent DDLS Id's for Open SQL</p>
CLASS zcl_abaptags_ddls_id DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-DATA:
      "! Release dependent view name of DDLS ZAbapTags_I_RootTagsWOCnt
      view_i_root_tags_with_counts TYPE string READ-ONLY,
      "! Release dependent view name of DDLS ZAbapTags_I_TaggedObjAggr
      view_i_tagged_obj_aggr       TYPE string READ-ONLY.

    CLASS-METHODS class_constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abaptags_ddls_id IMPLEMENTATION.

  METHOD class_constructor.
    IF sy-saprl < 750.
      view_i_root_tags_with_counts = 'zabaptagsirtwoct'.
      view_i_tagged_obj_aggr = 'zatagsitgobjagr'.
    ELSE.
      view_i_root_tags_with_counts = 'zabaptags_i_roottagswocnt'.
      view_i_tagged_obj_aggr = 'zabaptags_i_taggedobjaggr'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
