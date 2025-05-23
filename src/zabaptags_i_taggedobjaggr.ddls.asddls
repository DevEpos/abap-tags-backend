@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZATAGSITGOBJAGR'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Tagged Objects grouped by tag/object'

define view ZAbapTags_I_TaggedObjAggr
  as select distinct from zabaptags_tgobjn

{
  tag_id,
  object_name,
  object_type,
  component_name,
  component_type
}
