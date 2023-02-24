@AbapCatalog.sqlViewName: 'ZATAGSITGOBJGR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tagged Objects grouped by tag/object'

define view ZAbapTags_I_TaggedObjGrouped
  as select distinct from zabaptags_tgobjn
{
  tag_id,
  object_name,
  object_type
}
