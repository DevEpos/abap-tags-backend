@AbapCatalog.sqlViewName: 'ZABAPTAGSIRTWOCT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Root tags with object counts'

define view ZAbapTags_I_RootTagsWOCnt
  as select from zabaptags_tags   as tag
    inner join   zabaptags_tagsrm as map   on tag.tag_id = map.root_tag_id
    inner join   zabaptags_tgobjn as tgobj on tgobj.tag_id = map.tag_id

{
  tag.tag_id as TagId,
  count(*)   as ObjectCount
}
group by
  tag.tag_id
