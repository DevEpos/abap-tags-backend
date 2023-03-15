@AbapCatalog.sqlViewName: 'ZABAPTAGSICTGOB'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tagged Objects of lower tree levels'

define view ZAbapTags_I_ChildTgObj
  as select from zabaptags_tags            as tag
    inner join   zabaptags_tags            as child on  tag.root_tag_id = child.root_tag_id
                                                    and tag.tree_level  < child.tree_level
    inner join   ZAbapTags_I_TaggedObjAggr as aggr  on aggr.tag_id = child.tag_id
{
  tag.tag_id,
  tag.name,
  aggr.object_name,
  aggr.object_type
}
