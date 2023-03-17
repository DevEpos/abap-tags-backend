@AbapCatalog.sqlViewName: 'ZABAPTAGSISLOWT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sub Level Objects with Tag'

define view ZAbapTags_I_SubLvlObjWTag
  as select from    zabaptags_tgobjn   as ChildTgobj
    inner join      zabaptags_tags     as ChildTag      on ChildTgobj.tag_id = ChildTag.tag_id
    left outer join ZAbapTags_I_TgObjn as GrandChildObj on  ChildTgobj.object_name = GrandChildObj.ParentObjectName
                                                        and ChildTgobj.object_type = GrandChildObj.ParentObjectType
                                                        and ChildTgobj.tag_id      = GrandChildObj.ParentTagId
{
  ChildTgobj.tag_id                   as ChildTagId,
  ChildTgobj.parent_tag_id            as ChildParentTagId,
  ChildTgobj.object_name              as ChildObjectName,
  ChildTgobj.object_type              as ChildObjectType,
  ChildTag.name                       as ChildTagName,
  ChildTag.name_upper                 as ChildTagNameUpper,
  ChildTag.description                as ChildTagDescription,
  ChildTag.owner                      as ChildTagOwner,
  ChildTgobj.parent_object_type       as ChildParentObjectType,
  ChildTgobj.parent_object_name       as ChildParentObjectName,
  coalesce( GrandChildObj.Dummy, '' ) as HasGrandChildren

}
