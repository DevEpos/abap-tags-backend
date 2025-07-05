@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AbapCatalog.sqlViewName: 'ZABAPTAGSISLCWT'

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Sub Level Components with Tag'

define view ZAbapTags_I_SubLvlCmpWTag
  as select from zabaptags_tgobjn as ChildTgobj

    inner join   zabaptags_tags   as ChildTag   on ChildTgobj.tag_id = ChildTag.tag_id

{
  ChildTgobj.tag_id             as ChildTagId,
  ChildTgobj.parent_tag_id      as ChildParentTagId,
  ChildTgobj.object_name        as ChildObjectName,
  ChildTgobj.object_type        as ChildObjectType,
  ChildTgobj.component_name     as ChildComponentName,
  ChildTgobj.component_type     as ChildComponentType,
  ChildTag.name                 as ChildTagName,
  ChildTag.name_upper           as ChildTagNameUpper,
  ChildTag.description          as ChildTagDescription,
  ChildTag.owner                as ChildTagOwner,
  ChildTgobj.parent_object_type as ChildParentObjectType,
  ChildTgobj.parent_object_name as ChildParentObjectName
}

where component_name <> ''
  and component_type <> ''
