@AbapCatalog.sqlViewName: 'ZABAPTAGSITGOBN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tagged Objects'

define view ZAbapTags_I_TgObjn
  as select from zabaptags_tgobjn
{
  key id                 as Id,
      object_type        as ObjectType,
      object_name        as ObjectName,
      component_name     as ComponentName,
      component_type     as ComponentType,
      tag_id             as TagId,
      parent_tag_id      as ParentTagId,
      parent_object_type as ParentObjectType,
      parent_object_name as ParentObjectName,
      tagged_by          as TaggedBy,
      tagged_date        as TaggedDate,
      'X'                as Dummy
}
