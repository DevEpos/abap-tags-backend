@AbapCatalog.sqlViewName: 'ZABAPTAGSITAG'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ABAP Tag'

define view ZAbapTags_I_Tag
  as select from zabaptags_tags
{
  key tag_id            as TagId,
      name              as Name,
      name_upper        as NameUpper,
      description       as Description,
      parent_tag_id     as ParentTagId,
      owner             as Owner,
      is_shared         as IsShared,
      created_by        as CreatedBy,
      created_date_time as CreatedDateTime,
      changed_by        as ChangedBy,
      changed_date_time as ChangedDateTime
}
