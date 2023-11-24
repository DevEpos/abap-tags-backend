![](https://img.shields.io/badge/ABAP-v7.40sp08+-green)
[![abap package version](https://img.shields.io/endpoint?url=https://shield.abap.space/version-shield-json/github/DevEpos/abap-tags-backend/src/zif_abaptags_version.intf.abap/version&label=version)](https://github.com/DevEpos/abap-tags-backend/blob/main/src/zif_abaptags_version.intf.abap)

# abap-tags-backend

This Repository contains the ADT Backend that is needed to use the eclipse plugin
[ABAP Tags](https://github.com/DevEpos/eclipse-adt-plugins/tree/main/features/tags)

## Installation

Install this repository using [abapGit](https://github.com/abapGit/abapGit#abapgit).

## Upgrade to v2.0.0

> **Warning**  

Due to the introduction of new database tables it is necessary to execute the program `ZABAPTAGS_MIGR_v2_0` if you are upgrading from a previous version.  
It is _not_ required for new installations.

## Necessary Authorizations

To access the backend from ADT a user must have the following authorizations
Authorization Object | Authorization Field | Value
---------------------| ------------------- | -----
S_ADT_RES            | URI                 | /devepos/adt/atm/*
