# a2UI5 Apps - Generic Search Help / Generic Value Help

Popups with Search Helps, dynamically created based on imported values.

1) Generic DDIC Search help generator
2) Generic Value help generator
   
## 1) Generic DDIC Search help generator
### 1.1) Description
This app will generate a popup window based on the given DDIC searchelp ID. The popup renders all selection fields flagged for "selection" (Selection position). The result list gets rendered based on the fields flagged for "Display" (Display position). With the "Go" button you will run the search with the provide selection criteria. Selecting a record in result list will close the popup. The value flagged for "exporting" will be returned to the calling application.
By default each selection criteria contains a popup window in order to enter one or more logical expressions. 
### 1.2) App name (Class name)
Class: Z2UI5_CL_TOOL_APP_SHLP_GEN

### 1.3) Parameters
| Parameter Name          | Type       | Optional | Description                                                                              |
| ----------------------- | ---------- | -------- | ---------------------------------------------------------------------------------------- |
| IV_SHLP_ID              | Importing  |          | This is the ID of the DDIC searchhelp. It could be an elementary or collective searchelp |
| IV_POPUP_TITLE          | Importing  |          | This is the popop window title |
| IV_RESULT_FILTER_EXIT   | Importing  | Yes      | User-Exit in order to filter, restrict or enrich the searchhelp result. For more details, please refer to the comments in the "FACTORY" method |
| IV_SELOPT_PREFILL_EXIT  | Importing  | Yes      | User-Exit in order to prefill or adapt the searchhelp selection criteria. For more details, please refer to the comments in the "FACTORY" method |
| IT_SHLP_BLACKLIST       | Importing  | Yes      | This exclusion list can be used for collective searchhelp to hide some of the elementary searchhelp. The list contains the searchelp IDs to be hidden |
| IV_DEFAULT_SHLP_INDEX   | Importing  | Yes      | By default the first searchhelp of an collective searchhelp will be shown. With this parameter you can select another searchhelp to be shown first (Index: 1-x) |
| IT_SHLP_EXIT            | Importing  | Yes      | User-Exit in order to created your own deep value help on top of the seachhelp fields. For more details, please refer to the comments in the "FACTORY" method |
| IV_USE_DEEP_SHLP        | Importing  | Yes      | This parameter will active a deep searchhelp generator. This means, if the DDIC seachhelp field has a searchhelp assigned on data element level or the corresponding domain contains fix value, it will either generate a generic searchhelp (searchhelp assigned on data element) or value help (domain fix value) | 
| RESULT                  | Returning  |          | Returning instance of the Searchhelp app |

### 1.4 Demo
Demo app: Z2UI5_CL_TOOL_APP_09

## 2) Generic Value help generator

#### Demo

<img width="800" alt="image" src="https://github.com/oblomov-dev/a2UI5-generic_search_help/assets/102328295/17768c33-34f4-4369-a63e-e153b64cdacf">

<img width="800" alt="image" src="https://github.com/oblomov-dev/a2UI5-generic_search_help/assets/102328295/a43d8fc2-c0a9-49e3-b49a-a319fd578f96">

<img width="800" alt="image" src="https://github.com/oblomov-dev/a2UI5-generic_search_help/assets/102328295/46a56965-3dcc-4fdf-add1-7a5c198fb1e6">

<img width="800" alt="image" src="https://github.com/oblomov-dev/a2UI5-generic_search_help/assets/102328295/2d98b117-eac7-4383-bad9-35e6eaddaa17">


