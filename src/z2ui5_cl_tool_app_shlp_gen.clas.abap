CLASS z2ui5_cl_tool_app_shlp_gen DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object .
    INTERFACES z2ui5_if_app .

    TYPES:
      BEGIN OF ts_token,
        key      TYPE string,
        text     TYPE string,
        visible  TYPE abap_bool,
        selkz    TYPE abap_bool,
        editable TYPE abap_bool,
      END OF ts_token .
    TYPES:
      tt_token TYPE STANDARD TABLE OF ts_token WITH KEY key .
    TYPES:
      tt_range TYPE RANGE OF string .
    TYPES:
      ts_range TYPE LINE OF tt_range .
    TYPES:
      BEGIN OF ts_filter_pop,
        option TYPE string,
        low    TYPE string,
        high   TYPE string,
        key    TYPE string,
      END OF ts_filter_pop .
    TYPES:
      tt_filter_prop TYPE STANDARD TABLE OF ts_filter_pop WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_selopt_mapping,
        key   TYPE string,
        text  TYPE string,
        value TYPE string,
      END OF ts_selopt_mapping .
    TYPES:
      tt_selopt_mapping TYPE STANDARD TABLE OF ts_selopt_mapping WITH KEY key .
    TYPES:
      BEGIN OF ts_shlp_descr.
        INCLUDE TYPE shlp_descr. "Can be replaced by local def. for downport
    TYPES: END OF ts_shlp_descr .
    TYPES:
      tt_shlp_descr TYPE STANDARD TABLE OF ts_shlp_descr WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_shlp_blacklist,
        shlp_id TYPE char30,
      END OF ts_shlp_blacklist .
    TYPES:
      tt_shlp_blacklist TYPE STANDARD TABLE OF ts_shlp_blacklist WITH KEY shlp_id .
    TYPES:
      BEGIN OF ts_shlp_type,
        type  TYPE string,
        id    TYPE string,
        label TYPE string,
      END OF ts_shlp_type .
    TYPES:
      BEGIN OF ts_shlp_exit,
        rollname TYPE rollname,
        exit     TYPE string,
      END OF ts_shlp_exit .
    TYPES:
      tt_shlp_exit TYPE STANDARD TABLE OF ts_shlp_exit WITH KEY rollname .

    TYPES: ts_dd07v TYPE dd07v.
    TYPES: tt_dd07v TYPE STANDARD TABLE OF dd07v WITH DEFAULT KEY.

    DATA mv_check_initialized TYPE abap_bool .
    DATA mv_shlp_id TYPE char30 .
    DATA mv_popup_title TYPE string .
    DATA mv_shlp_result TYPE string .
    DATA mv_shlp_result2 TYPE string .
    DATA mv_shlp_result3 TYPE string .
    DATA mt_filter TYPE tt_filter_prop .
    DATA mt_mapping TYPE tt_selopt_mapping .
    DATA:
      BEGIN OF ms_screen,
        shlp_selkey TYPE char30,
      END OF ms_screen .
    DATA mr_shlp_fields_1 TYPE REF TO data .
    DATA mr_shlp_fields_2 TYPE REF TO data .
    DATA mr_shlp_fields_3 TYPE REF TO data .
    DATA mr_shlp_fields_4 TYPE REF TO data .
    DATA mr_shlp_fields_5 TYPE REF TO data .
    DATA mr_shlp_fields_6 TYPE REF TO data .
    DATA mr_shlp_fields_7 TYPE REF TO data .
    DATA mr_shlp_fields_8 TYPE REF TO data .
    DATA mr_shlp_fields_9 TYPE REF TO data .
    DATA mr_shlp_fields_10 TYPE REF TO data .
    DATA mr_shlp_result_1 TYPE REF TO data .
    DATA mr_shlp_result_2 TYPE REF TO data .
    DATA mr_shlp_result_3 TYPE REF TO data .
    DATA mr_shlp_result_4 TYPE REF TO data .
    DATA mr_shlp_result_5 TYPE REF TO data .
    DATA mr_shlp_result_6 TYPE REF TO data .
    DATA mr_shlp_result_7 TYPE REF TO data .
    DATA mr_shlp_result_8 TYPE REF TO data .
    DATA mr_shlp_result_9 TYPE REF TO data .
    DATA mr_shlp_result_10 TYPE REF TO data .
    CONSTANTS mc_evt_shlp_close TYPE string VALUE 'EVT_SHLP_CLOSE' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_go TYPE string VALUE 'EVT_SHLP_GO' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_select TYPE string VALUE 'EVT_SHLP_SELECT' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_selopt_open TYPE string VALUE 'EVT_SHLP_SELOPT_OPEN' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_selopt_token_upd TYPE string VALUE 'EVT_SHLP_SELOPT_TOKEN_UPD' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_selopt_add TYPE string VALUE 'EVT_SHLP_SELOPT_ADD' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_selopt_cancel TYPE string VALUE 'EVT_SHLP_SELOPT_CANCEL' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_selopt_ok TYPE string VALUE 'EVT_SHLP_SELOPT_OK' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_selopt_delete TYPE string VALUE 'EVT_SHLP_SELOPT_DELETE' ##NO_TEXT.
    CONSTANTS mc_evt_shlp_selopt_delete_all TYPE string VALUE 'EVT_SHLP_SELOPT_DELETE_ALL' ##NO_TEXT.
    CONSTANTS mc_shlp_fields_ref_name TYPE string VALUE 'MR_SHLP_FIELDS_' ##NO_TEXT.
    CONSTANTS mc_shlp_result_ref_name TYPE string VALUE 'MR_SHLP_FIELDS_' ##NO_TEXT.
    DATA mv_result_filter_exit TYPE string .
    DATA mv_selopt_prefill_exit TYPE string .
    CONSTANTS mc_evt_shlp_selopt_change TYPE string VALUE 'EVT_SHLP_SELOPT_CHANGE' ##NO_TEXT.
    CONSTANTS mc_token_upd_type_remove TYPE string VALUE 'removed' ##NO_TEXT.

    CLASS-METHODS factory
      IMPORTING
        !iv_shlp_id             TYPE clike
        !iv_popup_title         TYPE clike
        !iv_result_filter_exit  TYPE clike OPTIONAL
        !iv_selopt_prefill_exit TYPE clike OPTIONAL
        !it_shlp_blacklist      TYPE tt_shlp_blacklist OPTIONAL
        !iv_default_shlp_index  TYPE i DEFAULT 1
        !it_shlp_exit           TYPE tt_shlp_exit OPTIONAL
        !iv_use_deep_shlp       TYPE boole_d OPTIONAL
      RETURNING
        VALUE(result)           TYPE REF TO z2ui5_cl_tool_app_shlp_gen .
    CLASS-METHODS template_vh_user_exit
      IMPORTING
        !iv_rollname TYPE rollname
      EXPORTING
        !es_config   TYPE z2ui5_cl_tool_app_vh_gen=>ts_config
        !et_data     TYPE z2ui5_cl_tool_app_vh_gen=>tt_data .
protected section.

  data MV_SELOPT_FIELDNAME type STRING .
  data MT_SHLP_DESCR type TT_SHLP_DESCR .
  data MT_SHLP_BLACKLIST type TT_SHLP_BLACKLIST .
  data MV_CHECK_POPUP_SHLP type BOOLE_D .
  data MV_CHECK_POPUP_FIX_VAL type BOOLE_D .
  data MV_CHECK_POPUP_SHLP_EXIT type BOOLE_D .
  constants MC_SHLP_TYPE_DDIC_SHLP type STRING value 'SHLP' ##NO_TEXT.
  constants MC_SHLP_TYPE_FIX_DOMVAL type STRING value 'FIX_DOM_VALUE' ##NO_TEXT.
  constants MC_SHLP_TYPE_SHLP_EXIT type STRING value 'SHLP_EXIT' ##NO_TEXT.
  data MV_DEFAULT_SHLP_INDEX type I .
  data MT_SHLP_EXIT type TT_SHLP_EXIT .
  data MV_USE_DEEP_SHLP type BOOLE_D .

  methods ON_RENDERING
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
  methods ON_EVENT
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
  methods GENERATE_DDIC_SHLP
    importing
      !IR_PARENT type ref to Z2UI5_CL_XML_VIEW
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT
      !IV_SHLP_ID type CHAR30 .
  methods SELECT_DDIC_SHLP
    importing
      !IR_CONTROLLER type ref to OBJECT
      !IV_SHLP_ID type CHAR30
      !IV_MAXROWS type I default 150 .
  methods BUILD_DATA_REF
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT
      !IT_SHLP_DESCR type TT_SHLP_DESCR .
  methods GENERATE_DDIC_SHLP_SELOPT
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT
      !IV_FIELDNAME type CLIKE
      !IV_SHLP_ID type CHAR30 .
  methods GET_SHLP_RANGE_BY_VALUE
    importing
      !IV_VALUE type STRING
    returning
      value(RS_RESULT) type TS_RANGE .
  methods GET_SHLP_UUID
    returning
      value(RV_RESULT) type STRING .
  methods GET_SELOPT_MAPPING
    returning
      value(RT_MAPPING) type TT_SELOPT_MAPPING .
  methods ON_INIT
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
  methods EXPAND_SEARCHHELP
    importing
      !IV_SHLP_ID type CHAR30
      !IT_SHLP_BLACKLIST type TT_SHLP_BLACKLIST
    exporting
      value(ET_SHLP_DESCR) type TT_SHLP_DESCR
      !EV_SHLP_SELKEY type CHAR30 .
  methods GET_DATA_REF
    importing
      !IV_INDEX type I optional
      !IV_SHLP_ID type CHAR30 optional
    exporting
      !ER_SHLP_FIELDS type ref to DATA
      !ER_SHLP_RESULT type ref to DATA .
  methods CREATE_DATA_REF
    importing
      !IV_INDEX type I
      !IR_STRUC_TYPE type ref to CL_ABAP_STRUCTDESCR
      !IR_TABLE_TYPE type ref to CL_ABAP_TABLEDESCR .
  methods INIT_DATA_REF .
  methods FILL_TOKEN
    importing
      !IT_FILTER type TT_FILTER_PROP
    changing
      !CT_TOKEN type TT_TOKEN .
  methods FILL_FILTER
    importing
      !IT_TOKEN type TT_TOKEN
    changing
      !CT_FILTER type TT_FILTER_PROP .
  methods GET_INPUT_FIELDNAME
    importing
      !IV_FIELDNAME type CHAR30
    returning
      value(RV_INPUT_FIELDNAME) type CHAR30 .
  methods DELETE_TOKEN
    importing
      !IV_TOKEN_KEY type CLIKE
    changing
      !CT_TOKEN type TT_TOKEN .
  methods GET_SHLP_TYPE
    importing
      !IV_SHLP_ID type CHAR30
      !IV_FIELDNAME type STRING
    returning
      value(RS_SHLP_TYPE) type TS_SHLP_TYPE .
  methods HANDLE_POPUP_RETURN
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT
    changing
      !CS_SHLP_FIELDS type DATA
      !CS_SHLP_RESULT type DATA .
  methods CONVERT_RESULT_TO_TOKEN
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT
      !IV_SHLP_FIELDNAME type STRING
      !IV_RESULT type STRING
    changing
      !CS_SHLP_FIELDS type DATA .
  methods GENERATE_VH_DOMAIN_FIX_VAL
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT
      !IS_SHLP_TYPE type TS_SHLP_TYPE .
  methods GENERATE_VH_USER_EXIT
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT
      !IS_SHLP_TYPE type TS_SHLP_TYPE .
private section.
ENDCLASS.



CLASS Z2UI5_CL_TOOL_APP_SHLP_GEN IMPLEMENTATION.


  METHOD build_data_ref.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lt_comp                    TYPE cl_abap_structdescr=>component_table,
          lr_shlp_fields_struct_type TYPE REF TO cl_abap_structdescr,
          lr_shlp_result_struct_type TYPE REF TO cl_abap_structdescr,
          lr_shlp_result_table_type  TYPE REF TO cl_abap_tabledescr,
          lt_token                   TYPE tt_token,
          lv_tabix                   TYPE i.

    FIELD-SYMBOLS: <ls_fielddescr> TYPE dfies,
                   <ls_comp>       LIKE LINE OF lt_comp,
                   <ls_shlp_descr> TYPE ts_shlp_descr.

* ---------- Init data reference ------------------------------------------------------------------
    me->init_data_ref( ).

* ---------- Loop over all searchhelps (max 10) ---------------------------------------------------
    LOOP AT it_shlp_descr ASSIGNING <ls_shlp_descr> TO 10.
* ---------- Init loop data -----------------------------------------------------------------------
      CLEAR: lt_comp, lr_shlp_fields_struct_type, lr_shlp_result_struct_type,
             lr_shlp_result_table_type, lv_tabix.
      UNASSIGN: <ls_comp>.

* ---------- Keep table index ---------------------------------------------------------------------
      lv_tabix = sy-tabix.

* -------------------------------------------------------------------------------------------------
* Build up search fields component table
* -------------------------------------------------------------------------------------------------
      CLEAR: lt_comp.
      UNASSIGN: <ls_comp>.
      LOOP AT <ls_shlp_descr>-fielddescr ASSIGNING <ls_fielddescr>.
        APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
        <ls_comp>-name = <ls_fielddescr>-fieldname.
        <ls_comp>-type ?= cl_abap_datadescr=>describe_by_data( lt_token ).
        UNASSIGN: <ls_comp>.
        APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
        <ls_comp>-name = me->get_input_fieldname( iv_fieldname = <ls_fielddescr>-fieldname ) .
        <ls_comp>-type ?= cl_abap_datadescr=>describe_by_name( 'STRING' ).
      ENDLOOP.

* ---------- Create structure using component table -----------------------------------------------
      lr_shlp_fields_struct_type = cl_abap_structdescr=>create( lt_comp ).

* -------------------------------------------------------------------------------------------------
* Build up search result component table
* -------------------------------------------------------------------------------------------------
      CLEAR: lt_comp.
      UNASSIGN: <ls_comp>.
      LOOP AT <ls_shlp_descr>-fielddescr ASSIGNING <ls_fielddescr>.
        APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
        <ls_comp>-name = <ls_fielddescr>-fieldname.
        <ls_comp>-type ?= cl_abap_datadescr=>describe_by_name( <ls_fielddescr>-rollname ).
      ENDLOOP.

* ---------- Create Dynamic table using component table -------------------------------------------
      lr_shlp_result_struct_type = cl_abap_structdescr=>create( lt_comp ).
      lr_shlp_result_table_type  = cl_abap_tabledescr=>create( p_line_type = lr_shlp_result_struct_type ).

* ---------- Create Dynamic searchhelp structure and table ----------------------------------------
      me->create_data_ref(  iv_index       = lv_tabix
                            ir_struc_type  = lr_shlp_fields_struct_type
                            ir_table_type  = lr_shlp_result_table_type ).

    ENDLOOP.
  ENDMETHOD.


  METHOD create_data_ref.
* -------------------------------------------------------------------------------------------------
* Heap References and Stack References issue!
* Internal tables are dynamic data objects and have a special role because they have their own
* memory management. They allocate and release memory regardless of the statement CREATE and
* Garbage Collector. This means that heap references to rows in internal tables can become invalid.
* Therfore I had to create fix amount of reference variables (placeholders),
* instead of ITAB with references.
* -------------------------------------------------------------------------------------------------
    CASE iv_index.
      WHEN 1.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_1 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_1 TYPE HANDLE ir_table_type.

      WHEN 2.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_2 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_2 TYPE HANDLE ir_table_type.
      WHEN 3.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_3 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_3 TYPE HANDLE ir_table_type.

      WHEN 4.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_4 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_4 TYPE HANDLE ir_table_type.

      WHEN 5.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_5 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_5 TYPE HANDLE ir_table_type.

      WHEN 6.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_6 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_6 TYPE HANDLE ir_table_type.

      WHEN 7.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_7 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_7 TYPE HANDLE ir_table_type.

      WHEN 8.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_8 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_8 TYPE HANDLE ir_table_type.

      WHEN 9.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_9 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_9 TYPE HANDLE ir_table_type.

      WHEN 10.
* ---------- Create Dynamic structure and table ---------------------------------------------------
        CREATE DATA me->mr_shlp_fields_10 TYPE HANDLE ir_struc_type.
        CREATE DATA me->mr_shlp_result_10 TYPE HANDLE ir_table_type.
    ENDCASE.
  ENDMETHOD.


  METHOD delete_token.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    FIELD-SYMBOLS: <ls_filter>      TYPE ts_filter_pop,
                   <ls_field_token> TYPE ts_token,
                   <lv_field>       TYPE any.

    LOOP AT ct_token ASSIGNING <ls_field_token>.
* ---------- Init loop data -----------------------------------------------------------------------
      UNASSIGN: <lv_field>.

* ---------- Get token key assignment -------------------------------------------------------------
      ASSIGN COMPONENT 'KEY' OF STRUCTURE <ls_field_token> TO <lv_field>.
      IF <lv_field> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

* ---------- Check token key to be deleted --------------------------------------------------------
      IF <lv_field> <> iv_token_key.
        CONTINUE.
      ENDIF.

* ---------- Delete token -------------------------------------------------------------------------
      DELETE TABLE ct_token FROM <ls_field_token>.
    ENDLOOP.

  ENDMETHOD.


  METHOD expand_searchhelp.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: ls_shlp       TYPE  shlp_descr,
          lt_shlp_descr TYPE tt_shlp_descr.

    FIELD-SYMBOLS: <ls_shlp_descr> 	TYPE ts_shlp_descr.

* ---------- Get searchhelp description for given ID ----------------------------------------------
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = iv_shlp_id
      IMPORTING
        shlp     = ls_shlp.

    IF ls_shlp IS INITIAL.
      RETURN.
    ENDIF.

* ---------- Get all elementary shlps for given collective shlp -----------------------------------
    CALL FUNCTION 'F4IF_EXPAND_SEARCHHELP'
      EXPORTING
        shlp_top = ls_shlp
      IMPORTING
        shlp_tab = lt_shlp_descr.

* ---------- Apply blacklist (Remove unwanted searchhelp from the list) ---------------------------
    LOOP AT it_shlp_blacklist ASSIGNING FIELD-SYMBOL(<ls_shlp_blacklist>).
      DELETE lt_shlp_descr WHERE shlpname = <ls_shlp_blacklist>-shlp_id.
    ENDLOOP.

* ---------- Fetch details for each searchhelp (F4IF_EXPAND_SEARCHHELP doesn't provide them all) --
    LOOP AT lt_shlp_descr ASSIGNING <ls_shlp_descr>.
* ---------- Init loop data -----------------------------------------------------------------------
      CLEAR: ls_shlp.

* ---------- Get searchhelp description for given ID ----------------------------------------------
      CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
        EXPORTING
          shlpname = <ls_shlp_descr>-shlpname
        IMPORTING
          shlp     = ls_shlp.

* ---------- Append searchhelp --------------------------------------------------------------------
      APPEND ls_shlp TO et_shlp_descr.
    ENDLOOP.

* ---------- Set default searchhelp ---------------------------------------------------------------
    IF line_exists( et_shlp_descr[ me->mv_default_shlp_index ] ).
      ev_shlp_selkey = et_shlp_descr[ me->mv_default_shlp_index ]-shlpname.
    ELSEIF line_exists( et_shlp_descr[ 1 ] ).
      ev_shlp_selkey = et_shlp_descr[ 1 ]-shlpname.
    ENDIF.
  ENDMETHOD.


  METHOD factory.
* ---------- Create new DDIC searchhelp instance --------------------------------------------------
    result = NEW #( ).

* ---------- Set searchhelp ID --------------------------------------------------------------------
    result->mv_shlp_id = iv_shlp_id.

* ---------- Set searchhelp poup title ------------------------------------------------------------
    result->mv_popup_title = iv_popup_title.

* ---------- Set searchhelp blacklist -------------------------------------------------------------
    result->mt_shlp_blacklist = it_shlp_blacklist.

* ---------- Set default searchhelp index (default is 1) -------------------------------------------
    result->mv_default_shlp_index = iv_default_shlp_index.

* -------------------------------------------------------------------------------------------------
* Set exit optional parameters (CLASS NAME=>METHOD NAME)
* Example
* Parameter value:
*   ZCL_EXIT_HANDLER_CLASS=>SELOPT_PREFILL_EXIT
* Method definition:
*class-methods FILTER_RESULT_EXIT
*    importing
*      !IV_SHLP_ID type CHAR30 optional
*    changing
*      !CT_RESULT type TABLE .
*
*  class-methods SELOPT_PREFILL_EXIT
*    importing
*      !IV_SHLP_ID type CHAR30 optional
*    changing
*      !CT_SELOPT type TABLE .
* -------------------------------------------------------------------------------------------------
    result->mv_selopt_prefill_exit = iv_selopt_prefill_exit.
    result->mv_result_filter_exit  = iv_result_filter_exit.

* -------------------------------------------------------------------------------------------------
* Set exit optional table parameter
* -> rollname (Name of the DDIC data element)
* -> exit (CLASS NAME=>METHOD NAME)
* Template Method: Z2UI5_CL_TOOL_SHLP_GEN=>TEMPLATE_VH_USER_EXIT()
* Example
* Parameter value:
*   ZCL_EXIT_HANDLER_CLASS=>SHLP_EXIT
* Method definition:
*   class-methods TEMPLATE_VH_USER_EXIT
*    importing
*      !IV_ROLLNAME type ROLLNAME
*    exporting
*      !ES_CONFIG type Z2UI5_CL_TOOL_APP_VH_GEN=>TS_CONFIG
*      !ET_DATA type Z2UI5_CL_TOOL_APP_VH_GEN=>TT_DATA .
* -------------------------------------------------------------------------------------------------
    result->mt_shlp_exit = it_shlp_exit.

* -------------------------------------------------------------------------------------------------
* Use deep searchhelp
*  -> If true and the seachhelp field contains a searchhelp (SHLP in data element or fix values on
*     domain level), it will generate another DDIC searchhelp or value help on top of the existing
*     searchhelp.
* -------------------------------------------------------------------------------------------------
    result->mv_use_deep_shlp = iv_use_deep_shlp.
  ENDMETHOD.


  METHOD fill_filter.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: ls_range TYPE ts_range.

    FIELD-SYMBOLS: <ls_filter>      TYPE ts_filter_pop,
                   <ls_field_token> TYPE ts_token,
                   <lv_field>       TYPE any.

* ---------- Init ---------------------------------------------------------------------------------
    CLEAR: ct_filter.

* ---------- Fill filter --------------------------------------------------------------------------
    IF it_token IS NOT INITIAL.
      LOOP AT it_token ASSIGNING <ls_field_token>.
* ---------- Init loop data -----------------------------------------------------------------------
        CLEAR: ls_range.
        UNASSIGN: <lv_field>.

* ---------- Get key value from token -------------------------------------------------------------
        ASSIGN COMPONENT 'KEY' OF STRUCTURE <ls_field_token> TO <lv_field>.
        IF <lv_field> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

* ---------- Convert token into range format ------------------------------------------------------
        ls_range = me->get_shlp_range_by_value( iv_value = <lv_field> ).
        IF ls_range IS INITIAL.
          CONTINUE.
        ENDIF.

* ---------- Build new filter record --------------------------------------------------------------
        APPEND INITIAL LINE TO me->mt_filter ASSIGNING <ls_filter>.
        <ls_filter>-key     = me->get_shlp_uuid( ).
        <ls_filter>-option  = ls_range-option.
        <ls_filter>-low     = ls_range-low.
        <ls_filter>-high    = ls_range-high.

      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD fill_token.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    FIELD-SYMBOLS: <ls_filter>      TYPE ts_filter_pop,
                   <ls_field_token> TYPE ts_token,
                   <lv_field>       TYPE any.

* ---------- Fill token ---------------------------------------------------------------------------
    LOOP AT it_filter REFERENCE INTO DATA(lr_filter).
      DATA(lv_value) = me->mt_mapping[ key = lr_filter->option ]-value.
      REPLACE `{LOW}`  IN lv_value WITH lr_filter->low.
      REPLACE `{HIGH}` IN lv_value WITH lr_filter->high.

      APPEND INITIAL LINE TO ct_token ASSIGNING <ls_field_token>.

      UNASSIGN: <lv_field>.
      ASSIGN COMPONENT 'KEY' OF STRUCTURE <ls_field_token> TO <lv_field>.
      IF <lv_field> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      <lv_field> = lv_value.

      UNASSIGN: <lv_field>.
      ASSIGN COMPONENT 'TEXT' OF STRUCTURE <ls_field_token> TO <lv_field>.
      IF <lv_field> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      <lv_field> = lv_value.

      UNASSIGN: <lv_field>.
      ASSIGN COMPONENT 'VISIBLE' OF STRUCTURE <ls_field_token> TO <lv_field>.
      IF <lv_field> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      <lv_field> = abap_true.

      UNASSIGN: <lv_field>.
      ASSIGN COMPONENT 'EDITABLE' OF STRUCTURE <ls_field_token> TO <lv_field>.
      IF <lv_field> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      <lv_field> = abap_true.

    ENDLOOP.
  ENDMETHOD.


  METHOD generate_ddic_shlp.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: ls_shlp           TYPE  shlp_descr,
          lv_grid_form_no   TYPE i,
          lt_arg            TYPE TABLE OF stringval,
          lv_arg_fieldname  TYPE stringval,
          lv_cell_fieldname TYPE stringval,
          lt_fieldprop_sel  TYPE ddshfprops,
          lt_fieldprop_lis  TYPE ddshfprops.

    FIELD-SYMBOLS: <ls_fielddescr>    TYPE dfies,
                   <ls_fieldprop_sel> TYPE ddshfprop,
                   <ls_fieldprop_lis> TYPE ddshfprop,
                   <lt_result_itab>   TYPE STANDARD TABLE,
                   <ls_shlp_fields>   TYPE any,
                   <lv_field_token>   TYPE any,
                   <lv_field_input>   TYPE any.

* ---------- Get searchhelp data references -------------------------------------------------------
    DATA: lr_shlp_fields TYPE REF TO data,
          lr_shlp_result TYPE REF TO data.

    me->get_data_ref( EXPORTING iv_shlp_id     = iv_shlp_id
                      IMPORTING er_shlp_fields = lr_shlp_fields
                                er_shlp_result = lr_shlp_result ).

    ASSIGN lr_shlp_fields->* TO <ls_shlp_fields>.
    ASSIGN lr_shlp_result->* TO <lt_result_itab>.

    IF  <lt_result_itab>  IS NOT ASSIGNED OR
        <ls_shlp_fields>  IS NOT ASSIGNED.
      RETURN.
    ENDIF.

* ---------- Get searchhelp description -----------------------------------------------------------
    READ TABLE me->mt_shlp_descr INTO ls_shlp WITH KEY shlpname = iv_shlp_id.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* ---------- Set Selection and List properties ----------------------------------------------------
    lt_fieldprop_sel = ls_shlp-fieldprop.
    lt_fieldprop_lis = ls_shlp-fieldprop.
    DELETE lt_fieldprop_sel WHERE shlpselpos IS INITIAL.
    DELETE lt_fieldprop_lis WHERE shlplispos IS INITIAL.
    SORT lt_fieldprop_sel BY shlpselpos.
    SORT lt_fieldprop_lis BY shlplispos.

* -------------------------------------------------------------------------------------------------
* Searchfield Grid
* -------------------------------------------------------------------------------------------------
    DATA(lr_grid_shlp) = ir_parent->grid( 'L3 M3 S3' )->content( 'layout' ).

* ---------- Create 4 forms (grid columns) --------------------------------------------------------
    DATA(lr_form_shlp_1) = lr_grid_shlp->simple_form( )->content( 'form' ).
    DATA(lr_form_shlp_2) = lr_grid_shlp->simple_form( )->content( 'form' ).
    DATA(lr_form_shlp_3) = lr_grid_shlp->simple_form( )->content( 'form' ).
    DATA(lr_form_shlp_4) = lr_grid_shlp->simple_form( )->content( 'form' ).

    LOOP AT lt_fieldprop_sel ASSIGNING <ls_fieldprop_sel>.
* ---------- Init loop data -----------------------------------------------------------------------
      UNASSIGN: <lv_field_token>, <lv_field_input>, <ls_fielddescr>.

* ---------- Get corresponding field description --------------------------------------------------
      ASSIGN ls_shlp-fielddescr[ fieldname = <ls_fieldprop_sel>-fieldname ] TO <ls_fielddescr>.
      IF <ls_fielddescr> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

* ---------- Get token field reference ------------------------------------------------------------
      ASSIGN COMPONENT <ls_fielddescr>-fieldname OF STRUCTURE <ls_shlp_fields> TO <lv_field_token>.
      IF <lv_field_token> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

* ---------- Get input field reference ------------------------------------------------------------
      ASSIGN COMPONENT me->get_input_fieldname( <ls_fielddescr>-fieldname ) OF STRUCTURE <ls_shlp_fields> TO <lv_field_input>.
      IF <lv_field_input> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

* ---------- Determine grid form number -----------------------------------------------------------
      IF lv_grid_form_no IS INITIAL.
        lv_grid_form_no = 1.
      ELSEIF lv_grid_form_no = 4.
        lv_grid_form_no = 1.
      ELSE.
        lv_grid_form_no = lv_grid_form_no + 1.
      ENDIF.

      CASE lv_grid_form_no.
        WHEN 1.
* ---------- Grid 1--------------------------------------------------------------------------------
* ---------- Set field label ----------------------------------------------------------------------
          lr_form_shlp_1->label( <ls_fielddescr>-scrtext_l ).

* ---------- Set input field ----------------------------------------------------------------------
          lr_form_shlp_1->multi_input(  tokens            = ir_client->_bind( <lv_field_token> )
                                        value             = ir_client->_bind_edit( <lv_field_input> )
                                        showclearicon     = abap_false
                                        tokenupdate       = ir_client->_event( val    = mc_evt_shlp_selopt_token_upd
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) )
                                                                                                 ( `$event.mParameters.type` )
                                                                                                 ( `$event.mParameters.removedTokens[0].mProperties.key` ) ) )

                                        change            = ir_client->_event( val    = mc_evt_shlp_selopt_change
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) ) ) )
                                        valuehelprequest  = ir_client->_event( val    = mc_evt_shlp_selopt_open
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) )
                                                                                                 (  CONV #( iv_shlp_id ) ) ) )
                                        )->item(  key  = `{KEY}`
                                                  text = `{TEXT}`
                                                  )->tokens( )->token(  key      = `{KEY}`
                                                                        text     = `{TEXT}`
                                                                        visible  = `{VISIBLE}`
                                                                        selected = `{SELKZ}`
                                                                        editable = `{EDITABLE}` ).
        WHEN 2.
* ---------- Grid 2--------------------------------------------------------------------------------
* ---------- Set field label ----------------------------------------------------------------------
          lr_form_shlp_2->label( <ls_fielddescr>-scrtext_l ).

* ---------- Set input field ----------------------------------------------------------------------
          lr_form_shlp_2->multi_input(  tokens            = ir_client->_bind( <lv_field_token> )
                                        value             = ir_client->_bind_edit( <lv_field_input> )
                                        showclearicon     = abap_false
                                        tokenupdate       = ir_client->_event( val    = mc_evt_shlp_selopt_token_upd
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) )
                                                                                                 ( `$event.mParameters.type` )
                                                                                                 ( `$event.mParameters.removedTokens[0].mProperties.key` ) ) )

                                        change            = ir_client->_event( val    = mc_evt_shlp_selopt_change
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) ) ) )
                                        valuehelprequest  = ir_client->_event( val    = mc_evt_shlp_selopt_open
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) )
                                                                                                 (  CONV #( iv_shlp_id ) ) ) )
                                        )->item(  key  = `{KEY}`
                                                  text = `{TEXT}`
                                                  )->tokens( )->token(  key      = `{KEY}`
                                                                        text     = `{TEXT}`
                                                                        visible  = `{VISIBLE}`
                                                                        selected = `{SELKZ}`
                                                                        editable = `{EDITABLE}` ).

        WHEN 3.
* ---------- Grid 3--------------------------------------------------------------------------------
* ---------- Set field label ----------------------------------------------------------------------
          lr_form_shlp_3->label( <ls_fielddescr>-scrtext_l ).

* ---------- Set input field ----------------------------------------------------------------------
          lr_form_shlp_3->multi_input(  tokens            = ir_client->_bind( <lv_field_token> )
                                        value             = ir_client->_bind_edit( <lv_field_input> )
                                        showclearicon     = abap_false
                                        tokenupdate       = ir_client->_event( val    = mc_evt_shlp_selopt_token_upd
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) )
                                                                                                 ( `$event.mParameters.type` )
                                                                                                 ( `$event.mParameters.removedTokens[0].mProperties.key` ) ) )

                                        change            = ir_client->_event( val    = mc_evt_shlp_selopt_change
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) ) ) )
                                        valuehelprequest  = ir_client->_event( val    = mc_evt_shlp_selopt_open
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) )
                                                                                                 (  CONV #( iv_shlp_id ) ) ) )
                                        )->item(  key  = `{KEY}`
                                                  text = `{TEXT}`
                                                  )->tokens( )->token(  key      = `{KEY}`
                                                                        text     = `{TEXT}`
                                                                        visible  = `{VISIBLE}`
                                                                        selected = `{SELKZ}`
                                                                        editable = `{EDITABLE}` ).

        WHEN 4.
* ---------- Grid 4--------------------------------------------------------------------------------
* ---------- Set field label ----------------------------------------------------------------------
          lr_form_shlp_4->label( <ls_fielddescr>-scrtext_l ).

* ---------- Set input field ----------------------------------------------------------------------
          lr_form_shlp_4->multi_input(  tokens            = ir_client->_bind( <lv_field_token> )
                                        value             = ir_client->_bind_edit( <lv_field_input> )
                                        showclearicon     = abap_false
                                        tokenupdate       = ir_client->_event( val    = mc_evt_shlp_selopt_token_upd
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) )
                                                                                                 ( `$event.mParameters.type` )
                                                                                                 ( `$event.mParameters.removedTokens[0].mProperties.key` ) ) )

                                        change            = ir_client->_event( val    = mc_evt_shlp_selopt_change
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) ) ) )
                                        valuehelprequest  = ir_client->_event( val    = mc_evt_shlp_selopt_open
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) )
                                                                                                 (  CONV #( iv_shlp_id ) ) ) )
                                        )->item(  key  = `{KEY}`
                                                  text = `{TEXT}`
                                                  )->tokens( )->token(  key      = `{KEY}`
                                                                        text     = `{TEXT}`
                                                                        visible  = `{VISIBLE}`
                                                                        selected = `{SELKZ}`
                                                                        editable = `{EDITABLE}` ).
      ENDCASE.
    ENDLOOP.

* ---------- Create table -------------------------------------------------------------------------
    DATA(lr_table) = ir_parent->table( items = ir_client->_bind( <lt_result_itab> ) ).
* ---------- Create Columns -----------------------------------------------------------------------
    DATA(lr_columns) = lr_table->columns( ).

* ---------- Set column ---------------------------------------------------------------------------
    LOOP AT lt_fieldprop_lis ASSIGNING <ls_fieldprop_lis>.
* ---------- Init loop data -----------------------------------------------------------------------
      UNASSIGN: <ls_fielddescr>.

* ---------- Get corresponding field description --------------------------------------------------
      ASSIGN ls_shlp-fielddescr[ fieldname = <ls_fieldprop_lis>-fieldname ] TO <ls_fielddescr>.
      IF <ls_fielddescr> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      lr_columns->column( )->text( <ls_fielddescr>-scrtext_l ).
    ENDLOOP.

* ---------- Build export parameter list ----------------------------------------------------------
    LOOP AT lt_fieldprop_lis ASSIGNING <ls_fieldprop_lis> WHERE shlpoutput = abap_true.
* ---------- Init loop data -----------------------------------------------------------------------
      CLEAR: lv_arg_fieldname.

* ---------- Build parameter name -----------------------------------------------------------------
      lv_arg_fieldname = `${` && <ls_fieldprop_lis>-fieldname && `}`.

* ---------- Collect output fields ----------------------------------------------------------------
      APPEND lv_arg_fieldname TO lt_arg.
    ENDLOOP.

    DATA(lr_item) = lr_table->items(
        )->column_list_item( type = 'Navigation'  press = ir_client->_event( val    = mc_evt_shlp_close
                                                                             t_arg  = lt_arg ) ).

* ---------- Set cell content ---------------------------------------------------------------------
    LOOP AT lt_fieldprop_lis ASSIGNING <ls_fieldprop_lis>.
* ---------- Init loop data -----------------------------------------------------------------------
      CLEAR: lv_cell_fieldname.

* ---------- Build cell name ----------------------------------------------------------------------
      lv_cell_fieldname = `{` && <ls_fieldprop_lis>-fieldname && `}`.
      lr_item->cells( )->text( lv_cell_fieldname ).
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_ddic_shlp_selopt.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: ls_shlp       TYPE  shlp_descr,
          ls_fielddescr TYPE dfies.

* ---------- Get searchhelp description -----------------------------------------------------------
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = iv_shlp_id
      IMPORTING
        shlp     = ls_shlp.

* ---------- Get field description for given searchhelp field -------------------------------------
*    SELECT SINGLE * FROM @ls_shlp-fielddescr AS fielddescr ##ITAB_KEY_IN_SELECT
*                    WHERE fieldname = @iv_fieldname
*                    INTO @ls_fielddescr.

    READ TABLE ls_shlp-fielddescr INTO ls_fielddescr WITH KEY fieldname = iv_fieldname.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* ---------- Create Popup -------------------------------------------------------------------------
    DATA(lr_popup) = z2ui5_cl_xml_view=>factory_popup( ir_client ).

* ---------- Create Dialog ------------------------------------------------------------------------
    DATA(lr_dialog) = lr_popup->dialog( contentheight = `50%`
                                  contentwidth = `50%`
                                  title = TEXT-t02 && ` ` && ls_fielddescr-scrtext_l ).

* ---------- Create Vbox --------------------------------------------------------------------------
    DATA(lr_vbox) = lr_dialog->content( )->vbox( height = `100%` justifycontent = 'SpaceBetween' ).

* ---------- Create Panel -------------------------------------------------------------------------
    DATA(lr_panel)  = lr_vbox->panel( expandable = abap_false
                                      expanded   = abap_true
                                      headertext = ir_client->_bind_local( ls_fielddescr-scrtext_l ) ).

* ---------- Create List item ---------------------------------------------------------------------
    DATA(lr_item) = lr_panel->list(
              nodata = `no conditions defined`
             items           = ir_client->_bind_edit( me->mt_filter )
                )->custom_list_item( ).

* ---------- Create grid --------------------------------------------------------------------------
    DATA(lr_grid) = lr_item->grid( ).

* ---------- Create Combobox ----------------------------------------------------------------------
    lr_grid->combobox(
                 selectedkey = `{OPTION}`
                 items       = ir_client->_bind_edit( me->mt_mapping )
             )->item( key = '{KEY}'
                      text = '{TEXT}' ).

* ---------- Create input field based on the data type --------------------------------------------
    CASE ls_fielddescr-datatype.
      WHEN 'DATS'.
        lr_grid->date_picker( value  = `{LOW}` ).
        lr_grid->date_picker( value = `{HIGH}`  enabled = `{= ${OPTION} === 'BT' }` ).
      WHEN 'TIMS'.
        lr_grid->time_picker( value  = `{LOW}` ).
        lr_grid->time_picker( value = `{HIGH}`  enabled = `{= ${OPTION} === 'BT' }` ).
      WHEN OTHERS.
        lr_grid->input( value = `{LOW}` ).
        lr_grid->input( value = `{HIGH}`  visible = `{= ${OPTION} === 'BT' }` ).
    ENDCASE.

    lr_grid->hbox( justifycontent = `End`
        )->button( icon = 'sap-icon://decline'
                   type = `Transparent`
                   press = ir_client->_event( val = mc_evt_shlp_selopt_delete t_arg = VALUE #( ( `${KEY}` ) ) ) ).

    lr_panel->hbox( justifycontent = `End`
        )->button( text = TEXT-t03 icon = `sap-icon://add` press = ir_client->_event( val = mc_evt_shlp_selopt_add ) ).

* --------- Create footer buttons -----------------------------------------------------------------
    lr_dialog->buttons(
        )->button( text = TEXT-t04 icon = 'sap-icon://delete' type = `Transparent` press = ir_client->_event( val = mc_evt_shlp_selopt_delete_all )
        )->button( text = TEXT-t05 press = ir_client->_event( mc_evt_shlp_selopt_ok ) type  = 'Emphasized'
        )->button( text = TEXT-t00 press = ir_client->_event( mc_evt_shlp_selopt_cancel ) ).

* ---------- Display popup window -----------------------------------------------------------------
    ir_client->popup_display( lr_popup->stringify( ) ).
  ENDMETHOD.


  METHOD get_data_ref.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lv_index TYPE i.

* ---------- Check import parameter ---------------------------------------------------------------
    IF iv_index IS NOT INITIAL.
* ---------- Set index to local variable ----------------------------------------------------------
      lv_index = iv_index.
    ELSEIF iv_shlp_id IS NOT INITIAL.
* ---------- Get searchhelp index from shlp id ----------------------------------------------------
      lv_index = line_index( me->mt_shlp_descr[ shlpname = iv_shlp_id ] ).
    ELSE.
      RETURN.
    ENDIF.

* -------------------------------------------------------------------------------------------------
* Heap References and Stack References issue!
* Internal tables are dynamic data objects and have a special role because they have their own
* memory management. They allocate and release memory regardless of the statement CREATE and
* Garbage Collector. This means that heap references to rows in internal tables can become invalid.
* Therfore I had to create fix amount of reference variables (placeholders),
* instead of ITAB with references.
* -------------------------------------------------------------------------------------------------
    CASE lv_index.
      WHEN 1.
        er_shlp_fields = me->mr_shlp_fields_1 .
        er_shlp_result = me->mr_shlp_result_1 .

      WHEN 2.
        er_shlp_fields = me->mr_shlp_fields_2 .
        er_shlp_result = me->mr_shlp_result_2 .

      WHEN 3.
        er_shlp_fields = me->mr_shlp_fields_3 .
        er_shlp_result = me->mr_shlp_result_3 .

      WHEN 4.
        er_shlp_fields = me->mr_shlp_fields_4 .
        er_shlp_result = me->mr_shlp_result_4 .

      WHEN 5.
        er_shlp_fields = me->mr_shlp_fields_5 .
        er_shlp_result = me->mr_shlp_result_5 .

      WHEN 6.
        er_shlp_fields = me->mr_shlp_fields_6 .
        er_shlp_result = me->mr_shlp_result_6 .

      WHEN 7.
        er_shlp_fields = me->mr_shlp_fields_7 .
        er_shlp_result = me->mr_shlp_result_7 .

      WHEN 8.
        er_shlp_fields = me->mr_shlp_fields_8 .
        er_shlp_result = me->mr_shlp_result_8 .

      WHEN 9.
        er_shlp_fields = me->mr_shlp_fields_9 .
        er_shlp_result = me->mr_shlp_result_9 .

      WHEN 10.
        er_shlp_fields = me->mr_shlp_fields_10 .
        er_shlp_result = me->mr_shlp_result_10 .

    ENDCASE.
  ENDMETHOD.


  METHOD get_input_fieldname.
    rv_input_fieldname = |{ iv_fieldname }_INPUT|.
  ENDMETHOD.


  METHOD get_selopt_mapping.
    rt_mapping = VALUE #(
  (   key = 'EQ'
      text = TEXT-l01
      value = `={LOW}`    )

  (   key = 'LT'
      text = TEXT-l02
      value = `<{LOW}`   )

  (   key = 'LE'
      text = TEXT-l03
      value = `<={LOW}`  )

  (   key = 'GT'
      text = TEXT-l04
      value = `>{LOW}`   )

  (   key = 'GE'
      text = TEXT-l05
      value = `>={LOW}`  )

  (   key = 'CP'
      text = TEXT-l06
      value = `{LOW}`  )

  (   key = 'BT'
      text = TEXT-l07
      value = `{LOW}...{HIGH}` )

  (   key = 'NE'
      text = TEXT-l08
      value = `!(={LOW})`    )
  ).
  ENDMETHOD.


  METHOD get_shlp_range_by_value.
    DATA(lv_length) = strlen( iv_value ) - 1.
    CASE iv_value(1).
      WHEN `=`.
        rs_result = VALUE #( sign = `I` option = `EQ` low = iv_value+1 ).
      WHEN `<`.
        IF iv_value+1(1) = `=`.
          rs_result = VALUE #( sign = `I` option = `LE` low = iv_value+2 ).
        ELSE.
          rs_result = VALUE #( sign = `I` option = `LT` low = iv_value+1 ).
        ENDIF.
      WHEN `>`.
        IF iv_value+1(1) = `=`.
          rs_result = VALUE #( sign = `I` option = `GE` low = iv_value+2 ).
        ELSE.
          rs_result = VALUE #( sign = `I` option = `GT` low = iv_value+1 ).
        ENDIF.

      WHEN OTHERS.
        IF iv_value CS '...'.
          SPLIT iv_value AT '...' INTO rs_result-low rs_result-high.
          rs_result-option = `BT`.
        ELSEIF iv_value CS `*`.
          rs_result = VALUE #( sign = `I` option = `CP` low = iv_value ).
        ELSEIF iv_value CS `+`.
          rs_result = VALUE #( sign = `I` option = `CP` low = iv_value ).

        ELSE.
          rs_result = VALUE #( sign = `I` option = `EQ` low = iv_value  ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.


  METHOD get_shlp_uuid.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA uuid TYPE sysuuid_c32.

    TRY.
        CALL METHOD ('CL_SYSTEM_UUID')=>create_uuid_c32_static
          RECEIVING
            uuid = uuid.
      CATCH cx_sy_dyn_call_illegal_class.

        DATA(lv_fm) = 'GUID_CREATE'.
        CALL FUNCTION lv_fm
          IMPORTING
            ev_guid_32 = uuid.
    ENDTRY.

    rv_result = uuid.
  ENDMETHOD.


  METHOD init_data_ref.
* -------------------------------------------------------------------------------------------------
* Heap References and Stack References issue!
* Internal tables are dynamic data objects and have a special role because they have their own
* memory management. They allocate and release memory regardless of the statement CREATE and
* Garbage Collector. This means that heap references to rows in internal tables can become invalid.
* Therfore I had to create fix amount of reference variables (placeholders),
* instead of ITAB with references.
* -------------------------------------------------------------------------------------------------

* ---------- Init searchhelp field references -----------------------------------------------------
    CLEAR:
    mr_shlp_fields_1,
    mr_shlp_fields_2,
    mr_shlp_fields_3,
    mr_shlp_fields_4,
    mr_shlp_fields_5,
    mr_shlp_fields_6,
    mr_shlp_fields_7,
    mr_shlp_fields_8,
    mr_shlp_fields_9,
    mr_shlp_fields_10.

* ---------- Init searchhelp result references ----------------------------------------------------
    CLEAR:
    mr_shlp_result_1,
    mr_shlp_result_2,
    mr_shlp_result_3,
    mr_shlp_result_4,
    mr_shlp_result_5,
    mr_shlp_result_6,
    mr_shlp_result_7,
    mr_shlp_result_8,
    mr_shlp_result_9,
    mr_shlp_result_10.
  ENDMETHOD.


  METHOD on_event.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lt_event_arg TYPE string_table,
          ls_range     TYPE ts_range,
          lv_shlp_id   TYPE char30,
          ls_shlp_type TYPE ts_shlp_type.

    FIELD-SYMBOLS: <lt_field_token> TYPE STANDARD TABLE,
                   <lv_input_field> TYPE any,
                   <ls_field_token> TYPE any,
                   <lv_field>       TYPE any,
                   <ls_filter>      TYPE ts_filter_pop,
                   <ls_shlp_fields> TYPE any,
                   <ls_shlp_result> TYPE any.

* ---------- Get event parameters -----------------------------------------------------------------
    lt_event_arg = ir_client->get( )-t_event_arg.

* ----------- Get searchhelp record for requested searchhelp id -----------------------------------
    IF me->ms_screen-shlp_selkey IS NOT INITIAL.
* ---------- Get searchhelp data references -------------------------------------------------------
      DATA: lr_shlp_fields TYPE REF TO data,
            lr_shlp_result TYPE REF TO data.

      me->get_data_ref( EXPORTING iv_shlp_id     = me->ms_screen-shlp_selkey
                        IMPORTING er_shlp_fields = lr_shlp_fields
                                  er_shlp_result = lr_shlp_result ).

      ASSIGN lr_shlp_fields->* TO <ls_shlp_fields>.
      ASSIGN lr_shlp_result->* TO <ls_shlp_result>.
      IF <ls_shlp_fields> IS NOT ASSIGNED OR
         <ls_shlp_result> IS NOT ASSIGNED.
        RETURN.
      ENDIF.
    ENDIF.

* ---------- Handle popup returning paramter ------------------------------------------------------
    me->handle_popup_return( EXPORTING ir_client        =  ir_client
                             CHANGING  cs_shlp_fields   = <ls_shlp_fields>
                                       cs_shlp_result   = <ls_shlp_result> ).

    CASE ir_client->get( )-event.
      WHEN mc_evt_shlp_close.
* ---------- Set search field value ---------------------------------------------------------------
        IF line_exists( lt_event_arg[ 1 ] ).
          me->mv_shlp_result = lt_event_arg[ 1 ].
        ENDIF.
        IF line_exists( lt_event_arg[ 2 ] ).
          me->mv_shlp_result2 = lt_event_arg[ 2 ].
        ENDIF.
        IF line_exists( lt_event_arg[ 3 ] ).
          me->mv_shlp_result3 = lt_event_arg[ 3 ].
        ENDIF.

        ir_client->popup_destroy( ).
        ir_client->nav_app_leave( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app_stack ) ).
        RETURN.
      WHEN mc_evt_shlp_go.
* ---------- Init search result first -------------------------------------------------------------
        CLEAR: <ls_shlp_result>.
* ---------- Update popup model binding -----------------------------------------------------------
        ir_client->popup_model_update( ).

* ---------- Fetch searchhelp result --------------------------------------------------------------
        me->select_ddic_shlp( ir_controller  = me
                              iv_shlp_id     = me->ms_screen-shlp_selkey ).

* ---------- Update popup model binding -----------------------------------------------------------
        ir_client->popup_model_update( ).

      WHEN mc_evt_shlp_select.
* ---------- Init search result first -------------------------------------------------------------
        CLEAR: <ls_shlp_result>.
* ---------- Update popup model binding -----------------------------------------------------------
        ir_client->popup_model_update( ).

      WHEN mc_evt_shlp_selopt_open.
* ---------- Check for fieldname ------------------------------------------------------------------
        IF NOT line_exists( lt_event_arg[ 1 ] ).
          RETURN.
        ENDIF.

* ---------- Check for searchhelp id --------------------------------------------------------------
        IF NOT line_exists( lt_event_arg[ 1 ] ).
          RETURN.
        ENDIF.

* ---------- Set select-option field name ---------------------------------------------------------
        CLEAR: me->mv_selopt_fieldname.
        me->mv_selopt_fieldname = lt_event_arg[ 1 ].

* ---------- Set searchhelp ID  -------------------------------------------------------------------
        lv_shlp_id = lt_event_arg[ 2 ].

* ---------- Get Searchhelp type ------------------------------------------------------------------
        ls_shlp_type = me->get_shlp_type( iv_shlp_id    = lv_shlp_id
                                          iv_fieldname  = me->mv_selopt_fieldname ).

* ---------- Assign current token field -----------------------------------------------------------
        ASSIGN COMPONENT me->mv_selopt_fieldname OF STRUCTURE <ls_shlp_fields> TO <lt_field_token>.
        IF <lt_field_token> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

* ---------- Close searchhelp Popup window --------------------------------------------------------
        ir_client->popup_destroy( ).

* ---------- Open different searchhelp based on the SHLP type -------------------------------------
        CASE ls_shlp_type-type.
          WHEN abap_false. "No shlp -> open generic one
* ---------- Fill select-option filter ------------------------------------------------------------
            me->fill_filter( EXPORTING it_token  = <lt_field_token>
                             CHANGING  ct_filter = me->mt_filter ).

* ---------- Handle select-option popup opening ---------------------------------------------------
            me->generate_ddic_shlp_selopt( ir_client    = ir_client
                                           iv_fieldname = me->mv_selopt_fieldname
                                           iv_shlp_id   = me->ms_screen-shlp_selkey ).

          WHEN mc_shlp_type_ddic_shlp.
* ---------- Open searchhelp popup window ---------------------------------------------------------
            me->mv_check_popup_shlp = abap_true.
            ir_client->nav_app_call( z2ui5_cl_tool_app_shlp_gen=>factory( iv_popup_title          = ls_shlp_type-label
                                                                          iv_shlp_id              = ls_shlp_type-id ) ).
          WHEN mc_shlp_type_fix_domval.
* ---------- Open value help popup window for domain fix values -----------------------------------
            me->generate_vh_domain_fix_val( ir_client    = ir_client
                                            is_shlp_type = ls_shlp_type ).
          WHEN mc_shlp_type_shlp_exit.
* ---------- Open value help popup window for user-exit -------------------------------------------
            me->generate_vh_user_exit( ir_client    = ir_client
                                       is_shlp_type = ls_shlp_type ).
        ENDCASE.

      WHEN mc_evt_shlp_selopt_add.
        INSERT VALUE #( key = me->get_shlp_uuid( ) ) INTO TABLE me->mt_filter.

        ir_client->popup_model_update( ).

      WHEN mc_evt_shlp_selopt_cancel.
* ---------- Close select-option Popup window -----------------------------------------------------
        ir_client->popup_destroy( ).
* ---------- Handle searchhelp popup opening ------------------------------------------------------
        me->on_rendering( ir_client = ir_client ).

      WHEN mc_evt_shlp_selopt_ok.
* ---------- Assign current token field -----------------------------------------------------------
        ASSIGN COMPONENT me->mv_selopt_fieldname OF STRUCTURE <ls_shlp_fields> TO <lt_field_token>.
        IF <lt_field_token> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

* ---------- Init token first ---------------------------------------------------------------------
        CLEAR: <lt_field_token>.

* ---------- Fill token ---------------------------------------------------------------------------
        me->fill_token( EXPORTING it_filter = me->mt_filter
                        CHANGING ct_token  = <lt_field_token> ).

* ---------- Close select-option Popup window -----------------------------------------------------
        ir_client->popup_destroy( ).
* ---------- Handle searchhelp popup opening ------------------------------------------------------
        me->on_rendering( ir_client = ir_client ).

      WHEN mc_evt_shlp_selopt_delete.
        IF NOT line_exists( lt_event_arg[ 1 ] ).
          RETURN.
        ENDIF.

        DELETE me->mt_filter WHERE key = lt_event_arg[ 1 ].
        ir_client->popup_model_update( ).

      WHEN mc_evt_shlp_selopt_delete_all.
        me->mt_filter = VALUE #( ).
        ir_client->popup_model_update( ).

      WHEN mc_evt_shlp_selopt_change.
        IF NOT line_exists( lt_event_arg[ 1 ] ).
          RETURN.
        ENDIF.

* ---------- Set select-option field name ---------------------------------------------------------
        CLEAR: me->mv_selopt_fieldname.
        me->mv_selopt_fieldname = lt_event_arg[ 1 ].

* ---------- Assign current input field -----------------------------------------------------------
        ASSIGN COMPONENT me->get_input_fieldname( CONV #( me->mv_selopt_fieldname ) ) OF STRUCTURE <ls_shlp_fields> TO <lv_input_field>.
        IF <lv_input_field> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

* ---------- Set filter based on the input field value --------------------------------------------
        ls_range = me->get_shlp_range_by_value( iv_value = <lv_input_field> ).
        APPEND INITIAL LINE TO me->mt_filter ASSIGNING <ls_filter>.
        <ls_filter>-key     = me->get_shlp_uuid( ).
        <ls_filter>-option  = ls_range-option.
        <ls_filter>-low     = ls_range-low.
        <ls_filter>-high    = ls_range-high.
        CLEAR: <lv_input_field>.

* ---------- Assign current token field -----------------------------------------------------------
        ASSIGN COMPONENT me->mv_selopt_fieldname OF STRUCTURE <ls_shlp_fields> TO <lt_field_token>.
        IF <lt_field_token> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

* ---------- Fill token ---------------------------------------------------------------------------
        me->fill_token( EXPORTING it_filter = me->mt_filter
                        CHANGING ct_token  = <lt_field_token> ).

* ---------- Init filter again --------------------------------------------------------------------
        CLEAR: me->mt_filter.

* ---------- Update popup data model --------------------------------------------------------------
        ir_client->popup_model_update( ).

      WHEN mc_evt_shlp_selopt_token_upd.
        IF  NOT line_exists( lt_event_arg[ 1 ] ) OR
            NOT line_exists( lt_event_arg[ 2 ] ) OR
            NOT line_exists( lt_event_arg[ 3 ] ).
          RETURN.
        ENDIF.

* ---------- Retieve event parameters -------------------------------------------------------------
        CLEAR: me->mv_selopt_fieldname.
        me->mv_selopt_fieldname = lt_event_arg[ 1 ].
        DATA(lv_token_upd_type) = lt_event_arg[ 2 ].
        DATA(lv_token_key) = lt_event_arg[ 3 ].

* ---------- Assign current token field -----------------------------------------------------------
        ASSIGN COMPONENT me->mv_selopt_fieldname OF STRUCTURE <ls_shlp_fields> TO <lt_field_token>.
        IF <lt_field_token> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

        CASE lv_token_upd_type.
          WHEN mc_token_upd_type_remove.
            me->delete_token( EXPORTING iv_token_key  = lv_token_key
                              CHANGING ct_token       = <lt_field_token>  ).

          WHEN OTHERS.

        ENDCASE.

* ---------- Update popup data model --------------------------------------------------------------
        ir_client->popup_model_update( ).
    ENDCASE.
  ENDMETHOD.


  METHOD on_init.
    IF mv_check_initialized = abap_false.
      mv_check_initialized = abap_true.
* ---------- Get searchhelp description -----------------------------------------------------------
      me->expand_searchhelp( EXPORTING iv_shlp_id         = me->mv_shlp_id
                                       it_shlp_blacklist  = me->mt_shlp_blacklist
                             IMPORTING et_shlp_descr      = me->mt_shlp_descr
                                       ev_shlp_selkey     = me->ms_screen-shlp_selkey ).
      IF me->mt_shlp_descr IS INITIAL.
        RETURN.
      ENDIF.

* ---------- Prefill select-option mapping table --------------------------------------------------
      me->mt_mapping = me->get_selopt_mapping( ).

* ---------- Build searchhelp data references -----------------------------------------------------
      me->build_data_ref( ir_client = ir_client
                          it_shlp_descr = me->mt_shlp_descr ).

* ---------- Perform searchhelp popup rendering ---------------------------------------------------
      me->on_rendering( ir_client = ir_client ).
    ENDIF.


  ENDMETHOD.


  METHOD on_rendering.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    FIELD-SYMBOLS: <ls_shlp_descr> TYPE ts_shlp_descr.

* ---------- Create Popup -------------------------------------------------------------------------
    DATA(lr_popup) = z2ui5_cl_xml_view=>factory_popup( ).

* ---------- Create Dialog ------------------------------------------------------------------------
    DATA(lr_dialog) = lr_popup->dialog( title     = me->mv_popup_title
                                        resizable = abap_true ).

* ---------- Create Popup content -----------------------------------------------------------------
    DATA(lr_dialog_content) = lr_dialog->content( ).

* ---------- Create "Go" button -------------------------------------------------------------------
    DATA(lr_toolbar) = lr_dialog_content->toolbar( ).
    lr_toolbar->toolbar_spacer( ).
    lr_toolbar->button(
                  text    = TEXT-t01
                  type    = 'Emphasized'
                  press   = ir_client->_event( mc_evt_shlp_go ) ).

* ---------- Create icon filter tab bar -----------------------------------------------------------
    DATA(lr_icon_tab_bar) = lr_dialog_content->icon_tab_bar( selectedkey = ir_client->_bind_edit( me->ms_screen-shlp_selkey )
                                                            select = ir_client->_event( mc_evt_shlp_select ) )->items( ).

* ---------- Build for each searchhelp an own icon tab --------------------------------------------
    LOOP AT me->mt_shlp_descr ASSIGNING <ls_shlp_descr>.

* ---------- Create searchhelp icon tab -----------------------------------------------------------
      DATA(lr_icon_tab) = lr_icon_tab_bar->icon_tab_filter(  key   = <ls_shlp_descr>-shlpname
                                                             text  = <ls_shlp_descr>-intdescr-ddtext ).

* ---------- Generate searchfields ----------------------------------------------------------------
      me->generate_ddic_shlp( ir_parent                 = lr_icon_tab
                              ir_client                 = ir_client
                              iv_shlp_id                = <ls_shlp_descr>-shlpname ).

    ENDLOOP.

* ---------- Create Button ------------------------------------------------------------------------
    lr_dialog->buttons( )->button(
                  text    = TEXT-t00
                  press   = ir_client->_event( mc_evt_shlp_close ) ).

* ---------- Display popup window -----------------------------------------------------------------
    ir_client->popup_display( lr_popup->stringify( ) ).
  ENDMETHOD.


  METHOD select_ddic_shlp.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: ls_shlp          TYPE shlp_descr,
          lt_return_values TYPE TABLE OF ddshretval,
          lt_record_tab    TYPE TABLE OF seahlpres,
          lv_convexit_name TYPE rs38l_fnam,
          lv_offset        TYPE i,
          lv_length        TYPE i,
          lt_fieldprop_sel TYPE ddshfprops,
          lt_fieldprop_lis TYPE ddshfprops,
          ls_range         TYPE ts_range,
          lv_date_out      TYPE sy-datum,
          lv_time_out      TYPE sy-uzeit,
          lt_param         TYPE abap_parmbind_tab,
          lv_class_name    TYPE string,
          lv_meth_name     TYPE string,
          lr_conv_out      TYPE REF TO data.

    FIELD-SYMBOLS: <ls_fielddescr>    TYPE dfies,
                   <ls_record_tab>    TYPE seahlpres,
                   <ls_result>        TYPE any,
                   <ls_shlp_fields>   TYPE any,
                   <lv_field>         TYPE any,
                   <lt_token>         TYPE tt_token,
                   <ls_token>         TYPE ts_token,
                   <ls_fieldprop_sel> TYPE ddshfprop,
                   <ls_fieldprop_lis> TYPE ddshfprop,
                   <lt_result_itab>   TYPE STANDARD TABLE,
                   <lv_conv_out>      TYPE any.

* ---------- Get searchhelp data references -------------------------------------------------------
    DATA: lr_shlp_fields TYPE REF TO data,
          lr_shlp_result TYPE REF TO data.

    me->get_data_ref( EXPORTING iv_shlp_id     = iv_shlp_id
                      IMPORTING er_shlp_fields = lr_shlp_fields
                                er_shlp_result = lr_shlp_result ).

    ASSIGN lr_shlp_fields->* TO <ls_shlp_fields>.
    ASSIGN lr_shlp_result->* TO <lt_result_itab>.

    IF <lt_result_itab> IS NOT ASSIGNED OR
      <ls_shlp_fields> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

* ---------- Get searchhelp description -----------------------------------------------------------
    READ TABLE me->mt_shlp_descr INTO ls_shlp WITH KEY shlpname = iv_shlp_id.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* ---------- Set Selection and List properties ----------------------------------------------------
    lt_fieldprop_sel = ls_shlp-fieldprop.
    lt_fieldprop_lis = ls_shlp-fieldprop.
    DELETE lt_fieldprop_sel WHERE shlpselpos IS INITIAL.
    DELETE lt_fieldprop_lis WHERE shlplispos IS INITIAL.
    SORT lt_fieldprop_sel BY shlpselpos.
    SORT lt_fieldprop_lis BY shlplispos.

* ---------- Set filter criteria ------------------------------------------------------------------
    LOOP AT lt_fieldprop_sel ASSIGNING <ls_fieldprop_sel>.
* ---------- Init loop data -----------------------------------------------------------------------
      UNASSIGN: <lt_token>, <ls_token>, <ls_fielddescr>.

* ---------- Get corresponding field description --------------------------------------------------
      ASSIGN ls_shlp-fielddescr[ fieldname = <ls_fieldprop_sel>-fieldname ] TO <ls_fielddescr>.
      IF <ls_fielddescr> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

* ---------- Get reference of given fieldname -----------------------------------------------------
      ASSIGN COMPONENT <ls_fielddescr>-fieldname OF STRUCTURE <ls_shlp_fields> TO <lt_token>.

* ---------- In case no field found or the field is initial -> leave ------------------------------
      IF <lt_token> IS NOT ASSIGNED OR
         <lt_token> IS INITIAL.
        CONTINUE.
      ENDIF.

* ---------- Set filter criteria for given fieldname ----------------------------------------------
      LOOP AT <lt_token> ASSIGNING <ls_token>.
* ---------- Init loop data -----------------------------------------------------------------------
        CLEAR: ls_range, lv_date_out, lv_time_out, lr_conv_out.
        UNASSIGN: <lv_conv_out>.

* ---------- Convert token into range format ------------------------------------------------------
        ls_range = me->get_shlp_range_by_value( iv_value = <ls_token>-key ).
        IF ls_range IS INITIAL.
          CONTINUE.
        ENDIF.

* ---------- Convert date and time into internal format -------------------------------------------
        CASE <ls_fielddescr>-datatype.
          WHEN 'DATS'.
            CLEAR: lv_date_out.
            CALL FUNCTION 'CONVERSION_EXIT_DATLO_INPUT'
              EXPORTING
                input        = ls_range-low
              IMPORTING
                output       = lv_date_out
              EXCEPTIONS
                unknown_code = 1
                OTHERS       = 2.

            IF sy-subrc = 0.
              ls_range-low = lv_date_out.
            ELSE.
* ---------- Keep format --------------------------------------------------------------------------
            ENDIF.

            CLEAR: lv_date_out.
            CALL FUNCTION 'CONVERSION_EXIT_DATLO_INPUT'
              EXPORTING
                input        = ls_range-high
              IMPORTING
                output       = lv_date_out
              EXCEPTIONS
                unknown_code = 1
                OTHERS       = 2.

            IF sy-subrc = 0.
              ls_range-high = lv_date_out.
            ELSE.
* ---------- Keep format --------------------------------------------------------------------------
            ENDIF.

          WHEN 'TIMS'.
            CLEAR: lv_time_out.
            CALL FUNCTION 'CONVERSION_EXIT_TIME_INPUT'
              EXPORTING
                input                 = ls_range-low
              IMPORTING
                output                = lv_time_out
              EXCEPTIONS
                wrong_format_in_input = 1
                OTHERS                = 2.

            IF sy-subrc = 0.
              ls_range-low = lv_time_out.
            ELSE.
* ---------- Keep format --------------------------------------------------------------------------
            ENDIF.

            CLEAR: lv_time_out.
            CALL FUNCTION 'CONVERSION_EXIT_TIME_INPUT'
              EXPORTING
                input                 = ls_range-high
              IMPORTING
                output                = lv_time_out
              EXCEPTIONS
                wrong_format_in_input = 1
                OTHERS                = 2.

            IF sy-subrc = 0.
              ls_range-high = lv_time_out.
            ELSE.
* ---------- Keep format --------------------------------------------------------------------------
            ENDIF.

          WHEN 'CHAR'.
* ---------- Check for conversion exits -----------------------------------------------------------
            IF <ls_fielddescr>-convexit IS NOT INITIAL.
* ---------- Build conversion exit name ------------------------------------------------------------
              lv_convexit_name = 'CONVERSION_EXIT_' && <ls_fielddescr>-convexit && '_INPUT'.

* ---------- Check if conversion exit function module exists ---------------------------------------
              CALL FUNCTION 'FUNCTION_EXISTS'
                EXPORTING
                  funcname           = lv_convexit_name
                EXCEPTIONS
                  function_not_exist = 1
                  OTHERS             = 2.

              IF sy-subrc = 0.
* ---------- Create data refernce -----------------------------------------------------------------
                CREATE DATA lr_conv_out TYPE (<ls_fielddescr>-domname).
                IF lr_conv_out IS BOUND.

* ---------- Assign pointer -----------------------------------------------------------------------
                  ASSIGN lr_conv_out->* TO <lv_conv_out>.

* ---------- Execute conversion exit for low value ------------------------------------------------
                  CALL FUNCTION lv_convexit_name
                    EXPORTING
                      input  = ls_range-low
                    IMPORTING
                      output = <lv_conv_out>.

                  IF sy-subrc = 0.
                    ls_range-low = <lv_conv_out>.
                  ELSE.
* ---------- Keep format --------------------------------------------------------------------------
                  ENDIF.

* ---------- Execute conversion exit for high value -----------------------------------------------
                  CLEAR: <lv_conv_out>.
                  CALL FUNCTION lv_convexit_name
                    EXPORTING
                      input  = ls_range-high
                    IMPORTING
                      output = <lv_conv_out>.
                  IF sy-subrc = 0.
                    ls_range-high = <lv_conv_out>.
                  ELSE.
* ---------- Keep format --------------------------------------------------------------------------
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
        ENDCASE.

* ---------- Store search criteria to select option range table -----------------------------------
        APPEND VALUE #( shlpname  = ls_shlp-shlpname
                        shlpfield = <ls_fielddescr>-fieldname
                        sign      = 'I'
                        option    = ls_range-option
                        low       = ls_range-low
                        high      = ls_range-high ) TO ls_shlp-selopt.
      ENDLOOP.
    ENDLOOP.

* ---------- Call select-option prefill exit ------------------------------------------------------
    IF me->mv_selopt_prefill_exit IS NOT INITIAL.
* ---------- Split exit name into class and method ------------------------------------------------
      SPLIT me->mv_selopt_prefill_exit AT '=>' INTO lv_class_name lv_meth_name.

* ---------- Push result list table to the parameter list -----------------------------------------
      lt_param = VALUE #( ( name = 'CT_SELOPT'
                            kind = cl_abap_objectdescr=>changing
                            value = REF #( ls_shlp-selopt ) )
                          ( name = 'IV_SHLP_ID'
                            kind = cl_abap_objectdescr=>exporting
                            value = REF #( iv_shlp_id ) ) ).

* ---------- Call exit ----------------------------------------------------------------------------
      CALL METHOD (lv_class_name)=>(lv_meth_name)
        PARAMETER-TABLE lt_param.
    ENDIF.

* ---------- Fetch data from searchhelp -----------------------------------------------------------
    CALL FUNCTION 'F4IF_SELECT_VALUES'
      EXPORTING
        shlp           = ls_shlp
        maxrows        = iv_maxrows
        call_shlp_exit = abap_true
      TABLES
        record_tab     = lt_record_tab
        return_tab     = lt_return_values.

* ---------- Map string into structure ------------------------------------------------------------
    LOOP AT lt_record_tab ASSIGNING <ls_record_tab>.
* ---------- Create initial result record ---------------------------------------------------------
      APPEND INITIAL LINE TO <lt_result_itab> ASSIGNING <ls_result>.

* ---------- Perform data mapping -----------------------------------------------------------------
      LOOP AT lt_fieldprop_lis ASSIGNING <ls_fieldprop_lis>.
* ---------- Init loop data -----------------------------------------------------------------------
        CLEAR: lv_convexit_name, lv_offset, lv_length.
        UNASSIGN: <lv_field>, <ls_fielddescr>.

* ---------- Get corresponding field description --------------------------------------------------
        ASSIGN ls_shlp-fielddescr[ fieldname = <ls_fieldprop_lis>-fieldname ] TO <ls_fielddescr>.
        IF <ls_fielddescr> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

* ---------- Assign target field ------------------------------------------------------------------
        ASSIGN COMPONENT <ls_fielddescr>-fieldname OF STRUCTURE <ls_result> TO <lv_field>.
        IF <lv_field> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

* ---------- Set offset and length ---------------------------------------------------------------
        lv_offset = <ls_fielddescr>-offset / 2. "For any reason offset shows always double values ;-)
        lv_length = <ls_fielddescr>-leng.

* ---------- Map data via offset -----------------------------------------------------------------
        <lv_field> = <ls_record_tab>-string+lv_offset(lv_length).

* ---------- Set intial values for date and time (if needed) ---------------------------------------
        CASE <ls_fielddescr>-datatype.
          WHEN 'DATS'.
            IF <lv_field> = space.
              <lv_field> = '00000000'.
            ENDIF.
          WHEN 'TIMS'.
            IF <lv_field> = space.
              <lv_field> = '000000'.
            ENDIF.
        ENDCASE.

        IF <lv_field> IS INITIAL.
          CONTINUE.
        ENDIF.

* ---------- Perform conversion exit ---------------------------------------------------------------
        IF <ls_fielddescr>-convexit IS NOT INITIAL.
* ---------- Build conversion exit name ------------------------------------------------------------
          lv_convexit_name = 'CONVERSION_EXIT_' && <ls_fielddescr>-convexit && '_OUTPUT'.

* ---------- Check if conversion exit function module exists ---------------------------------------
          CALL FUNCTION 'FUNCTION_EXISTS'
            EXPORTING
              funcname           = lv_convexit_name
            EXCEPTIONS
              function_not_exist = 1
              OTHERS             = 2.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

* ---------- Execute conversion exit ---------------------------------------------------------------
          CALL FUNCTION lv_convexit_name
            EXPORTING
              input  = <lv_field>
            IMPORTING
              output = <lv_field>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

* ---------- Call result filter exit --------------------------------------------------------------
    IF me->mv_result_filter_exit IS NOT INITIAL.
* ---------- Split exit name into class and method ------------------------------------------------
      SPLIT me->mv_result_filter_exit AT '=>' INTO lv_class_name lv_meth_name.

* ---------- Push result list table to the parameter list -----------------------------------------
      lt_param = VALUE #( ( name = 'CT_RESULT'
                            kind = cl_abap_objectdescr=>changing
                            value = REF #( <lt_result_itab> ) )
                           ( name = 'IV_SHLP_ID'
                            kind = cl_abap_objectdescr=>exporting
                            value = REF #( iv_shlp_id ) ) ).

* ---------- Call exit ----------------------------------------------------------------------------
      CALL METHOD (lv_class_name)=>(lv_meth_name)
        PARAMETER-TABLE lt_param.
    ENDIF.
  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->on_init( ir_client = client ).

    me->on_event( ir_client = client ).

  ENDMETHOD.


  METHOD convert_result_to_token.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lt_filter TYPE tt_filter_prop.

    FIELD-SYMBOLS: <ls_filter>      TYPE ts_filter_pop,
                   <lt_field_token> TYPE STANDARD TABLE,
                   <ls_shlp_fields> TYPE any.

* ---------- Build new filter record --------------------------------------------------------------
    APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
    <ls_filter>-key     = me->get_shlp_uuid( ).
    <ls_filter>-option  = 'EQ'.
    <ls_filter>-low     = iv_result.

* ---------- Assign current token field -----------------------------------------------------------
    ASSIGN COMPONENT iv_shlp_fieldname OF STRUCTURE cs_shlp_fields TO <lt_field_token>.
    IF <lt_field_token> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

* ---------- Init token first ---------------------------------------------------------------------
    CLEAR: <lt_field_token>.

* ---------- Fill token ---------------------------------------------------------------------------
    me->fill_token( EXPORTING it_filter = lt_filter
                    CHANGING ct_token  = <lt_field_token> ).
  ENDMETHOD.


  METHOD get_shlp_type.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: ls_shlp        TYPE ts_shlp_descr,
          ls_field_descr TYPE dfies,
          lv_shlpname    TYPE shlpname,
          lt_dd07v       TYPE TABLE OF dd07v,
          lv_entitytab   TYPE entitytab.

* ---------- Get searchhelp description -----------------------------------------------------------
    READ TABLE me->mt_shlp_descr INTO ls_shlp WITH KEY shlpname = iv_shlp_id.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* ---------- Get fieldname description ------------------------------------------------------------
    READ TABLE ls_shlp-fielddescr INTO ls_field_descr WITH KEY fieldname = iv_fieldname.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* -------------------------------------------------------------------------------------------------
* 1. Check for SHLP user-exit
* -------------------------------------------------------------------------------------------------
    IF line_exists( me->mt_shlp_exit[ rollname =  ls_field_descr-rollname ] ).
* ---------- SHLP exit exists for given data element -----------------------------------------------
      rs_shlp_type-type   = mc_shlp_type_shlp_exit.
      rs_shlp_type-id     = ls_field_descr-rollname.
      rs_shlp_type-label  = ls_field_descr-scrtext_l.

* ---------- We have all needed info -> leave -----------------------------------------------------
      RETURN.
    ENDIF.

* ---------- Check if standard searchhelp (on Data element or domain) exists ----------------------
    IF ls_field_descr-inttype <> 'C' OR
       ls_field_descr-f4availabl = abap_false.
      RETURN.
    ENDIF.

* ---------- Check if deep searchelp has been requested -------------------------------------------
    IF me->mv_use_deep_shlp = abap_false.
      RETURN.
    ENDIF.

* -------------------------------------------------------------------------------------------------
* 2. Check for DDIC searchelp
* -------------------------------------------------------------------------------------------------
* ---------- Get data element details -------------------------------------------------------------
    SELECT SINGLE shlpname  FROM dd04l
                            INTO lv_shlpname
                            WHERE rollname = ls_field_descr-rollname
                            AND   shlpname <> space.

    IF sy-subrc = 0.
* ---------- Searchhelp exists for given data element ---------------------------------------------
      rs_shlp_type-type   = mc_shlp_type_ddic_shlp.
      rs_shlp_type-id     = lv_shlpname.
      rs_shlp_type-label  = ls_field_descr-scrtext_l.

* ---------- We have all needed info -> leave -----------------------------------------------------
      RETURN.
    ENDIF.

* -------------------------------------------------------------------------------------------------
* 3. Check for domain fix values
* -------------------------------------------------------------------------------------------------
* ---------- Get Domain Fix Values ----------------------------------------------------------------
    SELECT COUNT(*) FROM dd07v
                    WHERE domname     = ls_field_descr-domname
                    AND   ddlanguage  = sy-langu.

    IF sy-subrc = 0 AND
       sy-dbcnt > 0.
* ---------- Fix values existing for given domain -------------------------------------------------
      rs_shlp_type-type   = mc_shlp_type_fix_domval.
      rs_shlp_type-id     = ls_field_descr-domname.
      rs_shlp_type-label  = ls_field_descr-scrtext_l.

* ---------- We have all needed info -> leave -----------------------------------------------------
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD handle_popup_return.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lr_popup_app_shlp_gen TYPE REF TO z2ui5_cl_tool_app_shlp_gen,
          lr_popup_app_vh_gen   TYPE REF TO z2ui5_cl_tool_app_vh_gen,
          lv_result             TYPE stringval.

* ---------- Handle searchhelp popup returning parameter ------------------------------------------
    IF me->mv_check_popup_shlp = abap_true.
      me->mv_check_popup_shlp = abap_false.
* ---------- Get popup app class reference --------------------------------------------------------
      lr_popup_app_shlp_gen = CAST z2ui5_cl_tool_app_shlp_gen( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app )  ).
* ---------- Set returning value ------------------------------------------------------------------
      lv_result = lr_popup_app_shlp_gen->mv_shlp_result.

* ---------- Create token if return parameter has been provided -----------------------------------
      IF lv_result IS NOT INITIAL.
* ---------- Convert searchhelp result to token ---------------------------------------------------
        me->convert_result_to_token( EXPORTING ir_client         = ir_client
                                               iv_shlp_fieldname = me->mv_selopt_fieldname
                                               iv_result         = lv_result
                                     CHANGING  cs_shlp_fields    = cs_shlp_fields ).
      ENDIF.

* ---------- Re-render main app -------------------------------------------------------------------
      me->on_rendering( ir_client = ir_client ).
    ENDIF.

* ---------- Handle value help popup for domain fix values returning parameter --------------------
    IF me->mv_check_popup_fix_val = abap_true.
      me->mv_check_popup_fix_val = abap_false.
* ---------- Get popup app class reference --------------------------------------------------------
      lr_popup_app_vh_gen = CAST z2ui5_cl_tool_app_vh_gen( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app )  ).
* ---------- Set returning value ------------------------------------------------------------------
      lv_result = lr_popup_app_vh_gen->mv_result.

* ---------- Create token if return parameter has been provided -----------------------------------
      IF lv_result IS NOT INITIAL.
* ---------- Convert searchhelp result to token ---------------------------------------------------
        me->convert_result_to_token( EXPORTING ir_client         = ir_client
                                               iv_shlp_fieldname = me->mv_selopt_fieldname
                                               iv_result         = lv_result
                                     CHANGING  cs_shlp_fields    = cs_shlp_fields ).
      ENDIF.

* ---------- Re-render main app -------------------------------------------------------------------
      me->on_rendering( ir_client = ir_client ).
    ENDIF.

* ---------- Handle value help popup for SHLP user-exit returning parameter -----------------------
    IF me->mv_check_popup_shlp_exit = abap_true.
      me->mv_check_popup_shlp_exit = abap_false.
* ---------- Get popup app class reference --------------------------------------------------------
      lr_popup_app_vh_gen = CAST z2ui5_cl_tool_app_vh_gen( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app )  ).
* ---------- Set returning value ------------------------------------------------------------------
      lv_result = lr_popup_app_vh_gen->mv_result.

* ---------- Create token if return parameter has been provided -----------------------------------
      IF lv_result IS NOT INITIAL.
* ---------- Convert searchhelp result to token ---------------------------------------------------
        me->convert_result_to_token( EXPORTING ir_client         = ir_client
                                               iv_shlp_fieldname = me->mv_selopt_fieldname
                                               iv_result         = lv_result
                                     CHANGING  cs_shlp_fields    = cs_shlp_fields ).
      ENDIF.

* ---------- Re-render main app -------------------------------------------------------------------
      me->on_rendering( ir_client = ir_client ).
    ENDIF.
  ENDMETHOD.


  METHOD generate_vh_domain_fix_val.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lt_dd07v  TYPE z2ui5_cl_tool_app_shlp_gen=>tt_dd07v,
          lt_vh     TYPE z2ui5_cl_tool_app_vh_gen=>tt_data,
          ls_config TYPE z2ui5_cl_tool_app_vh_gen=>ts_config.

    FIELD-SYMBOLS:<ls_dd07v> TYPE dd07v,
                  <ls_vh>    TYPE z2ui5_cl_tool_app_vh_gen=>ts_data.

* ---------- Get Domain Fix Values ----------------------------------------------------------------
    SELECT FROM dd07v AS domain
      FIELDS *
      WHERE domname     = @is_shlp_type-id
      AND   ddlanguage  = @sy-langu
      INTO TABLE @lt_dd07v.

* ---------- Sort domain fix values by domain value key -------------------------------------------
    SORT lt_dd07v BY valpos.

* ---------- Map domain fix values into generic value help structure ------------------------------
    LOOP AT lt_dd07v ASSIGNING <ls_dd07v>.
      APPEND INITIAL LINE TO lt_vh ASSIGNING <ls_vh>.
      <ls_vh>-col01 = <ls_dd07v>-domvalue_l.
      <ls_vh>-col02 = <ls_dd07v>-ddtext.
    ENDLOOP.

* ---------- Set value help title -----------------------------------------------------------------
    ls_config-popup_title = is_shlp_type-label.

* ---------- Set value help window content size ---------------------------------------------------
    ls_config-contentheight = '25%'.
    ls_config-contentwidth = '25%'.

* ---------- Set field configuration --------------------------------------------------------------
    ls_config-fields = VALUE #( ( fieldname = 'COL01'
                                  label     = is_shlp_type-label
                                  width     = '10%'
                                  retval    = abap_true )

                                ( fieldname = 'COL02'
                                  label = TEXT-t06
                                  width = '10%' ) ).

* ---------- Open value help popup window ---------------------------------------------------------
    mv_check_popup_fix_val = abap_true.
    ir_client->nav_app_call( z2ui5_cl_tool_app_vh_gen=>factory( it_data   = lt_vh
                                                                is_config = ls_config ) ).
  ENDMETHOD.


  METHOD generate_vh_user_exit.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lv_shlp_exit  TYPE string,
          lt_param      TYPE abap_parmbind_tab,
          lv_class_name TYPE string,
          lv_meth_name  TYPE string,
          ls_vh_config  TYPE z2ui5_cl_tool_app_vh_gen=>ts_config,
          lt_vh_data    TYPE z2ui5_cl_tool_app_vh_gen=>tt_data,
          lv_rollname   TYPE rollname.

    IF NOT line_exists( me->mt_shlp_exit[ rollname = is_shlp_type-id ] ).
* ---------- No user-exit found for given data element --------------------------------------------
      RETURN.
    ENDIF.

* ---------- Get user-exit name ([CLASS NAME]=>[METHOD_NAME]) -------------------------------------
    lv_shlp_exit = me->mt_shlp_exit[ rollname = is_shlp_type-id ]-exit.

    IF lv_shlp_exit IS INITIAL.
* ---------- No user-exit found for given data element --------------------------------------------
      RETURN.
    ENDIF.

* ---------- Call searchhelp user-exit ------------------------------------------------------------
    IF lv_shlp_exit IS NOT INITIAL.
* ---------- Split exit name into class and method ------------------------------------------------
      SPLIT lv_shlp_exit AT '=>' INTO lv_class_name lv_meth_name.

* ---------- Set data element ID ------------------------------------------------------------------
      lv_rollname = is_shlp_type-id.

* ---------- Build parameter list -----------------------------------------------------------------
      lt_param = VALUE #( ( name = 'IV_ROLLNAME'
                            kind = cl_abap_objectdescr=>exporting
                            value = REF #( lv_rollname ) )
                           ( name = 'ES_CONFIG'
                            kind = cl_abap_objectdescr=>importing
                            value = REF #( ls_vh_config ) )
                           ( name = 'ET_DATA'
                            kind = cl_abap_objectdescr=>importing
                            value = REF #( lt_vh_data ) ) ).

* ---------- Call exit ----------------------------------------------------------------------------
      CALL METHOD (lv_class_name)=>(lv_meth_name)
        PARAMETER-TABLE lt_param.
    ENDIF.

* ---------- Nothing has been provide via user-exit -> leave --------------------------------------
    IF ls_vh_config IS INITIAL OR
       lt_vh_data   IS INITIAL.
      RETURN.
    ENDIF.

* ---------- Open value help popup window ---------------------------------------------------------
    me->mv_check_popup_shlp_exit = abap_true.
    ir_client->nav_app_call( z2ui5_cl_tool_app_vh_gen=>factory( it_data   = lt_vh_data
                                                                is_config = ls_vh_config ) ).

  ENDMETHOD.


  METHOD template_vh_user_exit.
*----------------------------------------------------------------------*
* Template for value help user-exit
*----------------------------------------------------------------------*

**----------------------------------------------------------------------*
** LOCAL DATA DEFINITION
**----------------------------------------------------------------------*
*    DATA: lt_dd07v  TYPE z2ui5_cl_tool_app_shlp_gen=>tt_dd07v.
*
*    FIELD-SYMBOLS:<ls_dd07v> TYPE dd07v,
*                  <ls_vh>    TYPE z2ui5_cl_tool_app_vh_gen=>ts_data.
*
** ---------- Get Domain Fix Values ----------------------------------------------------------------
*        SELECT FROM dd07v AS domain
*          FIELDS *
*          WHERE domname     = ''
*          AND   ddlanguage  = @sy-langu
*          INTO TABLE @lt_dd07v.
*
** ---------- Sort domain fix values by domain value key -------------------------------------------
*        SORT lt_dd07v BY valpos.
*
** ---------- Map domain fix values into generic value help structure ------------------------------
*        LOOP AT lt_dd07v ASSIGNING <ls_dd07v>.
*          APPEND INITIAL LINE TO et_data ASSIGNING <ls_vh>.
*          <ls_vh>-col01 = <ls_dd07v>-domvalue_l.
*          <ls_vh>-col02 = <ls_dd07v>-ddtext.
*        ENDLOOP.
*
** ---------- Set value help title -----------------------------------------------------------------
*        es_config-popup_title = 'My own label'.
*
** ---------- Set value help window content size ---------------------------------------------------
*        es_config-contentheight = '25%'.
*        es_config-contentwidth = '25%'.
*
** ---------- Set field configuration --------------------------------------------------------------
*        es_config-fields = VALUE #( ( fieldname = 'COL01'
*                                      label     = 'Code'
*                                      width     = '10%'
*                                      retval    = abap_true )
*
*                                    ( fieldname = 'COL02'
*                                      label = 'Description'
*                                      width = '10%' ) ).
*
  ENDMETHOD.
ENDCLASS.
