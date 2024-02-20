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
      RETURNING
        VALUE(result)           TYPE REF TO z2ui5_cl_tool_app_shlp_gen .
  PROTECTED SECTION.

    DATA mv_selopt_fieldname TYPE string .
    DATA mt_shlp_descr TYPE tt_shlp_descr .
    DATA mt_shlp_blacklist TYPE tt_shlp_blacklist .

    METHODS on_rendering
      IMPORTING
        !ir_client TYPE REF TO z2ui5_if_client .
    METHODS on_event
      IMPORTING
        !ir_client TYPE REF TO z2ui5_if_client .
    METHODS generate_ddic_shlp
      IMPORTING
        !ir_parent  TYPE REF TO z2ui5_cl_xml_view
        !ir_client  TYPE REF TO z2ui5_if_client
        !iv_shlp_id TYPE char30 .
    METHODS select_ddic_shlp
      IMPORTING
        !ir_controller TYPE REF TO object
        !iv_shlp_id    TYPE char30
        !iv_maxrows    TYPE i DEFAULT 150 .
    METHODS build_data_ref
      IMPORTING
        !ir_client     TYPE REF TO z2ui5_if_client
        !it_shlp_descr TYPE tt_shlp_descr .
    METHODS generate_ddic_shlp_selopt
      IMPORTING
        !ir_client    TYPE REF TO z2ui5_if_client
        !iv_fieldname TYPE clike
        !iv_shlp_id   TYPE char30 .
    METHODS get_shlp_range_by_value
      IMPORTING
        !iv_value        TYPE string
      RETURNING
        VALUE(rs_result) TYPE ts_range .
    METHODS get_shlp_uuid
      RETURNING
        VALUE(rv_result) TYPE string .
    METHODS get_selopt_mapping
      RETURNING
        VALUE(rt_mapping) TYPE tt_selopt_mapping .
    METHODS on_init
      IMPORTING
        !ir_client TYPE REF TO z2ui5_if_client .
    METHODS expand_searchhelp
      IMPORTING
        !iv_shlp_id          TYPE char30
        !it_shlp_blacklist   TYPE tt_shlp_blacklist
      EXPORTING
        VALUE(et_shlp_descr) TYPE tt_shlp_descr
        !ev_shlp_selkey      TYPE char30 .
    METHODS get_data_ref
      IMPORTING
        !iv_index       TYPE i OPTIONAL
        !iv_shlp_id     TYPE char30 OPTIONAL
      EXPORTING
        !er_shlp_fields TYPE REF TO data
        !er_shlp_result TYPE REF TO data .
    METHODS create_data_ref
      IMPORTING
        !iv_index      TYPE i
        !ir_struc_type TYPE REF TO cl_abap_structdescr
        !ir_table_type TYPE REF TO cl_abap_tabledescr .
    METHODS init_data_ref .
    METHODS fill_token
      IMPORTING
        !it_filter TYPE tt_filter_prop
      CHANGING
        !ct_token  TYPE tt_token .
    METHODS fill_filter
      IMPORTING
        !it_token  TYPE tt_token
      CHANGING
        !ct_filter TYPE tt_filter_prop .
    METHODS get_input_fieldname
      IMPORTING
        !iv_fieldname             TYPE char30
      RETURNING
        VALUE(rv_input_fieldname) TYPE char30 .
    METHODS delete_token
      IMPORTING
        !iv_token_key TYPE clike
      CHANGING
        !ct_token     TYPE tt_token .
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_cl_tool_app_shlp_gen IMPLEMENTATION.


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
    DATA: ls_shlp TYPE  shlp_descr.

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
        shlp_tab = et_shlp_descr.

* ---------- Apply blacklist (Remove unwanted searchhelp from the list) ---------------------------
    LOOP AT it_shlp_blacklist ASSIGNING FIELD-SYMBOL(<ls_shlp_blacklist>).
      DELETE et_shlp_descr WHERE shlpname = <ls_shlp_blacklist>-shlp_id.
    ENDLOOP.

* ---------- Set default searchhelp ---------------------------------------------------------------
    IF line_exists( et_shlp_descr[ 1 ] ).
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
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = iv_shlp_id
      IMPORTING
        shlp     = ls_shlp.

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
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) ) ) )
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
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) ) ) )
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
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) ) ) )
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
                                                                               t_arg  = VALUE #( (  CONV #( <ls_fieldprop_sel>-fieldname ) ) ) )
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
    DATA(lr_popup) = z2ui5_cl_xml_view=>factory_popup( ).

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
          ls_range     TYPE ts_range.

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

        IF NOT line_exists( lt_event_arg[ 1 ] ).
          RETURN.
        ENDIF.

* ---------- Set select-option field name and title -----------------------------------------------
        CLEAR: me->mv_selopt_fieldname.
        me->mv_selopt_fieldname = lt_event_arg[ 1 ].

* ---------- Assign current token field -----------------------------------------------------------
        ASSIGN COMPONENT me->mv_selopt_fieldname OF STRUCTURE <ls_shlp_fields> TO <lt_field_token>.
        IF <lt_field_token> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

* ---------- Close searchhelp Popup window --------------------------------------------------------
        ir_client->popup_destroy( ).

* ---------- Fill select-option filter ------------------------------------------------------------
        me->fill_filter( EXPORTING it_token  = <lt_field_token>
                         CHANGING  ct_filter = me->mt_filter ).

* ---------- Handle select-option popup opening ---------------------------------------------------
        me->generate_ddic_shlp_selopt( ir_client    = ir_client
                                       iv_fieldname = me->mv_selopt_fieldname
                                       iv_shlp_id   = me->ms_screen-shlp_selkey ).

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
          lv_meth_name     TYPE string.

    FIELD-SYMBOLS: <ls_fielddescr>    TYPE dfies,
                   <ls_record_tab>    TYPE seahlpres,
                   <ls_result>        TYPE any,
                   <ls_shlp_fields>   TYPE any,
                   <lv_field>         TYPE any,
                   <lt_token>         TYPE tt_token,
                   <ls_token>         TYPE ts_token,
                   <ls_fieldprop_sel> TYPE ddshfprop,
                   <ls_fieldprop_lis> TYPE ddshfprop,
                   <lt_result_itab>   TYPE STANDARD TABLE.

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
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = iv_shlp_id
      IMPORTING
        shlp     = ls_shlp.

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
        CLEAR: ls_range, lv_date_out.

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
        ENDCASE.

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
ENDCLASS.
