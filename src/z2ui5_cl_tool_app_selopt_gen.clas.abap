class Z2UI5_CL_TOOL_APP_SELOPT_GEN definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .
  interfaces Z2UI5_IF_APP .

  types:
    BEGIN OF ts_token,
        key      TYPE string,
        text     TYPE string,
        visible  TYPE abap_bool,
        selkz    TYPE abap_bool,
        editable TYPE abap_bool,
      END OF ts_token .
  types:
    tt_token TYPE STANDARD TABLE OF ts_token WITH KEY key .
  types:
    tt_range TYPE RANGE OF string .
  types:
    ts_range TYPE LINE OF tt_range .
  types:
    BEGIN OF ts_filter_pop,
        option TYPE string,
        low    TYPE string,
        high   TYPE string,
        key    TYPE string,
      END OF ts_filter_pop .
  types:
    tt_filter_prop TYPE STANDARD TABLE OF ts_filter_pop WITH EMPTY KEY .
  types:
    BEGIN OF ts_selopt_mapping,
        key   TYPE string,
        text  TYPE string,
        value TYPE string,
      END OF ts_selopt_mapping .
  types:
    tt_selopt_mapping TYPE STANDARD TABLE OF ts_selopt_mapping WITH KEY key .
  types:
    BEGIN OF ts_config,
        popup_title         TYPE string,
        header_text         TYPE string,
        contentheight       TYPE string,
        contentwidth        TYPE string,
        datatype            TYPE string,
        date_display_format TYPE string,
        date_value_format   TYPE string,
        time_display_format TYPE string,
        time_value_format   TYPE string,
      END OF ts_config .

  data MT_TOKEN_RESULT type TT_TOKEN .
  constants MC_SORT_DIR_ASC type STRING value 'ASCENDING' ##NO_TEXT.
  constants MC_SORT_DIR_DESC type STRING value 'DESCENDING' ##NO_TEXT.
  constants MC_EVT_SEARCH type STRING value 'EVT_SEARCH' ##NO_TEXT.
  constants MC_EVT_CANCEL type STRING value 'EVT_CANCEL' ##NO_TEXT.
  constants MC_EVT_CONFIRM type STRING value 'EVT_CONFIRM' ##NO_TEXT.
  data MS_CONFIG type TS_CONFIG .
  data MT_FILTER type TT_FILTER_PROP .
  class-data MT_MAPPING type TT_SELOPT_MAPPING .
  constants MC_EVT_SHLP_SELOPT_DELETE type STRING value 'EVT_SHLP_SELOPT_DELETE' ##NO_TEXT.
  constants MC_EVT_SHLP_SELOPT_DELETE_ALL type STRING value 'EVT_SHLP_SELOPT_DELETE_ALL' ##NO_TEXT.
  constants MC_EVT_SHLP_SELOPT_ADD type STRING value 'EVT_SHLP_SELOPT_ADD' ##NO_TEXT.
  constants MC_EVT_SHLP_SELOPT_CANCEL type STRING value 'EVT_SHLP_SELOPT_CANCEL' ##NO_TEXT.
  constants MC_EVT_SHLP_SELOPT_OK type STRING value 'EVT_SHLP_SELOPT_OK' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  class-methods FACTORY
    importing
      !IS_CONFIG type TS_CONFIG
      !IT_TOKEN type TT_TOKEN optional
    returning
      value(RESULT) type ref to Z2UI5_CL_TOOL_APP_SELOPT_GEN .
  class-methods GET_SELOPT_MAPPING
    returning
      value(RT_MAPPING) type TT_SELOPT_MAPPING .
  class-methods FILL_TOKEN
    importing
      !IT_FILTER type TT_FILTER_PROP
    changing
      !CT_TOKEN type TT_TOKEN .
  class-methods GET_SHLP_RANGE_BY_VALUE
    importing
      !IV_VALUE type STRING
    returning
      value(RS_RESULT) type TS_RANGE .
  class-methods FILL_FILTER
    importing
      !IT_TOKEN type TT_TOKEN
    changing
      !CT_FILTER type TT_FILTER_PROP .
  class-methods GET_SHLP_UUID
    returning
      value(RV_RESULT) type STRING .
protected section.

  data MV_CHECK_INITIALIZED type ABAP_BOOL .
  data MT_TOKEN type TT_TOKEN .

  methods ON_RENDERING
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
  methods ON_EVENT
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
  methods ON_INIT
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
private section.
ENDCLASS.



CLASS Z2UI5_CL_TOOL_APP_SELOPT_GEN IMPLEMENTATION.


  METHOD factory.
* ---------- Create new value help instance -------------------------------------------------------
    result = NEW #( ).

* -------------------------------------------------------------------------------------------------
* Set value help config
* popup_title   -> Title of the popup window
* header_text   -> Header text of the popup window
* contentheight -> Height of the popup content (default 50%)
* contentwidth  -> Width of the popup content (default 50%)
* data_type     -> Data type of the select-option field (DATS, TIMS or blank (all others))
* -------------------------------------------------------------------------------------------------
    result->ms_config = is_config.

* ---------- Set tokens ---------------------------------------------------------------------------
    result->mt_token = it_token.

  ENDMETHOD.


  method FILL_FILTER.
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
        ls_range = get_shlp_range_by_value( iv_value = <lv_field> ).
        IF ls_range IS INITIAL.
          CONTINUE.
        ENDIF.

* ---------- Build new filter record --------------------------------------------------------------
        APPEND INITIAL LINE TO ct_filter ASSIGNING <ls_filter>.
        <ls_filter>-key     = get_shlp_uuid( ).
        <ls_filter>-option  = ls_range-option.
        <ls_filter>-low     = ls_range-low.
        <ls_filter>-high    = ls_range-high.

      ENDLOOP.
    ENDIF.
  endmethod.


  METHOD fill_token.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    FIELD-SYMBOLS: <ls_filter>      TYPE ts_filter_pop,
                   <ls_field_token> TYPE ts_token,
                   <lv_field>       TYPE any.

* ---------- Fill token ---------------------------------------------------------------------------
    LOOP AT it_filter REFERENCE INTO DATA(lr_filter).

      DATA(lv_value) = mt_mapping[ key = lr_filter->option ]-value.
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
    DATA: lv_uuid TYPE sysuuid_c32.

    TRY.
        CALL METHOD ('CL_SYSTEM_UUID')=>create_uuid_c32_static
          RECEIVING
            uuid = lv_uuid.
      CATCH cx_sy_dyn_call_illegal_class.

        DATA(lv_fm) = 'GUID_CREATE'.
        CALL FUNCTION lv_fm
          IMPORTING
            ev_guid_32 = lv_uuid.
    ENDTRY.

    rv_result = lv_uuid.
  ENDMETHOD.


  METHOD on_event.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lt_event_arg TYPE string_table.

* ---------- Get event parameters -----------------------------------------------------------------
    lt_event_arg = ir_client->get( )-t_event_arg.

    CASE ir_client->get( )-event.
      WHEN mc_evt_shlp_selopt_add.
        INSERT VALUE #( key = me->get_shlp_uuid( ) ) INTO TABLE me->mt_filter.

        ir_client->popup_model_update( ).

      WHEN mc_evt_shlp_selopt_cancel.
* ---------- Init token result --------------------------------------------------------------------
        CLEAR: me->mt_token, me->mt_token_result.

* ---------- Close select-option Popup window -----------------------------------------------------
        ir_client->popup_destroy( ).
        ir_client->nav_app_leave( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app_stack ) ).
        RETURN.

      WHEN mc_evt_shlp_selopt_ok.

* ---------- Check input --------------------------------------------------------------------------
        LOOP AT me->mt_filter ASSIGNING FIELD-SYMBOL(<ls_filter>).
          IF <ls_filter>-option IS INITIAL.
* ---------- Raise error message ------------------------------------------------------------------
            ir_client->message_box_display( text = TEXT-e01
                                            type = 'error' ).
            RETURN.
          ENDIF.
        ENDLOOP.

* ---------- Fill token ---------------------------------------------------------------------------
        fill_token( EXPORTING it_filter = me->mt_filter
                    CHANGING  ct_token  = me->mt_token ).

* ---------- Set token result ---------------------------------------------------------------------
        me->mt_token_result = me->mt_token.

* ---------- Close select-option Popup window -----------------------------------------------------
        ir_client->popup_destroy( ).
        ir_client->nav_app_leave( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app_stack ) ).
        RETURN.


      WHEN mc_evt_shlp_selopt_delete.
        IF NOT line_exists( lt_event_arg[ 1 ] ).
          RETURN.
        ENDIF.

        DELETE me->mt_filter WHERE key = lt_event_arg[ 1 ].
        ir_client->popup_model_update( ).

      WHEN mc_evt_shlp_selopt_delete_all.
        me->mt_filter = VALUE #( ).
        ir_client->popup_model_update( ).

    ENDCASE.
  ENDMETHOD.


  METHOD on_init.
    IF mv_check_initialized = abap_false.
      mv_check_initialized = abap_true.

* ---------- Check config -------------------------------------------------------------------------
      IF me->ms_config IS INITIAL.
* ---------- Raise error message ------------------------------------------------------------------
        ir_client->message_box_display( text = TEXT-e00
                                        type = 'error' ).
        RETURN.
      ENDIF.

* ---------- Set default height/width -------------------------------------------------------------
      IF me->ms_config-contentheight IS INITIAL.
        me->ms_config-contentheight = '50%'.
      ENDIF.
      IF me->ms_config-contentwidth IS INITIAL.
        me->ms_config-contentwidth = '50%'.
      ENDIF.

* ---------- Import tokens from calling application -----------------------------------------------
      IF me->mt_token IS NOT INITIAL.
        fill_filter( EXPORTING it_token  = me->mt_token
                     CHANGING   ct_filter = me->mt_filter ).
        CLEAR: me->mt_token.
      ENDIF.

* ---------- Perform searchhelp popup rendering ---------------------------------------------------
      me->on_rendering( ir_client = ir_client ).
    ENDIF.
  ENDMETHOD.


  METHOD on_rendering.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*

* ---------- Create Popup -------------------------------------------------------------------------
    DATA(lr_popup) = z2ui5_cl_xml_view=>factory_popup( ).

* ---------- Create Dialog ------------------------------------------------------------------------
    DATA(lr_dialog) = lr_popup->dialog( contentheight = me->ms_config-contentheight
                                        contentwidth  = me->ms_config-contentwidth
                                        title         = me->ms_config-popup_title ).

* ---------- Create Vbox --------------------------------------------------------------------------
    DATA(lr_vbox) = lr_dialog->content( )->vbox( height = `100%` justifycontent = 'SpaceBetween' ).

* ---------- Create Panel -------------------------------------------------------------------------
    DATA(lr_panel)  = lr_vbox->panel( expandable = abap_false
                                      expanded   = abap_true
                                      headertext = me->ms_config-header_text ).

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
                 items       = ir_client->_bind_edit( mt_mapping )
             )->item( key = '{KEY}'
                      text = '{TEXT}' ).

* ---------- Create input field based on the data type --------------------------------------------
    CASE me->ms_config-datatype.
      WHEN 'DATS'.
        lr_grid->date_picker( value  = `{LOW}`  valueformat = me->ms_config-date_value_format displayformat = me->ms_config-date_display_format ).
        lr_grid->date_picker( value = `{HIGH}`  valueformat = me->ms_config-date_value_format displayformat = me->ms_config-date_display_format enabled = `{= ${OPTION} === 'BT' }` ).
      WHEN 'TIMS'.
        lr_grid->time_picker( value  = `{LOW}` valueformat = me->ms_config-time_value_format displayformat = me->ms_config-time_display_format ).
        lr_grid->time_picker( value = `{HIGH}` valueformat = me->ms_config-time_value_format displayformat = me->ms_config-time_display_format enabled = `{= ${OPTION} === 'BT' }` ).
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
        )->button( text = TEXT-t00 icon = 'sap-icon://delete' type = `Transparent` press = ir_client->_event( val = mc_evt_shlp_selopt_delete_all )
        )->button( text = TEXT-t01 press = ir_client->_event( mc_evt_shlp_selopt_ok ) type  = 'Emphasized'
        )->button( text = TEXT-t02 press = ir_client->_event( mc_evt_shlp_selopt_cancel ) ).

* ---------- Display popup window -----------------------------------------------------------------
    ir_client->popup_display( lr_popup->stringify( ) ).

  ENDMETHOD.


  METHOD Z2UI5_IF_APP~MAIN.

    me->on_init( ir_client = client ).

    me->on_event( ir_client = client ).

  ENDMETHOD.


  METHOD class_constructor.
* ---------- Prefill select-option mapping table --------------------------------------------------
    mt_mapping = get_selopt_mapping( ).
  ENDMETHOD.
ENDCLASS.
