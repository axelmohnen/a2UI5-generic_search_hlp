class Z2UI5_CL_TOOL_APP_VH_GEN definition
  public
  final
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .
  interfaces Z2UI5_IF_APP .

  types:
    BEGIN OF ts_data,
        selkz TYPE abap_bool,
        col01 TYPE string,
        col02 TYPE string,
        col03 TYPE string,
        col04 TYPE string,
        col05 TYPE string,
        col06 TYPE string,
        col07 TYPE string,
        col08 TYPE string,
        col09 TYPE string,
        col10 TYPE string,
      END OF ts_data .
  types:
    tt_data TYPE STANDARD TABLE OF ts_data WITH DEFAULT KEY .
  types:
    BEGIN OF ts_config_fields,
        fieldname TYPE string,
        label     TYPE string,
        width     TYPE string,
        retval    TYPE boole_d,
      END OF ts_config_fields .
  types:
    tt_config_fields TYPE STANDARD TABLE OF ts_config_fields WITH KEY fieldname .
  types:
    BEGIN OF ts_config,
        POPUP_TITLE   type string,
        contentheight TYPE string,
        contentwidth  TYPE string,
        sortby        TYPE string,
        sortdir       TYPE string,
        fields        TYPE tt_config_fields,
      END OF ts_config .

  data MV_RESULT type STRING .
  constants MC_SORT_DIR_ASC type STRING value 'ASCENDING' ##NO_TEXT.
  constants MC_SORT_DIR_DESC type STRING value 'DESCENDING' ##NO_TEXT.
  constants MC_EVT_SEARCH type STRING value 'EVT_SEARCH' ##NO_TEXT.
  constants MC_EVT_CANCEL type STRING value 'EVT_CANCEL' ##NO_TEXT.
  constants MC_EVT_CONFIRM type STRING value 'EVT_CONFIRM' ##NO_TEXT.
  data MT_DATA type TT_DATA .
  data MS_CONFIG type TS_CONFIG .

  class-methods FACTORY
    importing
      !IT_DATA type TT_DATA
      !IS_CONFIG type TS_CONFIG
    returning
      value(RESULT) type ref to Z2UI5_CL_TOOL_APP_VH_GEN .
protected section.

  data MT_DATA_BAK type TT_DATA .
  data MV_CHECK_INITIALIZED type ABAP_BOOL .

  methods ON_RENDERING
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
  methods ON_EVENT
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
  methods ON_INIT
    importing
      !IR_CLIENT type ref to Z2UI5_IF_CLIENT .
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2UI5_CL_TOOL_APP_VH_GEN IMPLEMENTATION.


  METHOD factory.
* ---------- Create new value help instance -------------------------------------------------------
    result = NEW #( ).

* ---------- Set value help data ------------------------------------------------------------------
    result->mt_data     = it_data.
    result->mt_data_bak = it_data.

* -------------------------------------------------------------------------------------------------
* Set value help config
* popup_title   -> Title of the popup window
* contentheight -> Height of the popup content
* contentwidth  -> Width of the popup content
* sortby        -> fieldname to be sorted by
* sortdir       -> sorting direction (MC_SORT_DIR_ASC or MC_SORT_DIR_DESC)
* fields        -> fieldname list. Must be correspond to the MT_DATA fieldnames (COL01 -> COL10)
*   fieldname -> fieldname from MT_DATA (COL01 -> COL10)
*   label     -> Column label
*   width     -> Column width
*   retval    -> Fieldvalue to be returned. Only one returning value is supported (ABAP_TRUE/ABAP_FALSE)
* -------------------------------------------------------------------------------------------------
    result->ms_config = is_config.
  ENDMETHOD.


  METHOD on_event.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lv_search_criteria TYPE string,
          lv_tabix           TYPE sy-tabix,
          lv_col_id          TYPE numc2,
          lv_fieldname       TYPE string,
          lv_skip_delete     TYPE boole_d,
          ls_data            TYPE ts_data,
          lv_return_field    TYPE string.

    FIELD-SYMBOLS: <ls_data>       TYPE ts_data,
                   <lv_fieldvalue> TYPE any.

    CASE ir_client->get( )-event.
      WHEN mc_evt_cancel.
* ---------- Init returning value -----------------------------------------------------------------
        CLEAR: me->mv_result.

* ---------- Leave popup --------------------------------------------------------------------------
        ir_client->popup_destroy( ).
        ir_client->nav_app_leave( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app_stack ) ).
      WHEN mc_evt_confirm.
* ---------- Has a record been selected? ----------------------------------------------------------
        IF line_exists( me->mt_data[ selkz = abap_true ] ).
          ls_data = me->mt_data[ selkz = abap_true ].
        ELSE.
          RETURN.
        ENDIF.

* ---------- Get returning field name -------------------------------------------------------------
        IF line_exists( me->ms_config-fields[ retval = abap_true ] ).
          lv_return_field = me->ms_config-fields[ retval = abap_true ]-fieldname.
        ELSE.
          RETURN.
        ENDIF.

* ---------- Assign returning value ---------------------------------------------------------------
        ASSIGN COMPONENT lv_return_field OF STRUCTURE ls_data TO <lv_fieldvalue>.
        IF <lv_fieldvalue> IS NOT ASSIGNED.
          RETURN.
        ENDIF.

* ---------- Set returning value ------------------------------------------------------------------
        me->mv_result = <lv_fieldvalue>.

* ---------- Leave popup --------------------------------------------------------------------------
        ir_client->popup_destroy( ).
        ir_client->nav_app_leave( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN mc_evt_search.
* ---------- Get the search argument --------------------------------------------------------------
        DATA(lt_arg) = ir_client->get( )-t_event_arg.
        IF line_exists( lt_arg[ 1 ] ).
          lv_search_criteria = lt_arg[ 1 ].
        ELSE.
          RETURN.
        ENDIF.

* ---------- Reset data table from backup ---------------------------------------------------------
        me->mt_data = me->mt_data_bak.

* ---------- Leave -> no search criteria has been provided ----------------------------------------
        IF lv_search_criteria IS INITIAL.
* ---------- Update model binding -----------------------------------------------------------------
          ir_client->popup_model_update( ).
          RETURN.
        ENDIF.

        LOOP AT me->mt_data ASSIGNING <ls_data>.
* ---------- Init loop data -----------------------------------------------------------------------
          CLEAR: lv_tabix, lv_col_id, lv_fieldname, lv_skip_delete.
          UNASSIGN: <lv_fieldvalue>.

* ---------- Keep table index ---------------------------------------------------------------------
          lv_tabix = sy-tabix.

* ---------- Check all columns against the search criteria ----------------------------------------
          DO 10 TIMES.
* ---------- Init loop data -----------------------------------------------------------------------
            CLEAR: lv_fieldname.
            UNASSIGN: <lv_fieldvalue>.

* ---------- Increment column id ------------------------------------------------------------------
            lv_col_id = lv_col_id  + 1.

* ---------- Build fieldname ----------------------------------------------------------------------
            lv_fieldname = 'COL' && lv_col_id.

* ---------- Assign field value -------------------------------------------------------------------
            ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_data> TO <lv_fieldvalue>.

* ---------- Continue with next record in case no field found or initial --------------------------
            IF <lv_fieldvalue> IS NOT ASSIGNED OR
              <lv_fieldvalue> IS INITIAL.
              CONTINUE.
            ENDIF.

* ---------- Search criteria matchs -> exit loop and continue with next record --------------------
            IF <lv_fieldvalue> CS lv_search_criteria.
              lv_skip_delete = abap_true.
              EXIT.
            ENDIF.
          ENDDO.

* ---------- Search crtieria doesn't match -> remove record ---------------------------------------
          IF lv_skip_delete = abap_false.
            DELETE me->mt_data INDEX lv_tabix.
          ENDIF.
        ENDLOOP.

* ---------- Update model binding -----------------------------------------------------------------
        ir_client->popup_model_update( ).
    ENDCASE.
  ENDMETHOD.


  METHOD on_init.
    IF mv_check_initialized = abap_false.
      mv_check_initialized = abap_true.

* ---------- Check config -------------------------------------------------------------------------
      IF me->ms_config-fields IS INITIAL.
* ---------- Raise error message ------------------------------------------------------------------
        ir_client->message_box_display( text = text-e00
                                        type = 'error' ).
        RETURN.
      ENDIF.

* ---------- Perform searchhelp popup rendering ---------------------------------------------------
      me->on_rendering( ir_client = ir_client ).
    ENDIF.
  ENDMETHOD.


  METHOD on_rendering.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lv_items_str       TYPE string,
          lv_sort_descending TYPE string.

    FIELD-SYMBOLS: <ls_config_fields> TYPE ts_config_fields.

* ---------- Create Popup -------------------------------------------------------------------------
    DATA(lr_popup) = z2ui5_cl_xml_view=>factory_popup( ).

* ---------- Build "ITEMS" parameter string -------------------------------------------------------
    IF me->ms_config-sortby IS INITIAL.
* ---------- Value table without sorting ----------------------------------------------------------
      lv_items_str = `{path:'` && ir_client->_bind_edit( val = me->mt_data path = abap_true ) && `'}`.
    ELSE.
* ---------- Value table sorting applied ----------------------------------------------------------
      IF me->ms_config-sortdir = mc_sort_dir_desc.
        lv_sort_descending = 'true'.
      ELSE.
        lv_sort_descending = 'false'.
      ENDIF.

      lv_items_str = `{path:'` && ir_client->_bind_edit( val = me->mt_data path = abap_true ) && `', sorter : { path : '` && me->ms_config-sortby && `', descending : ` && lv_sort_descending && ` } }`.
    ENDIF.

* ---------- Create Dialog ------------------------------------------------------------------------
    DATA(lr_dialog) = lr_popup->table_select_dialog( title          = ir_client->_bind( me->ms_config-popup_title )
                                                     items          = lv_items_str
                                                     contentheight  = ir_client->_bind( me->ms_config-contentheight )
                                                     contentwidth   = ir_client->_bind( me->ms_config-contentwidth )
                                                     cancel         = ir_client->_event( mc_evt_cancel )
                                                     search         = ir_client->_event( val = mc_evt_search t_arg = VALUE #( ( `${$parameters>/value}` ) ( `${$parameters>/clearButtonPressed}`  ) ) )
                                                     confirm        = ir_client->_event( val = mc_evt_confirm t_arg = VALUE #( ( `${$parameters>/selectedContexts[0]/sPath}` ) ) ) ).

* --------- Create list item ----------------------------------------------------------------------
    DATA(lr_list_item) = lr_dialog->column_list_item( valign = `Top` selected = `{SELKZ}` ).

* --------- Create cells --------------------------------------------------------------------------
    DATA(lr_cells) = lr_list_item->cells( ).

* --------- Set fieldnames ------------------------------------------------------------------------
    LOOP AT me->ms_config-fields ASSIGNING <ls_config_fields>.
      lr_cells->text( text = `{` && <ls_config_fields>-fieldname && `}` ).
    ENDLOOP.

* --------- Create columns ------------------------------------------------------------------------
    DATA(lr_columns) = lr_dialog->columns( ).

* --------- Set field labels ----------------------------------------------------------------------
    LOOP AT me->ms_config-fields ASSIGNING <ls_config_fields>.
      lr_columns->column( width = <ls_config_fields>-width )->header( ns = `` )->text( text = <ls_config_fields>-label ).
    ENDLOOP.

* --------- Display popup -------------------------------------------------------------------------
    ir_client->popup_display( lr_popup->stringify( ) ).

  ENDMETHOD.


  METHOD Z2UI5_IF_APP~MAIN.

    me->on_init( ir_client = client ).

    me->on_event( ir_client = client ).

  ENDMETHOD.
ENDCLASS.
