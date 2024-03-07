class Z2UI5_CL_TOOL_APP_13 definition
  public
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .
  interfaces Z2UI5_IF_APP .

  types TS_T001 type T001 .
  types:
    tt_t001 type STANDARD TABLE OF ts_t001 WITH DEFAULT KEY .

  data:
    BEGIN OF ms_screen,
        selopt TYPE string,
      END OF ms_screen .
  data MV_CHECK_POPUP type ABAP_BOOL .
  data MV_CHECK_INITIALIZED type ABAP_BOOL .
  data MT_T001 type TT_T001 .
  PROTECTED SECTION.

    METHODS z2ui5_on_init .
    METHODS z2ui5_on_event
      IMPORTING
        !ir_client TYPE REF TO z2ui5_if_client .
    METHODS z2ui5_on_render
      IMPORTING
        !ir_client TYPE REF TO z2ui5_if_client .
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2UI5_CL_TOOL_APP_13 IMPLEMENTATION.


  METHOD Z2UI5_IF_APP~MAIN.

    IF mv_check_initialized = abap_false.
      mv_check_initialized = abap_true.
      z2ui5_on_init( ).
      z2ui5_on_render( ir_client = client ).
    ENDIF.

    IF mv_check_popup = abap_true.
      mv_check_popup = abap_false.
      DATA(app) = CAST z2ui5_cl_tool_app_vh_gen( client->get_app( client->get( )-s_draft-id_prev_app )  ).
      client->message_toast_display( app->mv_result ).
      me->ms_screen-selopt = app->mv_result.
      z2ui5_on_render( ir_client = client ).
    ENDIF.

    z2ui5_on_event( ir_client = client ).


  ENDMETHOD.


  METHOD Z2UI5_ON_EVENT.
*----------------------------------------------------------------------*
* LOCAL DATA DEFINITION
*----------------------------------------------------------------------*
    DATA: lt_data   TYPE z2ui5_cl_tool_app_vh_gen=>tt_data,
          ls_config TYPE z2ui5_cl_tool_app_vh_gen=>ts_config,
          lt_t001   TYPE TABLE OF t001.

    FIELD-SYMBOLS: <ls_t001> TYPE t001,
                   <ls_data> TYPE z2ui5_cl_tool_app_vh_gen=>ts_data.

    CASE ir_client->get( )-event.
      WHEN `FILTER_VALUE_HELP`.
** ---------- Map value help data ------------------------------------------------------------------
*        LOOP AT me->mt_t001 ASSIGNING <ls_t001>.
*          APPEND INITIAL LINE TO lt_data ASSIGNING <ls_data>.
*          <ls_data>-col01 = <ls_t001>-bukrs.
*          <ls_data>-col02 = <ls_t001>-butxt.
*          <ls_data>-col03 = <ls_t001>-ort01.
*        ENDLOOP.
*
** ---------- Set value help title -----------------------------------------------------------------
*        ls_config-popup_title = 'Generic value help for company code'.
*
** ---------- Set value help window content size ---------------------------------------------------
*        ls_config-contentheight = '25%'.
*        ls_config-contentwidth = '25%'.
*
** ---------- Set field configuration --------------------------------------------------------------
*        ls_config-fields = VALUE #( ( fieldname = 'COL01'
*                                      label     = 'Company Code'
*                                      width     = '10%'
*                                      retval    = abap_true )
*
*                                    ( fieldname = 'COL02'
*                                      label     = 'Company Name'
*                                      width     = '10%' )
*
*                                    ( fieldname = 'COL03'
*                                      label = 'City'
*                                      width = '10%' ) ).

* ---------- Open value help popup window ---------------------------------------------------------
        mv_check_popup = abap_true.

        z2ui5_cl_tool_app_vh_gen=>render_vh_domain_fix_values(  ir_client        = IR_CLIENT
                                                                iv_domname       = 'SALV_DO_SELOPT_OPTION'
*          iv_popup_title   =
*          iv_contentheight = '25%'
*          iv_contentwidth  = '25%'
        ).


      WHEN 'BACK'.
        ir_client->nav_app_leave( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app_stack ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD Z2UI5_ON_INIT.

* ---------- Read company codes for value help ----------------------------------------------------
    SELECT * FROM t001 INTO TABLE me->mt_t001.
  ENDMETHOD.


  METHOD Z2UI5_ON_RENDER.
    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    DATA(page) = view->page( id = `page_main`
             title          = 'abap2UI5 - Run generic value help'
             navbuttonpress = ir_client->_event( 'BACK' )
             shownavbutton  = abap_true ).


    DATA(grid) = page->grid( 'L7 M12 S12' )->content( 'layout'
        )->simple_form( 'run value help in new app' )->content( 'form'
            )->label( `Select option`
            )->input(  value = ir_client->_bind_edit( ms_screen-selopt )
                  showvaluehelp                = abap_true
                  valuehelprequest             = ir_client->_event( 'FILTER_VALUE_HELP' ) ).

    ir_client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
