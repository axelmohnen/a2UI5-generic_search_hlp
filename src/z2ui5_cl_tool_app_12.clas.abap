class Z2UI5_CL_TOOL_APP_12 definition
  public
  create public .

public section.

  interfaces IF_SERIALIZABLE_OBJECT .
  interfaces Z2UI5_IF_APP .

  data:
    BEGIN OF ms_screen,
        langu TYPE spras,
      END OF ms_screen .
  data MV_CHECK_POPUP type ABAP_BOOL .
  data MV_CHECK_INITIALIZED type ABAP_BOOL .
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



CLASS Z2UI5_CL_TOOL_APP_12 IMPLEMENTATION.


  METHOD Z2UI5_IF_APP~MAIN.

    IF mv_check_initialized = abap_false.
      mv_check_initialized = abap_true.
      z2ui5_on_render( ir_client = client ).
    ENDIF.

    IF mv_check_popup = abap_true.
      mv_check_popup = abap_false.
      DATA(app) = CAST z2ui5_cl_tool_app_shlp_gen( client->get_app( client->get( )-s_draft-id_prev_app )  ).
      client->message_toast_display( app->mv_shlp_result ).
      me->ms_screen-langu = app->mv_shlp_result.
      z2ui5_on_render( ir_client = client ).
    ENDIF.

    z2ui5_on_event( ir_client = client ).


  ENDMETHOD.


  METHOD Z2UI5_ON_EVENT.

    CASE ir_client->get( )-event.
      WHEN `FILTER_VALUE_HELP`.
        mv_check_popup = abap_true.
        ir_client->nav_app_call( z2ui5_cl_tool_app_shlp_gen=>factory(
          iv_popup_title = 'SHLP DEMO - using deep search'
          iv_shlp_id = 'H_T002'
          iv_use_deep_shlp = abap_true ) ).

      WHEN 'BACK'.
        ir_client->nav_app_leave( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app_stack ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD Z2UI5_ON_INIT.

  ENDMETHOD.


  METHOD Z2UI5_ON_RENDER.
    DATA(view) = z2ui5_cl_xml_view=>factory( ir_client ).

    DATA(page) = view->page( id = `page_main`
             title          = 'abap2UI5 - Run generic DDIC searchelp'
             navbuttonpress = ir_client->_event( 'BACK' )
             shownavbutton  = abap_true ).


    DATA(grid) = page->grid( 'L7 M12 S12' )->content( 'layout'
        )->simple_form( 'run DDIC searchhelp in new app' )->content( 'form'
            )->label( `Language`
            )->input(  value = ir_client->_bind_edit( ms_screen-langu )
                  showvaluehelp                = abap_true
                  valuehelprequest             = ir_client->_event( 'FILTER_VALUE_HELP' ) ).

    ir_client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
