CLASS z2ui5_cl_tool_app_09 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object .
    INTERFACES z2ui5_if_app .

    DATA:
      BEGIN OF ms_screen,
        partner TYPE string,
      END OF ms_screen .
    DATA mv_check_popup TYPE abap_bool .
    DATA mv_check_initialized TYPE abap_bool .
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



CLASS Z2UI5_CL_TOOL_APP_09 IMPLEMENTATION.


  METHOD z2ui5_if_app~main.

    IF mv_check_initialized = abap_false.
      mv_check_initialized = abap_true.
      z2ui5_on_render( ir_client = client ).
    ENDIF.

    IF mv_check_popup = abap_true.
      mv_check_popup = abap_false.
      DATA(app) = CAST z2ui5_cl_tool_app_shlp_gen( client->get_app( client->get( )-s_draft-id_prev_app )  ).
      client->message_toast_display( app->mv_shlp_result ).
      me->ms_screen-partner = app->mv_shlp_result.
      "client->view_model_update( ).
      z2ui5_on_render( ir_client = client ).
    ENDIF.

    z2ui5_on_event( ir_client = client ).


  ENDMETHOD.


  METHOD z2ui5_on_event.

    CASE ir_client->get( )-event.
      WHEN `FILTER_VALUE_HELP`.
        mv_check_popup = abap_true.
        ir_client->nav_app_call( z2ui5_cl_tool_app_shlp_gen=>factory(
          iv_popup_title = 'THIS is the DDIC SHLP title'
          iv_shlp_id = 'F4SHLP_ACMDTUI_DDLSOURCE' ) ).

      WHEN 'BACK'.
        ir_client->nav_app_leave( ir_client->get_app( ir_client->get( )-s_draft-id_prev_app_stack ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD z2ui5_on_init.

  ENDMETHOD.


  METHOD z2ui5_on_render.
    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    DATA(page) = view->page( id = `page_main`
             title          = 'abap2UI5 - Run generic DDIC searchelp'
             navbuttonpress = ir_client->_event( 'BACK' )
             shownavbutton  = abap_true ).


    DATA(grid) = page->grid( 'L7 M12 S12' )->content( 'layout'
        )->simple_form( 'run DDIC searchhelp in new app' )->content( 'form'
            )->label( `Partner`
            )->input(  value = ir_client->_bind_edit( ms_screen-partner )
                  showvaluehelp                = abap_true
                  valuehelprequest             = ir_client->_event( 'FILTER_VALUE_HELP' ) ).

    ir_client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
