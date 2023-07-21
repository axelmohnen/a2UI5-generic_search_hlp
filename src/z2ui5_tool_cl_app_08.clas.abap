CLASS z2ui5_tool_cl_app_08 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA ms_view TYPE z2ui5_tool_cl_view=>ty_s_file.

    DATA mv_type TYPE string.
    DATA mv_path TYPE string.
    DATA mv_editor TYPE string.
    DATA mv_check_editable TYPE abap_bool.
    DATA check_initialized TYPE abap_bool.

    DATA mv_name TYPE string.

    METHODS db_load.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_tool_cl_app_08 IMPLEMENTATION.

  METHOD db_load.

    ms_view = z2ui5_tool_cl_view=>db_read( ms_view-name ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      ms_view-name = z2ui5_cl_xml_view=>factory( client )->hlp_get_url_param( `view`).

      IF ms_view-name IS NOT INITIAL.
        db_load( ).
      ENDIF.
    ENDIF.

    CASE client->get( )-event.

      WHEN 'DB_SAVE'.
        z2ui5_tool_cl_view=>db_create( ms_view ).
        COMMIT WORK AND WAIT.
        client->message_box_display( text = 'File changed sucessfully!' type = 'success' ).

      WHEN 'DB_LOAD'.
        db_load( ).

      WHEN 'CLEAR'.
        ms_view-data = ``.

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
        RETURN.
    ENDCASE.


    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( )->page(
    title = 'abap2UI5 - File Editor'
    navbuttonpress = client->_event( 'BACK' )
    shownavbutton = abap_true
            )->header_content(
            )->link( text = 'Source_Code' target = '_blank' href = view->hlp_get_source_code_url(  )
        )->get_parent( ).

    DATA(grid) = page->grid( 'L7 M12 S12' )->content( 'layout' ).

    grid->simple_form( title = 'File' editable = abap_true )->content( 'form'
         )->label( 'Name'
          )->input( client->_bind_edit( ms_view-name )
                    )->button(
            text  = 'Load'
            press = client->_event( 'DB_LOAD' )
                   )->label( 'Description'
          )->input( client->_bind_edit( ms_view-descr )
        ).

    grid = page->grid( 'L12 M12 S12' )->content( 'layout' ).

    grid->simple_form( 'Editor' )->content( 'form'
            )->scroll_container( '75%'
                )->code_editor(
                    type  = 'xml'
                    value = client->_bind_edit( ms_view-data ) ).

    page->footer( )->overflow_toolbar(
        )->toolbar_spacer(
        )->button(
            text  = 'Save'
            press = client->_event( 'DB_SAVE' )
            type  = 'Emphasized'
            icon = 'sap-icon://upload-to-cloud' ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
