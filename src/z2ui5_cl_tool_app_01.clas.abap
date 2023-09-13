CLASS z2ui5_cl_tool_app_01 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA ms_file TYPE z2ui5_cl_tool_file_api=>ty_s_file.

    DATA mv_type TYPE string.
    DATA mv_path TYPE string.
    DATA mv_editor TYPE string.
    DATA mv_check_editable TYPE abap_bool.
    DATA check_initialized TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS Z2UI5_CL_TOOL_APP_01 IMPLEMENTATION.


  METHOD z2ui5_if_app~main.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      ms_file = z2ui5_cl_tool_file_api=>read( ms_file-id ).

      DATA(lv_data3) =  ms_file-data.
      DATA(lv_data2) = z2ui5_cl_tool_utility=>decode_x_base64( lv_data3 ).
      ms_file-data = z2ui5_cl_tool_utility=>get_string_by_xstring( lv_data2 ).

      mv_path = '../../demo/text'.
      mv_type = 'plain_text'.
    ENDIF.

    CASE client->get( )-event.

      WHEN 'DB_SAVE'.

        DATA(ls_db) = ms_file.
        DATA(lv_readyx) = z2ui5_cl_tool_utility=>get_xstring_by_string( ls_db-data ).
        ls_db-data = z2ui5_cl_tool_utility=>encode_x_base64( lv_readyx ).

        z2ui5_cl_tool_file_api=>update_data( ls_db ).
        commit work and wait.
        client->message_box_display( text = 'File changed sucessfully!' type = 'success' ).

      WHEN 'EDIT'.
        mv_check_editable = xsdbool( mv_check_editable = abap_false ).
      WHEN 'CLEAR'.
        mv_editor = ``.
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
*                )->link( text = 'Demo'        target = '_blank' href = 'https://twitter.com/abap2UI5/status/1631562906570575875'
                )->link( text = 'Source_Code' target = '_blank' href = view->hlp_get_source_code_url(  )
        )->get_parent( ).

    DATA(grid) = page->grid( 'L7 M12 S12' )->content( 'layout' ).

    grid->simple_form( title = 'File' editable = abap_true )->content( 'form'
*         )->label( 'ID / Size'
*         )->input( value = client->_bind( ms_file-id ) enabled = abap_false
*         )->input( value = client->_bind( ms_file-file_size ) enabled = abap_false
         )->label( 'Format'
*         )->input( client->_bind( ms_file-name )
          )->input(
                value           = client->_bind_edit( mv_type )
                suggestionitems = client->_bind( z2ui5_cl_tool_file_api=>get_editor_type( ) ) )->get(
            )->suggestion_items(
                )->list_item( text = '{N}' additionaltext = '{V}'
                 )->get_parent( )->get_parent(
*         )->label( 'Description'
*         )->input( client->_bind( ms_file-descr )
        ).

    grid = page->grid( 'L12 M12 S12' )->content( 'layout' ).

    grid->simple_form( 'Editor' )->content( 'form'
            )->scroll_container( '75%'
                )->code_editor(
                    type  = mv_type
*                    editable = mv_check_editable
                    value = client->_bind_edit( ms_file-data ) ).

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
