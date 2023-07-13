CLASS z2ui5_tool_cl_app_07 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mv_path TYPE string.
    DATA mv_value TYPE string.
    DATA mr_table TYPE REF TO data.
    data mv_check_edit type abap_bool.

  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.

    METHODS ui5_on_init.
    METHODS ui5_on_event.

    METHODS ui5_view_main_display.

    METHODS ui5_view_init_display.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_tool_cl_app_07 IMPLEMENTATION.


  METHOD ui5_on_event.
    TRY.

        CASE client->get( )-event.

          WHEN 'START'.
            ui5_view_main_display( ).

          WHEN 'UPLOAD'.

            SPLIT mv_value AT `;` INTO DATA(lv_dummy) DATA(lv_data).
            SPLIT lv_data AT `,` INTO lv_dummy lv_data.

            DATA(lv_data2) = z2ui5_tool_cl_utility=>decode_x_base64( lv_data ).
            DATA(lv_ready) = z2ui5_tool_cl_utility=>get_string_by_xstring( lv_data2 ).

            mr_table = z2ui5_tool_cl_utility=>get_table_by_csv( lv_ready ).
            client->message_box_display( `CSV loaded to table` ).

            ui5_view_main_display( ).

            CLEAR mv_value.
            CLEAR mv_path.

          WHEN 'BACK'.
            client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

        ENDCASE.

      CATCH cx_root INTO DATA(x).
        client->message_box_display( text = x->get_text( ) type = `error` ).
    ENDTRY.

  ENDMETHOD.


  METHOD ui5_on_init.

    ui5_view_init_display( ).
    client->timer_set( event_finished = `START` interval_ms = `0` ).

  ENDMETHOD.

  METHOD ui5_view_init_display.

    DATA(lo_view) = z2ui5_cl_xml_view=>factory( client = client t_ns = VALUE #(
         ( n = `xmlns:mvc` v = `sap.ui.core.mvc` )
         ( n = `xmlns:m` v = `sap.m` )
         ( n = `xmlns:z2ui5` v = `z2ui5` )
         ( n = `xmlns:core` v = `sap.ui.core` )
         ( n = `xmlns` v = `http://www.w3.org/1999/xhtml` )
     ) ).

    DATA(page) = lo_view->_generic( name = 'Shell' ns = 'm' )->page(
           ns             = 'm'
           title          = 'abap2UI5 - File Upload/Download'
           navbuttonpress = client->_event( 'BACK' )
           shownavbutton  = abap_true
       )->get_parent( ).

    page->zz_plain( `  <script>  ` && z2ui5_cl_xml_view=>cc_file_uploader_get_js( ) && ` </script>` ).
    client->view_display( lo_view->stringify( ) ).

  ENDMETHOD.


  METHOD ui5_view_main_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( )->page(
            title          = 'abap2UI5 - CSV to ABAP internal Table'
            navbuttonpress = client->_event( 'BACK' )
            shownavbutton  = abap_true
        )->header_content(
            )->toolbar_spacer(
            )->link( text = 'Source_Code' href = view->hlp_get_source_code_url(  )
        )->get_parent( ).


    IF mr_table IS NOT INITIAL.

      DATA(tab) = page->table(
              headertext = 'Table'
              items = client->_bind_edit( mr_table->* )
          )->header_toolbar(
              )->overflow_toolbar(
                  )->title( 'Table Content'
                  )->toolbar_spacer(
                  )->switch(
                    state         = client->_bind_edit( mv_check_edit )

                        customtexton  = 'Edit'
                        customtextoff = 'View'
*                        type = 'AcceptReject'
          )->get_parent( )->get_parent( ).


      DATA(lr_fields) = z2ui5_tool_cl_utility=>get_fieldlist_by_table( mr_table->* ).
      DATA(lo_cols) = tab->columns( ).
      LOOP AT lr_fields REFERENCE INTO DATA(lr_col).
        lo_cols->column( )->text( lr_col->* ).
      ENDLOOP.
      DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
      LOOP AT lr_fields REFERENCE INTO lr_col.
        lo_cells->text( `{` && lr_col->* && `}` ).
      ENDLOOP.
    ENDIF.

    DATA(footer) = page->footer( )->overflow_toolbar( ).
    footer->cc_file_uploader(
      value       = client->_bind_edit( mv_value )
      path        = client->_bind_edit( mv_path )
      placeholder = 'filepath here...'
      upload      = client->_event( 'UPLOAD' ) ).
    footer->toolbar_spacer(
            )->button(
                text  = 'Download CSV'
                press = client->_event( 'READ' )
                type  = 'Emphasized'
                icon = 'sap-icon://download' ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      ui5_on_init( ).
      RETURN.
    ENDIF.

    IF client->get( )-check_on_navigated = abap_true.
      ui5_view_main_display( ).
    ENDIF.

    ui5_on_event( ).

  ENDMETHOD.


ENDCLASS.
