CLASS z2ui5_cl_tool_app_07 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mv_path TYPE string.
    DATA mv_value TYPE string.
    DATA mr_table TYPE REF TO data.
    DATA mv_check_edit TYPE abap_bool.
    DATA mv_check_download TYPE abap_bool.

  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.

    METHODS ui5_on_init.
    METHODS ui5_on_event.

    METHODS ui5_view_main_display.

    METHODS ui5_view_init_display.

  PRIVATE SECTION.
ENDCLASS.



CLASS Z2UI5_CL_TOOL_APP_07 IMPLEMENTATION.


  METHOD ui5_on_event.
    TRY.

        CASE client->get( )-event.

          WHEN 'START' OR 'CHANGE'.
            ui5_view_main_display( ).

          WHEN 'DOWNLOAD'.
            mv_check_download = abap_true.
            ui5_view_main_display( ).

          WHEN 'UPLOAD'.

            SPLIT mv_value AT `;` INTO DATA(lv_dummy) DATA(lv_data).
            SPLIT lv_data AT `,` INTO lv_dummy lv_data.

            DATA(lv_data2) = z2ui5_cl_tool_utility=>decode_x_base64( lv_data ).
            DATA(lv_ready) = z2ui5_cl_tool_utility=>get_string_by_xstring( lv_data2 ).

            mr_table = z2ui5_cl_tool_utility=>get_table_by_csv( lv_ready ).
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

  ENDMETHOD.


  METHOD ui5_view_init_display.

    client->view_display( z2ui5_cl_xml_view=>factory( client
         )->_cc( )->timer( )->control( client->_event( `START` )
         )->_cc( )->ui5_file_uploader( )->load_cc(
         )->stringify( ) ).

  ENDMETHOD.


  METHOD ui5_view_main_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( )->page(
            title          = 'abap2UI5 - CSV to ABAP internal Table'
            navbuttonpress = client->_event( 'BACK' )
            shownavbutton  = abap_true
        )->header_content(
            )->toolbar_spacer(
*            )->link( text = 'Source_Code' target = '_blank' href = view->hlp_get_source_code_url(  )
        )->get_parent( ).

    IF mv_check_download = abap_true.

    FIELD-SYMBOLS <tab> type table.
  assign mr_table->* to <tab>.
      mv_check_download = abap_false.
      DATA(lv_csv) = z2ui5_cl_tool_utility=>get_csv_by_table( <tab> ).
      DATA(lv_xcsv) = z2ui5_cl_tool_utility=>get_xstring_by_string( lv_csv ).
      DATA(LV_base) = z2ui5_cl_tool_utility=>encode_x_base64( lv_xcsv ).
      view->_cc_plain_xml( '<html:iframe src="data:text/csv;base64,' && LV_base && '" height="0%" width="0%"/>' ).
    ENDIF.

    IF mr_table IS NOT INITIAL.
  assign mr_table->* to <tab>.

      DATA(tab) = page->table(
              items = COND #( WHEN mv_check_edit = abap_true THEN client->_bind_edit( <tab> ) ELSE client->_bind_edit( <tab> ) )
          )->header_toolbar(
              )->overflow_toolbar(
                  )->title( 'CSV Content'
                  )->toolbar_spacer(
                  )->switch(
                        change        = client->_event( `CHANGE` )
                        state         = client->_bind_edit( mv_check_edit )
                        customtexton  = 'Edit'
                        customtextoff = 'View'
          )->get_parent( )->get_parent( ).


      DATA(lr_fields) = z2ui5_cl_tool_utility=>get_fieldlist_by_table( <tab> ).
      DATA(lo_cols) = tab->columns( ).
      LOOP AT lr_fields REFERENCE INTO DATA(lr_col).
        lo_cols->column( )->text( lr_col->* ).
      ENDLOOP.
      DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
      LOOP AT lr_fields REFERENCE INTO lr_col.
        IF mv_check_edit = abap_true.
          lo_cells->input( `{` && lr_col->* && `}` ).
        ELSE.
          lo_cells->text( `{` && lr_col->* && `}` ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    DATA(footer) = page->footer( )->overflow_toolbar( ).

    footer->_cc( )->ui5_file_uploader( )->control(
      value       = client->_bind_edit( mv_value )
      path        = client->_bind_edit( mv_path )
      placeholder = 'filepath here...'
      upload      = client->_event( 'UPLOAD' ) ).

    footer->toolbar_spacer(
        )->button(
           text  = 'Download CSV'
           press = client->_event( 'DOWNLOAD' )
           type  = 'Emphasized'
           icon  = 'sap-icon://download' ).

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
