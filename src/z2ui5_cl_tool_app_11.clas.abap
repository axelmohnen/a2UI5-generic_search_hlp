CLASS z2ui5_cl_tool_app_11 DEFINITION PUBLIC.

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

    CLASS-METHODS get_table_by_xlsx
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS get_xlsx_by_table
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE xstring.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_cl_tool_app_11 IMPLEMENTATION.


  METHOD get_xlsx_by_table.


    DATA: lo_excel         TYPE REF TO zcl_excel,
          lo_writer        TYPE REF TO zif_excel_writer,
          lo_worksheet     TYPE REF TO zcl_excel_worksheet.

    DATA: lt_field_catalog  TYPE zexcel_t_fieldcatalog,
          ls_table_settings TYPE zexcel_s_table_settings.


    " Creates active sheet
    CREATE OBJECT lo_excel.

    " Get active sheet
    lo_worksheet = lo_excel->get_active_worksheet( ).
    lo_worksheet->set_title( 'Internal table' ).


    lt_field_catalog = zcl_excel_common=>get_fieldcatalog( ip_table = val ).

    ls_table_settings-table_style  = zcl_excel_table=>builtinstyle_medium5.

    lo_worksheet->bind_table( ip_table          = val
                              is_table_settings = ls_table_settings
                              it_field_catalog  = lt_field_catalog ).

    lo_worksheet->freeze_panes( ip_num_rows = 1 ).

    CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.
    result = lo_writer->write_file( lo_excel ).


  ENDMETHOD.

  METHOD get_table_by_xlsx.

    DATA: lo_excel         TYPE REF TO zcl_excel,
          lo_reader        TYPE REF TO zif_excel_reader,
          lo_worksheet     TYPE REF TO zcl_excel_worksheet.

    CREATE OBJECT lo_reader TYPE zcl_excel_reader_2007.
    lo_excel = lo_reader->load( val ).
    lo_worksheet = lo_excel->get_worksheet_by_index( 1 ).
    lo_worksheet->convert_to_table(
      IMPORTING
        er_data          = result
    ).

  ENDMETHOD.

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

            DATA(lv_xdata) = z2ui5_cl_tool_utility=>decode_x_base64( lv_data ).
            mr_table = get_table_by_xlsx( lv_xdata ).
            client->message_box_display( `XLSX loaded to table` ).

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
    client->timer_set( event_finished = client->_event( `START` ) interval_ms = `0` ).

  ENDMETHOD.


  METHOD ui5_view_init_display.

    client->view_display( z2ui5_cl_xml_view=>factory( client
         )->zcc_file_uploader_js(
         )->stringify( ) ).

  ENDMETHOD.


  METHOD ui5_view_main_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( )->page(
            title          = 'abap2UI5 - XLSX to ABAP internal Table'
            navbuttonpress = client->_event( 'BACK' )
            shownavbutton  = abap_true
        )->header_content(
            )->toolbar_spacer(
            )->link( text = 'Source_Code' target = '_blank' href = view->hlp_get_source_code_url(  )
        )->get_parent( ).

    IF mv_check_download = abap_true.

      FIELD-SYMBOLS <tab> TYPE table.
      ASSIGN mr_table->* TO <tab>.
      mv_check_download = abap_false.
      DATA(lv_xlsx) = get_xlsx_by_table( <tab> ).
      DATA(lv_base) = z2ui5_cl_tool_utility=>encode_x_base64( lv_xlsx ).
      view->zcc_plain_xml( '<html:iframe src="data:text/csv;base64,' && lv_base && '" height="0%" width="0%"/>' ).
    ENDIF.

    IF mr_table IS NOT INITIAL.
      ASSIGN mr_table->* TO <tab>.

      DATA(tab) = page->table(
              items = COND #( WHEN mv_check_edit = abap_true THEN client->_bind_edit( <tab> ) ELSE client->_bind_edit( <tab> ) )
          )->header_toolbar(
              )->overflow_toolbar(
                  )->title( 'XLSX Content'
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

    footer->zcc_file_uploader(
      value       = client->_bind_edit( mv_value )
      path        = client->_bind_edit( mv_path )
      placeholder = 'filepath here...'
      upload      = client->_event( 'UPLOAD' ) ).

    footer->toolbar_spacer(
        )->button(
           text  = 'Download XLSX'
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
