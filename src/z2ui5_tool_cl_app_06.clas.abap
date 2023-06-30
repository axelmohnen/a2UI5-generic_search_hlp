CLASS z2ui5_tool_cl_app_06 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mv_path TYPE string.
    DATA mv_value TYPE string.

    TYPES:
      BEGIN OF ty_file,
        selkz  TYPE abap_bool,
        name   TYPE string,
        format TYPE string,
        size   TYPE string,
        descr  TYPE string,
        data   TYPE string,
      END OF ty_file.

    DATA mt_file      TYPE STANDARD TABLE OF ty_file WITH EMPTY KEY.
    DATA ms_file_edit TYPE ty_file.
    DATA ms_file_prev TYPE ty_file.

        DATA mt_table TYPE REF TO data.
    DATA mt_cols TYPE string_table.
    DATA mv_name TYPE string.

  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.

    METHODS ui5_on_init.
    METHODS ui5_on_event.

    METHODS ui5_render_view_main
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS ui5_render_view_init
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS ui5_render_popup_descr
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS ui5_render_popup_data
      RETURNING
        VALUE(r_result) TYPE string.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_tool_cl_app_06 IMPLEMENTATION.


  METHOD ui5_on_event.
    TRY.

        CASE client->get( )-event.

          WHEN 'START'.
            ui5_render_view_main( ).

          WHEN 'DISPLAY'.
            ms_file_prev = mt_file[ selkz = abap_true ].

          WHEN 'UPLOAD'.
            INSERT VALUE #( name = mv_path data = mv_value size = strlen( mv_value ) format = mv_value+5(5) )   INTO TABLE mt_file.
            CLEAR ms_file_prev.
            CLEAR ms_file_edit.
            CLEAR mv_value.
            CLEAR mv_path.

          WHEN 'TEXTAREA_DATA_CONFIRM'.
            client->popup_close( ).

          WHEN 'TEXTAREA_DESCR_CONFIRM'.
            mt_file[ selkz = abap_true ] = ms_file_edit.
            CLEAR ms_file_edit.

          WHEN 'TEXTAREA_DATA_CONFIRM'.
            CLEAR ms_file_edit.

          WHEN 'POPUP_DESCR'.
            ms_file_edit = mt_file[ selkz = abap_true ].
            ui5_render_popup_descr( ).

          WHEN 'POPUP_DATA'.
            ms_file_edit = mt_file[ selkz = abap_true ].
            ui5_render_popup_data( ).

          WHEN 'POPUP_NORMAL'.
            ms_file_edit = mt_file[ selkz = abap_true ].
            DATA xstring  TYPE xstring.
*          .
*            xstring = conv #( ms_file_edit-data ).
*            data(rv_csv_data) = cl_abap_conv_codepage=>create_in( )->convert( ms_file_edit-data ).
*            data(rv_csv_data) = cl_abap_conv_codepage=>create_out( )->convert( segment( val = ms_file_edit-data sep = `,` index = 2 ) ).

            DATA(lv_base64) = segment( val = ms_file_edit-data sep = `,` index = 2 ).

            DATA(lv_data) = z2ui5_tool_cl_utility=>decode_x_base64( lv_base64 ).
            DATA(lv_ready) = z2ui5_tool_cl_utility=>get_string_by_xstring( lv_data ).
            ms_file_edit-data = lv_ready.

*            DATA(lv_readyx) = lcl_mime_api=>get_xstring_by_string( lv_ready ).
*            DATA(lv_data2) = lcl_mime_api=>get_base64_encoded( lv_readyx ).


*            IF lv_base64 <> lv_data2.
*              ASSERT 1 = 0.
*            ENDIF.

            ui5_render_popup_data( ).

          WHEN 'BACK'.
            client->nav_app_leave( client->get_app( client->get( )-id_prev_app_stack ) ).

        ENDCASE.

      CATCH cx_root INTO DATA(x).
        client->message_box_display( text = x->get_text( ) type = `error` ).
    ENDTRY.

  ENDMETHOD.


  METHOD ui5_on_init.

    ui5_render_view_init( ).
    client->timer_set( event_finished = `START` interval_ms = `0` ).

  ENDMETHOD.


  METHOD ui5_render_popup_data.

    DATA(lo_popup) = z2ui5_cl_xml_view=>factory_popup( client ).
    lo_popup->dialog(
            stretch = abap_true
            title = 'Data:'
        )->content(
            )->text_area(
                height = '99%'
                width = '99%'
                enabled = abap_false
                value = client->_bind( ms_file_edit-data )
        )->get_parent(
        )->footer( )->overflow_toolbar(
            )->toolbar_spacer(
            )->button(
                text  = 'close'
                press = client->_event( 'TEXTAREA_DATA_CONFIRM' )
                type  = 'Emphasized' ).

    client->popup_display( lo_popup->stringify( ) ).

  ENDMETHOD.


  METHOD ui5_render_popup_descr.

    DATA(lo_popup) = z2ui5_cl_xml_view=>factory_popup( client
              )->dialog(
                      title = 'Edit Description'
                      icon = 'sap-icon://edit'
                  )->content(
                      )->text_area(
                          height = '99%'
                          width = '99%'
                          value = client->_bind_edit( ms_file_edit-descr )
                  )->get_parent(
                  )->footer( )->overflow_toolbar(
                      )->toolbar_spacer(
                      )->button(
                          text  = 'Cancel'
                          press = client->_event( 'TEXTAREA_CANCEL' )
                      )->button(
                          text  = 'Confirm'
                          press = client->_event( 'TEXTAREA_DESCR_CONFIRM' )
                          type  = 'Emphasized' ).

    client->popup_display( lo_popup->stringify( ) ).

  ENDMETHOD.


  METHOD ui5_render_view_init.

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
*       )->header_content( ns = 'm'
*           )->toolbar_spacer( ns = 'm'
*           )->link( ns = 'm' text = 'Demo'   target = '_blank'     href = 'https://twitter.com/abap2UI5/status/1638487600930357248'
*           )->link( ns = 'm'  target = '_blank' text = 'Source_Code' href = lo_view->hlp_get_source_code_url(  )
       )->get_parent( ).

*    page->text( ns = 'm' text = 'Custom Control for File Upload is now loaded...'
*        )->button( ns = 'm' text = 'continue' press = client->_event( 'START' )
        page->zz_plain( `  <script>  ` && z2ui5_cl_xml_view=>cc_file_uploader_get_js( ) && ` </script>`
    ).

    client->view_display( lo_view->stringify( ) ).

  ENDMETHOD.


  METHOD ui5_render_view_main.

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( )->page(
            title          = 'abap2UI5 - File Upload/Download'
            navbuttonpress = client->_event( 'BACK' )
            shownavbutton  = abap_true
        )->header_content(
            )->toolbar_spacer(
            )->link( text = 'Demo'        href = 'https://twitter.com/abap2UI5/status/1638487600930357248'
            )->link( text = 'Source_Code' href = view->hlp_get_source_code_url(  )
        )->get_parent( ).

    page->cc_file_uploader(
        value       = client->_bind_edit( mv_value )
        path        = client->_bind_edit( mv_path )
        placeholder = 'filepath here...'
        upload      = client->_event( 'UPLOAD' ) ).





*       FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
*        CREATE DATA mt_table TYPE STANDARD TABLE OF (mv_name) with DEFAULT KEY.
*        ASSIGN mt_table->* TO <tab>.
*        mt_cols = lcl_db=>get_fieldlist_by_table( <tab> ).
*
*  IF mt_table IS BOUND.
*
*      FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
*      ASSIGN mt_table->* TO <tab>.
*      DATA(tab) = lo_view->get_parent( )->get_parent( )->simple_form( editable = abap_true
*                )->content( 'form' )->table(
*                  items = client->_bind( val = <tab> )
*              ).
*
*      DATA(lo_columns) = tab->columns( ).
*      mt_cols = lcl_db=>get_fieldlist_by_table( <tab> ).
*
*      LOOP AT mt_cols INTO DATA(lv_field) FROM 2.
*        lo_columns->column( )->text( lv_field ).
*      ENDLOOP.
*
*      DATA(lo_cells) = tab->items( )->column_list_item( selected = '{SELKZ}' )->cells( ).
*      LOOP AT mt_cols INTO lv_field FROM 2.
*        lo_cells->input( `{` && lv_field && `}` ).
*      ENDLOOP.
*
*    ENDIF.





    IF ms_file_prev-data IS NOT INITIAL.
      page->zz_plain( '<html:iframe src="' && ms_file_prev-data && '" height="75%" width="98%"/>' ).
      CLEAR mv_value.
    ENDIF.

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      ui5_on_init( ).
      RETURN.
    ENDIF.

    ui5_on_event( ).

  ENDMETHOD.
ENDCLASS.
