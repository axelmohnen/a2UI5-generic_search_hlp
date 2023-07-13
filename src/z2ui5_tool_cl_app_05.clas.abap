CLASS z2ui5_tool_cl_app_05 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mv_path TYPE string.
    DATA mv_value TYPE string.

    TYPES:
      BEGIN OF ty_s_file_out,
        selkz       TYPE abap_bool,
        id          TYPE string,
        name        TYPE string,
        file_format TYPE string,
        file_size   TYPE string,
        descr       TYPE string,
      END OF ty_s_file_out.
    DATA mt_out TYPE STANDARD TABLE OF ty_s_file_out WITH EMPTY KEY.

    DATA ms_file       TYPE ty_s_file_out.
    DATA ms_file_popup TYPE z2ui5_tool_cl_file_api=>ty_s_file.

    DATA mt_file      TYPE STANDARD TABLE OF ty_s_file_out WITH EMPTY KEY.
    DATA ms_file_edit TYPE z2ui5_tool_cl_file_api=>ty_s_file.
    DATA ms_file_prev TYPE ty_s_file_out.
    DATA mv_check_download TYPE abap_Bool.

  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.

    METHODS ui5_on_init.
    METHODS ui5_on_event.

    METHODS ui5_delete.

    METHODS ui5_view_main_display
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS ui5_view_init_display
      RETURNING
        VALUE(r_result) TYPE string.

    METHODS ui5_popup_metadata_display..

    METHODS ui5_popup_data_display
      IMPORTING
        data            TYPE string.

    METHODS ui5_load.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_tool_cl_app_05 IMPLEMENTATION.


  METHOD ui5_on_event.
    TRY.

        ms_file = VALUE #( mt_out[ selkz = abap_true ] DEFAULT VALUE #( ) ).

        CASE client->get( )-event.

          WHEN 'READ'.
            ui5_load( ).
            client->view_model_update( ).

          WHEN 'DELETE'.
            ui5_delete( ).

          WHEN 'START'.
            ui5_view_main_display( ).

          WHEN 'DISPLAY'.
            ms_file_prev = mt_file[ selkz = abap_true ].

          WHEN 'UPLOAD'.

            SPLIT mv_value AT `;` INTO DATA(lv_dummy) DATA(lv_data).
            SPLIT lv_data AT `,` INTO DATA(lv_format) lv_data.
            DATA(lv_id_new) = z2ui5_tool_cl_file_api=>create( VALUE #( data = lv_data name = mv_path file_format = lv_format ) ).
            COMMIT WORK.

            ui5_load( ).
            mt_out[ id = lv_id_new ]-selkz = abap_true.

            client->message_box_display( `File saved succesfully` ).

            CLEAR ms_file_prev.
            CLEAR ms_file_edit.
            CLEAR mv_value.
            CLEAR mv_path.

          WHEN 'BUTTON_METADATA_CONFIRM'.
            z2ui5_tool_cl_file_api=>update_metadata( ms_file_popup ).
            COMMIT WORK AND WAIT.
            client->message_box_display( `File metadata changed succesfully` ).
            DATA(lv_selkz_id) = VALUE #( mt_out[ selkz = abap_true ]-id OPTIONAL ).
            ui5_load( ).
            IF lv_selkz_id IS NOT INITIAL.
              mt_out[ id = lv_selkz_id ]-selkz = abap_true.
            ENDIF.
            client->popup_destroy( ).

          WHEN 'EDIT'.
            ms_file_popup = z2ui5_tool_cl_file_api=>read( id = ms_file-id ).
            CLEAR ms_file_popup-data.
            ui5_popup_metadata_display( ).


          WHEN 'EDITOR'.
            DATA(lo_Editor) = NEW z2ui5_tool_cl_app_01( ).
            lo_editor->ms_file-id = mt_out[ selkz = abap_true ]-id.
            client->nav_app_call( lo_editor ).

          WHEN 'POPUP_BASE64'.
            DATA(lv_data3) = z2ui5_tool_cl_file_api=>read( id = ms_file-id )-data.
            ui5_popup_data_display( lv_data3 ).

          when 'DOWNLOAD'.
            mv_check_download = ABAP_TRUE.
            ui5_view_main_display( ).

          WHEN 'POPUP_DATA'.
            lv_data3 = z2ui5_tool_cl_file_api=>read( id = ms_file-id )-data.
            DATA(lv_data2) = z2ui5_tool_cl_utility=>decode_x_base64( lv_data3 ).
            lv_data3 = z2ui5_tool_cl_utility=>get_string_by_xstring( lv_data2 ).
            ui5_popup_data_display( lv_data3 ).

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


  METHOD ui5_popup_data_display.

    DATA(lo_popup) = z2ui5_cl_xml_view=>factory_popup( client ).
    lo_popup->dialog(
            stretch = abap_true
            title = 'Data:'
        )->content(
            )->text_area(
                height = '99%'
                width = '99%'
                enabled = abap_false
                value = data
        )->get_parent(
        )->footer( )->overflow_toolbar(
            )->toolbar_spacer(
            )->button(
                text  = 'close'
                press = client->_event_client( client->cs_event-popup_close )
                type  = 'Emphasized' ).

    client->popup_display( lo_popup->stringify( ) ).

  ENDMETHOD.


  METHOD ui5_popup_metadata_display.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( client )->dialog(
       contentheight = '500px'
       contentwidth  = '500px'
       title = 'Edit Metadata'
       )->content(
           )->simple_form(
               )->label( 'Name'
               )->input( client->_bind_edit( ms_file_popup-name )
               )->label( 'Format'
               )->input( client->_bind_edit( ms_file_popup-file_format )
               )->label( 'Description'
               )->input( client->_bind_edit( ms_file_popup-descr )
       )->get_parent( )->get_parent(
       )->footer( )->overflow_toolbar(
           )->toolbar_spacer(
           )->button(
               text  = 'Cancel'
               press = client->_event_client( client->cs_event-popup_close )
           )->button(
               text  = 'Confirm'
               press = client->_event( 'BUTTON_METADATA_CONFIRM' )
               type  = 'Emphasized' ).

    client->popup_display( popup->stringify( ) ).

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


  METHOD ui5_view_main_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).
    DATA(page) = view->shell( )->page(
            title          = 'abap2UI5 - Upload & Download Files'
            navbuttonpress = client->_event( 'BACK' )
            shownavbutton  = abap_true
            showHeader     = xsdbool( client->get( )-check_launchpad_active = abap_false )
        )->header_content(
            )->toolbar_spacer(
            )->link( text = 'Source_Code' href = view->hlp_get_source_code_url(  )
        )->get_parent( ).

    DATA(tab) = page->table(
            headertext = 'Table'
            mode = 'SingleSelectLeft'
            items = client->_bind_edit( mt_out )
        )->header_toolbar(
            )->overflow_toolbar(
                )->title( 'Files'
                      )->toolbar_spacer(
                      )->toolbar_spacer(
                      )->toolbar_spacer(
                                )->button(
                    text = 'Show Base64'
                    press = client->_event( 'POPUP_BASE64' )
                )->button(
                    text = 'Show text'
                    press = client->_event( 'POPUP_DATA' )
                )->toolbar_spacer(
                )->button(
                    text = 'Delete File'
                    press = client->_event( 'DELETE' )
                    icon = `sap-icon://delete`
                )->button(
                    text = 'Change Metadata'
                    press = client->_event( 'EDIT' )
                    icon = 'sap-icon://edit'
                 )->button(
                    text = 'Editor'
                    press = client->_event( 'EDITOR' )
                    type = `Emphasized`
                    icon = 'sap-icon://write-new-document'

        )->get_parent( )->get_parent( ).

    tab->columns(
        )->column(
            )->text( 'Name' )->get_parent(
        )->column(
            )->text( 'Format' )->get_parent(
        )->column(
            )->text( 'Size' )->get_parent(
        )->column(
            )->text( 'Description' ).

    tab->items( )->column_list_item( selected = '{SELKZ}' )->cells(
       )->text( '{NAME}'
       )->text( '{FILE_FORMAT}'
       )->text( '{FILE_SIZE}'
       )->text( '{DESCR}' ).

    DATA(footer) = page->footer( )->overflow_toolbar( ).

    footer->cc_file_uploader(
      value       = client->_bind_edit( mv_value )
      path        = client->_bind_edit( mv_path )
      placeholder = 'filepath here...'
      upload      = client->_event( 'UPLOAD' ) ).

*    footer->button( text = 'Download' press = client->_event( 'DOWNLOAD' ) ).
    footer->toolbar_spacer(
*        )->button(
*            text  = 'Edit'
*            press = client->_event( 'EDIT' )
*            icon = 'sap-icon://edit'
            )->button(
                text  = 'Load List'
                press = client->_event( 'READ' )
                type  = 'Emphasized'
                icon = 'sap-icon://refresh' ).

*    IF mv_check_download = abap_true.
*      mv_check_download = abap_false.
*      footer->zz_plain( '<html:iframe src="data:text/csv;base64,' && z2ui5_tool_cl_file_api=>read( ms_file-id )-data && '" height="75%" width="98%"/>' ).
*    ENDIF.

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

  METHOD ui5_load.

    mt_out = CORRESPONDING #( z2ui5_tool_cl_file_api=>read_all( ) ).

  ENDMETHOD.


  METHOD ui5_delete.

    z2ui5_tool_cl_file_api=>delete( ms_file-id ).
    COMMIT WORK AND WAIT.
    ui5_load( ).
    client->message_box_display( type = `success` text = `File deleted successfully` ).

  ENDMETHOD.

ENDCLASS.
