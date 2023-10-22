CLASS z2ui5_cl_tool_app_00 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS z2ui5_cl_tool_app_00 IMPLEMENTATION.


  METHOD z2ui5_if_app~main.

    IF client->get( )-check_on_navigated = abap_true.

      DATA(view) = z2ui5_cl_xml_view=>factory( client ).

      DATA(page) = view->shell( )->page(
              title          = 'abap2UI5 - Tools'
              navbuttonpress = client->_event( 'BACK' )
              shownavbutton  = abap_true ).

      page->generic_tile(
         class     = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
         header    = `Upload`
         press     = client->_event( `z2ui5_cl_tool_app_05` )
      )->get( )->tile_content(
        footer = `Files in the System`
        )->numeric_content( value = `1` icon = 'sap-icon://documents' withmargin = abap_false ).

      page->generic_tile(
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `File Editor`
         press     = client->_event( `z2ui5_cl_tool_app_01` )
      )->get( )->tile_content(
         )->image_content( src = 'sap-icon://edit' ).

      page->generic_tile(
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `Database Viewer`
         press     = client->_event( `z2ui5_cl_tool_app_02` )
      )->get( )->tile_content(
         )->image_content( src = 'sap-icon://detail-view' ).

      page->generic_tile(
         class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
         header    = `CSV to Itab Editor`
          press     = client->_event( `z2ui5_cl_tool_app_07` )
       )->get( )->tile_content(
          )->image_content( src = 'sap-icon://table-view' ).

      page->generic_tile(
         class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
         header    = `List Report Viewer`
          press     = client->_event( `z2ui5_cl_tool_app_03` )
        )->get( )->tile_content(
          )->image_content( src = 'sap-icon://my-view' ).

      page->generic_tile(
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `Generic Search Help Demo I`
        press     = client->_event( `z2ui5_cl_tool_app_10` )
        )->get( )->tile_content( ).

      page->generic_tile(
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `Generic Search Help Demo II`
        press     = client->_event( `z2ui5_cl_tool_app_09` )
        )->get( )->tile_content( ).

      client->view_display( view->stringify( ) ).

    ENDIF.

    IF client->get( )-event IS INITIAL.
      RETURN.
    ENDIF.

    CASE client->get( )-event.

      WHEN `BACK`.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN OTHERS.

        DATA li_app TYPE REF TO z2ui5_if_app.
        DATA(lv_classname) = to_upper( client->get( )-event ).
        CREATE OBJECT li_app TYPE (lv_classname).
        client->nav_app_call( li_app ).

    ENDCASE.


  ENDMETHOD.
ENDCLASS.
