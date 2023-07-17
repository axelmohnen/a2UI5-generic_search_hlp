CLASS z2ui5_tool_cl_app_00 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.


    DATA mv_value TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS z2ui5_tool_cl_app_00 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->get( )-check_on_navigated = abap_true.

      DATA(view) = z2ui5_cl_xml_view=>factory( client ).

      DATA(page) = view->shell( )->page(
              title          = 'abap2UI5 - Tools'
              navbuttonpress = client->_event( 'BACK' )
              shownavbutton  = abap_true ).

*      DATA(box) = page->vbox( ).
*      DATA(hbox) = box->hbox( ).
*      hbox->title( text = `Files` ).

*    page->title( text = `Files` ).
*      data(form) =  page->simple_form( title = 'Files' editable = abap_true )->content( 'form' ).
*
*    DATA(grid) = page->grid( 'L3 M6 S12'
*        )->content( 'layout' ).

*      hbox = box->hbox( ).
*    <GenericTile class="sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout" header="Cumulative Totals" subheader="Expenses" press="onPress" >
*        <TileContent unit="Unit" footer="Footer Text">
*            <NumericContent value="1762" icon="sap-icon://line-charts" withMargin="false" />
*        </TileContent>
*    </GenericTile>

    select from z2ui5_tool_t_001
    fields
        count( id ) as number
      into table @data(lt_tab).

      data(lv_count) = value #( lt_tab[ 1 ]-number optional ).

      page->generictile(
*      EXPORTING
         class     = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
         header    = `Upload`
         press     = client->_event( `z2ui5_tool_cl_app_05` )
*        frametype =
*         subheader = ` from your Client`
*      RECEIVING
*        result    =
      )->get( )->TileContent(
        footer = `Files in the System`
        )->NumericContent( value = conv #( lv_count ) icon = 'sap-icon://documents' withMargin = abap_false
      ).

      page->generictile(
*      EXPORTING
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `File Editor`
         press     = client->_event( `z2ui5_tool_cl_app_01` )
      )->get( )->TileContent(
         )->imagecontent( src = 'sap-icon://edit'
      ).

      page->generictile(
*      EXPORTING
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `Database Viewer`
         press     = client->_event( `z2ui5_tool_cl_app_02` )
      )->get( )->TileContent(
         )->imagecontent( src = 'sap-icon://detail-view'
      ).

     page->generictile(
*      EXPORTING
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `CSV to Itab Editor`
         press     = client->_event( `z2ui5_tool_cl_app_07` )
      )->get( )->TileContent(
         )->imagecontent( src = 'sap-icon://table-view'
      ).

     page->generictile(
*      EXPORTING
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `List Report Viewer`
         press     = client->_event( `z2ui5_tool_cl_app_03` )
      )->get( )->TileContent(
         )->imagecontent( src = 'sap-icon://my-view'
      ).

*      hbox = box->hbox( ).
*      hbox->title( text = `Transformation` ).
*      hbox = box->hbox( ).
*
*
*      hbox->generictile(
**      EXPORTING
*        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
*        header    = `csv -> itab`
*        press     = client->_event( `z2ui5_tool_cl_app_07` )
**        frametype =
**        subheader =
**      RECEIVING
**        result    =
*).
*
*      hbox->generictile(
**      EXPORTING
*        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
*        header    = `db table -> csv`
*        press     = client->_event( `z2ui5_tool_cl_app_06` )
**        frametype =
**        subheader =
**      RECEIVING
**        result    =
*).
*
*      hbox = box->hbox( ).
*      hbox->title( text = `Maintenance` ).
*      hbox = box->hbox( ).
*
*      hbox->generictile(
**      EXPORTING
*  class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
*       header    = `tab`
**         press     =
**        frametype =
**        subheader =
**      RECEIVING
**        result    =
*  ).
*
*      hbox->generictile(
**      EXPORTING
*      class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
*       header    = `tab edit`
**         press     =
**        frametype =
**        subheader =
**      RECEIVING
**        result    =
*  ).
*
*      hbox = box->hbox( ).
*      hbox->title( text = `XML` ).
*      hbox = box->hbox( ).
*
*      page->generictile(
**      EXPORTING
*  class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
*   header    = `validator`
**         press     =
**        frametype =
**        subheader =
**      RECEIVING
**        result    =
*).

      client->view_display( view->stringify( ) ).

    ENDIF.


    mv_value = z2ui5_cl_xml_view=>factory( client )->hlp_get_url_param( `q` ).
    mv_value = mv_value && mv_value.

    z2ui5_cl_xml_view=>factory( client )->hlp_set_url_param( n = `q` v = mv_value ).

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
