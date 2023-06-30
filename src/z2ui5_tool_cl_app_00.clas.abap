CLASS z2ui5_tool_cl_app_00 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_tool_cl_app_00 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    IF client->get( )-check_on_navigated = abap_true.

      DATA(view) = z2ui5_cl_xml_view=>factory( client ).

      DATA(page) = view->shell( )->page(
              title          = 'abap2UI5 - tools menu'
              navbuttonpress = client->_event( 'BACK' )
              shownavbutton  = abap_true ).

      page->generictile(
*      EXPORTING
  class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
           header    = `file upload download`
            press     = client->_event( `z2ui5_tool_cl_app_05` )
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
      ).

            page->generictile(
*      EXPORTING
  class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
           header    = `csv -> itab`
            press     = client->_event( `z2ui5_tool_cl_app_06` )
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
      ).

      page->generictile(
*      EXPORTING
  class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
       header    = `editor`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
  ).

      page->generictile(
*      EXPORTING
  class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
       header    = `table maintenance`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
  ).

      page->generictile(
*      EXPORTING
      class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
       header    = `import / export`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
  ).

      client->view_display( view->stringify( ) ).

    ENDIF.

    IF client->get( )-event IS NOT INITIAL.

      DATA li_app TYPE REF TO z2ui5_if_app.
      DATA(lv_classname) = to_upper( client->get( )-event ).
      CREATE OBJECT li_app TYPE (lv_classname).
      client->nav_app_call( li_app ).

    ENDIF.


  ENDMETHOD.

ENDCLASS.
