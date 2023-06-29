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

    DATA(view) = z2ui5_cl_xml_view=>factory( client ).

    DATA(page) = view->shell( )->page(
            title          = 'abap2UI5 - tools menu'
            navbuttonpress = client->_event( 'BACK' )
            shownavbutton  = abap_true ).

    page->generictile(
*      EXPORTING
*        class     =
         header    = `file upload download`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
    ).

        page->generictile(
*      EXPORTING
*        class     =
         header    = `editor`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
    ).

        page->generictile(
*      EXPORTING
*        class     =
         header    = `table maintenance`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
    ).

        page->generictile(
*      EXPORTING
*        class     =
         header    = `import / export`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
    ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

ENDCLASS.
