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

      DATA(box) = page->vbox( ).
      DATA(hbox) = box->hbox( ).
      hbox->title( text = `Files` ).

      hbox = box->hbox( ).

      hbox->generictile(
*      EXPORTING
         class     = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
         header    = `upload & download`
         press     = client->_event( `z2ui5_tool_cl_app_05` )
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
      ).

      hbox->generictile(
*      EXPORTING
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `editor`
         press     = client->_event( `z2ui5_tool_cl_app_01` )
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
  ).

      hbox = box->hbox( ).
      hbox->title( text = `Transformation` ).
      hbox = box->hbox( ).


      hbox->generictile(
*      EXPORTING
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `csv -> itab`
        press     = client->_event( `z2ui5_tool_cl_app_06` )
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
).

      hbox->generictile(
*      EXPORTING
        class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
        header    = `db table -> csv`
        press     = client->_event( `z2ui5_tool_cl_app_06` )
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
).

      hbox = box->hbox( ).
      hbox->title( text = `Maintenance` ).
      hbox = box->hbox( ).

      hbox->generictile(
*      EXPORTING
  class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
       header    = `tab`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
  ).

      hbox->generictile(
*      EXPORTING
      class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
       header    = `tab edit`
*         press     =
*        frametype =
*        subheader =
*      RECEIVING
*        result    =
  ).

      hbox = box->hbox( ).
      hbox->title( text = `XML` ).
      hbox = box->hbox( ).

      page->generictile(
*      EXPORTING
  class = 'sapUiTinyMarginBegin sapUiTinyMarginTop tileLayout'
   header    = `validator`
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
