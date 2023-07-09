CLASS z2ui5_tool_cl_app_00 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.


    data mv_value type string.

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

      hbox->input( value = client->_bind( mv_value ) ).

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
        header    = `file editor`
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
        press     = client->_event( `z2ui5_tool_cl_app_07` )
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
