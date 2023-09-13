CLASS z2ui5_cl_tool_app_02 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA mt_table TYPE REF TO data.
    DATA mt_cols TYPE string_table.
    DATA mv_name TYPE string.

    TYPES:
      BEGIN OF ty_S_range,
        name    TYPE string,
        value   TYPE string,
        t_range TYPE RANGE OF string,
      END OF ty_S_range.

    DATA mt_range TYPE STANDARD TABLE OF ty_S_range.
  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.
    DATA:
      BEGIN OF app,
        check_initialized TYPE abap_bool,
        view_main         TYPE string,
        view_popup        TYPE string,
        get               TYPE z2ui5_if_client=>ty_s_get,
      END OF app.

    METHODS z2ui5_on_init.
    METHODS z2ui5_on_event.
    METHODS z2ui5_on_render.

  PRIVATE SECTION.
ENDCLASS.



CLASS Z2UI5_CL_TOOL_APP_02 IMPLEMENTATION.


  METHOD z2ui5_if_app~main.

    me->client     = client.
    app-get        = client->get( ).
    app-view_popup = ``.

    IF app-check_initialized = abap_false.
      app-check_initialized = abap_true.
      z2ui5_on_init( ).
    ENDIF.

    IF app-get-event IS NOT INITIAL.
      z2ui5_on_event( ).
    ENDIF.

    z2ui5_on_render( ).


  ENDMETHOD.


  METHOD z2ui5_on_event.

    CASE client->get( )-event.

      WHEN 'BUTTON_TABLE'.
        FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        CREATE DATA mt_table TYPE STANDARD TABLE OF (mv_name) WITH DEFAULT KEY.
        ASSIGN mt_table->* TO <tab>.
        mt_cols = z2ui5_cl_tool_utility=>get_fieldlist_by_table( <tab> ).


      WHEN 'BUTTON_POST'.

        CREATE DATA mt_table TYPE STANDARD TABLE OF (mv_name).
        ASSIGN mt_table->* TO <tab>.

        SELECT FROM (mv_name)
            FIELDS *
          INTO CORRESPONDING FIELDS OF TABLE @<tab>
            UP TO 50 ROWS.

      WHEN 'BUTTON_CONFIRM'.
        client->message_toast_display( |confirm| ).
        app-view_popup = ''.

      WHEN 'BUTTON_CANCEL'.
        client->message_toast_display( |cancel| ).
        app-view_popup = ''.

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD z2ui5_on_init.

    app-view_main = 'VIEW_MAIN'.
    mv_name = `Z2UI5_T_DRAFT`.

  ENDMETHOD.


  METHOD z2ui5_on_render.

    DATA(lo_view) = z2ui5_cl_xml_view=>factory( client )->shell( )->page(
             title          = 'abap2UI5 - Database View'
             navbuttonpress = client->_event( 'BACK' )
             shownavbutton  = abap_true
         )->header_content(
             )->link(
                 text = 'Demo' target = '_blank'
                 href = 'https://twitter.com/abap2UI5/status/1656904560953237508'
             )->link(
                 text = 'Source_Code' target = '_blank'
*                 href = z2ui5_cl_xml_view=>hlp_get_source_code_url( app = me )
         )->get_parent(
         )->simple_form(  editable = abap_true
             )->content( `form`
                 )->title( 'Table'
                 )->label( 'Name' ).

    lo_view->input( client->_bind_edit( mv_name  ) ).

    lo_view->button(
                text  = 'read'
                press = client->_event( 'BUTTON_POST' )
            ).


    IF mt_table IS BOUND.

      FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
      ASSIGN mt_table->* TO <tab>.
      mt_cols = z2ui5_cl_tool_utility=>get_fieldlist_by_table( <tab> ).

      mt_range = VALUE #( FOR line IN mt_cols ( name = line ) ).

      lo_view->get_parent( )->get_parent( )->list(
        items = client->_bind( mt_range )
        headertext      = `Filter`
        )->custom_list_item(
            )->hbox(
                )->text( `{NAME}`
                )->input( value = `{VALUE}` enabled = abap_false
        ).



      DATA(tab) = lo_view->get_parent( )->get_parent( )->simple_form( editable = abap_true
                )->content( 'form' )->table(
                  items = client->_bind( val = <tab> )
              ).

      DATA(lo_columns) = tab->columns( ).


      LOOP AT mt_cols INTO DATA(lv_field) FROM 2.
        lo_columns->column( )->text( lv_field ).
      ENDLOOP.

      DATA(lo_cells) = tab->items( )->column_list_item( selected = '{SELKZ}' )->cells( ).
      LOOP AT mt_cols INTO lv_field FROM 2.
        lo_cells->input( `{` && lv_field && `}` ).
      ENDLOOP.

    ENDIF.

    client->view_display( lo_view->stringify( ) ).
*    app-next-xml_main = lo_view->get_root( )->xml_get( ).

  ENDMETHOD.
ENDCLASS.
