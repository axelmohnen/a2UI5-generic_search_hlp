CLASS z2ui5_tool_cl_app_06 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    CLASS-METHODS read_view
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_tool_cl_app_06 IMPLEMENTATION.

  METHOD read_view.

*    result = `<mvc:View ` && |\n|  &&
*             `  xmlns="sap.m" ` && |\n|  &&
*             `  xmlns:z2ui5="z2ui5" ` && |\n|  &&
*             `  xmlns:core="sap.ui.core" ` && |\n|  &&
*             `  xmlns:mvc="sap.ui.core.mvc" ` && |\n|  &&
*             `  xmlns:layout="sap.ui.layout" ` && |\n|  &&
*             `  xmlns:f="sap.f" ` && |\n|  &&
*             `  xmlns:form="sap.ui.layout.form" ` && |\n|  &&
*             `  xmlns:editor="sap.ui.codeeditor" ` && |\n|  &&
*             `  xmlns:mchart="sap.suite.ui.microchart" ` && |\n|  &&
*             `  xmlns:webc="sap.ui.webc.main" ` && |\n|  &&
*             `  xmlns:uxap="sap.uxap" ` && |\n|  &&
*             `  xmlns:sap="sap" ` && |\n|  &&
*             `  xmlns:text="sap.ui.richtextedito" ` && |\n|  &&
*             `  xmlns:html="http://www.w3.org/1999/xhtml" ` && |\n|  &&
*             `  displayBlock="true" ` && |\n|  &&
*             `  height="100%" ` && |\n|  &&
*             `  controllerName="z2ui5_controller" ` && |\n|  &&
*             ` > <Shell ` && |\n|  &&
*             ` > <Page ` && |\n|  &&
*             `  title="abap2UI5 - z2ui5_cl_app_hello_world" ` && |\n|  &&
*             ` > <form:SimpleForm ` && |\n|  &&
*             `  title="Hello World" ` && |\n|  &&
*             `  editable="true" ` && |\n|  &&
*             ` > <form:content ` && |\n|  &&
*             ` > <Title ` && |\n|  &&
*             `  text="Make an input here and send it to the server..." ` && |\n|  &&
*             ` /> <Label ` && |\n|  &&
*             `  text="quantity" ` && |\n|  &&
*             ` /> <Input ` && |\n|  &&
*             `  value="{/oUpdate/QUANTITY}"` && |\n|  &&
*             ` /> <Label ` && |\n|  &&
*             `  text="product" ` && |\n|  &&
*             ` /> <Input ` && |\n|  &&
*             `  enabled="false" ` && |\n|  &&
*             `  value="{VALUE}" ` && |\n|  &&
*             ` /> <Button ` && |\n|  &&
*             `  press="onEvent( { &apos;EVENT&apos; : &apos;BUTTON_POST&apos;, &apos;METHOD&apos; : &apos;UPDATE&apos; , &apos;isHoldView&apos; : false } )" ` && |\n|  &&
*             `  text="post" ` && |\n|  &&
*             ` /></form:content></form:SimpleForm></Page></Shell></mvc:View>`.

    result = 'test'.

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

*    "frontend -> backend
*    IF client->get( )-s_config-body IS NOT INITIAL.
*      DATA lr_model TYPE REF TO data.
*      /ui2/cl_json=>deserialize(
*        EXPORTING
*          json             = client->get( )-s_config-body
*        CHANGING
*          data             = lr_model
*      ).
*      ASSIGN (`LR_MODEL->OUPDATE->QUANTITY->*`) TO FIELD-SYMBOL(<field>).
*      quantity = <field>.
*    ENDIF.
*
*    IF check_initialized = abap_false.
*      check_initialized = abap_true.
*      quantity = `600`.
*    ENDIF.
*
*
*
*
*    "backend -> frontend
*    DATA(lv_model_new) = `{ "oUpdate" : { "QUANTITY" : "` && quantity && `" } }`.
*    DATA(lv_view_new)  = read_view( ).
*
*    client->view_display( lv_view_new ).
**        _viewmodel = lv_model_new


  ENDMETHOD.

ENDCLASS.
