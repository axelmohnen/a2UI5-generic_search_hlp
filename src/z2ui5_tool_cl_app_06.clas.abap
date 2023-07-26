CLASS z2ui5_tool_cl_app_06 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA quantity TYPE i.
    DATA text TYPE string VALUE `quantity`.
    DATA check_initialized TYPE abap_bool.
    DATA href TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2UI5_TOOL_CL_APP_06 IMPLEMENTATION.


  METHOD z2ui5_if_app~main.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      quantity = 10.
      href = href && `?app_start=z2ui5_tool_cl_app_08&view=` && 'TEST_VIEW'.

    ENDIF.

    z2ui5_tool_cl_view=>factory( client )->display( 'TEST_VIEW' ).

  ENDMETHOD.
ENDCLASS.
