*"* use this source file for your ABAP unit test classes

CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      first_test FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD first_test.

    DATA(lv_csv_base64) =
`VXNlcm5hbWUsIElkZW50aWZpZXIsRmlyc3QgbmFtZSxMYXN0IG5hbWUNCmJvb2tlcjEyLDkwMTIsUmFjaGVsLEJvb2tlcg0KZ3JleTA3LDIwNzAsTGF1cmEsR3JleQ0Kam9obnNvbjgxLDQwODEsQ3JhaWcsSm9obnNvbg0KamVua2luczQ2LDkzNDYsTWFyeSxKZW5raW5zDQpzbWl0aDc5LDUwNzksSmFtaWUsU21pdGg=`.

    DATA(lv_data) = lcl_mime_api=>decode_x_base64( lv_csv_base64 ).
    DATA(lv_ready) = lcl_mime_api=>get_string_by_xstring( lv_data ).

    DATA(lv_readyx) = lcl_mime_api=>get_xstring_by_string( lv_ready ).
    DATA(lv_data2) = lcl_mime_api=>encode_x_base64( lv_readyx ).

    IF lv_data2 <> lv_csv_base64.
      ASSERT 1 = 0.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
