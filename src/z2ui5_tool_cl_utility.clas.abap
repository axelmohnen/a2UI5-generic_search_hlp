CLASS z2ui5_tool_cl_utility DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_table_by_json
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS get_table_by_xml
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS get_table_by_csv
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.

    CLASS-METHODS get_csv_by_table
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_xml_by_table
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_json_by_table
      IMPORTING
        val           TYPE any
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_fieldlist_by_table
      IMPORTING
        it_table      TYPE any
      RETURNING
        VALUE(result) TYPE string_table.


    CLASS-METHODS decode_x_base64
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS encode_x_base64
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_string_by_xstring
      IMPORTING
        val           TYPE xstring
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS get_xstring_by_string
      IMPORTING
        val           TYPE string
      RETURNING
        VALUE(result) TYPE xstring.

    CLASS-METHODS get_uuid
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_tool_cl_utility IMPLEMENTATION.


  METHOD get_uuid.
    TRY.

        DATA uuid TYPE c LENGTH 32.

        TRY.
            CALL METHOD (`CL_SYSTEM_UUID`)=>if_system_uuid_static~create_uuid_c32
              RECEIVING
                uuid = uuid.

          CATCH cx_sy_dyn_call_illegal_class.

            DATA(lv_fm) = `GUID_CREATE`.
            CALL FUNCTION lv_fm
              IMPORTING
                ev_guid_32 = uuid.

        ENDTRY.

        result = uuid.

      CATCH cx_root.
        ASSERT 1 = 0.
    ENDTRY.
  ENDMETHOD.

  METHOD get_table_by_json.

*    DATA lt_tab TYPE ty_t_table.
*
*    /ui2/cl_json=>deserialize(
*      EXPORTING
*        json             = val
**        jsonx            =
**        pretty_name      =
**        assoc_arrays     =
**        assoc_arrays_opt =
**        name_mappings    =
**        conversion_exits =
**        hex_as_base64    =
*      CHANGING
*        data             = lt_tab
*    ).
*
*    result = lt_tab.

  ENDMETHOD.


  METHOD get_table_by_xml.

*    DATA lt_tab TYPE ty_t_table.
*
*    CALL TRANSFORMATION id SOURCE xml = val RESULT data = lt_tab.
*
*    result = lt_tab.

  ENDMETHOD.

  METHOD get_table_by_csv.

    SPLIT val AT cl_abap_char_utilities=>cr_lf INTO TABLE DATA(lt_rows).
    SPLIT lt_rows[ 1 ] AT ';' INTO TABLE DATA(lt_cols).

    DATA lt_comp TYPE cl_abap_structdescr=>component_table.
    LOOP AT lt_cols REFERENCE INTO DATA(lr_col).
      INSERT VALUE #( name = lr_col->* type = cl_abap_elemdescr=>get_c( 40 ) ) INTO TABLE lt_comp.
    ENDLOOP.

    DATA(struc) = cl_abap_structdescr=>get( lt_comp ).
    DATA(o_table_desc) = cl_abap_tabledescr=>create(
          p_line_type  = CAST #( struc )
          p_table_kind = cl_abap_tabledescr=>tablekind_std
          p_unique     = abap_false ).

    CREATE DATA result TYPE HANDLE o_table_desc.

    LOOP AT lt_rows REFERENCE INTO DATA(lr_rows) FROM 2.

      SPLIT lr_rows->* AT ';' INTO TABLE lt_cols.
      DATA lr_row TYPE REF TO data.
      CREATE DATA lr_row TYPE HANDLE struc.

      LOOP AT lt_cols REFERENCE INTO lr_col.
        ASSIGN COMPONENT sy-index OF STRUCTURE lr_row->* TO FIELD-SYMBOL(<field>).
        <field> = lr_col->*.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD decode_x_base64.

    TRY.

        CALL METHOD ('CL_WEB_HTTP_UTILITY')=>('DECODE_X_BASE64')
          EXPORTING
            encoded = val
          RECEIVING
            decoded = result.

      CATCH cx_sy_dyn_call_illegal_class.

        DATA(classname) = 'CL_HTTP_UTILITY'.
        CALL METHOD (classname)=>('DECODE_X_BASE64')
          EXPORTING
            encoded = val
          RECEIVING
            decoded = result.

    ENDTRY.

  ENDMETHOD.


  METHOD encode_x_base64.

    TRY.

        CALL METHOD ('CL_WEB_HTTP_UTILITY')=>('ENCODE_X_BASE64')
          EXPORTING
            unencoded = val
          RECEIVING
            encoded   = result.

      CATCH cx_sy_dyn_call_illegal_class.

        DATA(classname) = 'CL_HTTP_UTILITY'.
        CALL METHOD (classname)=>('ENCODE_X_BASE64')
          EXPORTING
            unencoded = val
          RECEIVING
            encoded   = result.

    ENDTRY.

  ENDMETHOD.

  METHOD get_csv_by_table.

*    LOOP AT val INTO DATA(ls_row).
*
*      DATA(lv_index) = 1.
*      DO.
*        ASSIGN COMPONENT lv_index OF STRUCTURE ls_row TO FIELD-SYMBOL(<field>).
*        IF sy-subrc <> 0.
*          EXIT.
*        ENDIF.
*        lv_index = lv_index + 1.
*        result = result && <field> && ';'.
*      ENDDO.
*      result = result && cl_abap_char_utilities=>cr_lf.
*    ENDLOOP.


  ENDMETHOD.

  METHOD get_json_by_table.

    result = /ui2/cl_json=>serialize(
               val
*               compress         =
*               name             =
*               pretty_name      =
*               type_descr       =
*               assoc_arrays     =
*               ts_as_iso8601    =
*               expand_includes  =
*               assoc_arrays_opt =
*               numc_as_string   =
*               name_mappings    =
*               conversion_exits =
           "   format_output    = abap_true
*               hex_as_base64    =
             ).


  ENDMETHOD.

  METHOD get_xml_by_table.

    CALL TRANSFORMATION id SOURCE values = val RESULT XML result.

  ENDMETHOD.

  METHOD get_fieldlist_by_table.

    DATA(lo_tab) = CAST cl_abap_tabledescr( cl_abap_datadescr=>describe_by_data( it_table ) ).
    DATA(lo_struc) = CAST cl_abap_structdescr( lo_tab->get_table_line_type( ) ).

    DATA(lt_comp) = lo_struc->get_components( ).

    LOOP AT lt_comp INTO DATA(ls_comp).
      INSERT ls_comp-name INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_string_by_xstring.

    DATA conv TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
          RECEIVING
            instance = conv.

        CALL METHOD conv->('IF_ABAP_CONV_IN~CONVERT')
          EXPORTING
            source = val
          RECEIVING
            result = result.
      CATCH cx_sy_dyn_call_illegal_class.

        DATA(conv_in_class) = 'CL_ABAP_CONV_IN_CE'.
        CALL METHOD (conv_in_class)=>create
          EXPORTING
            encoding = 'UTF-8'
          RECEIVING
            conv     = conv.

        CALL METHOD conv->('CONVERT')
          EXPORTING
            input = val
          IMPORTING
            data  = result.
    ENDTRY.

  ENDMETHOD.

  METHOD get_xstring_by_string.

    DATA conv TYPE REF TO object.

    TRY.
        CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_out
          RECEIVING
            instance = conv.

        CALL METHOD conv->('IF_ABAP_CONV_OUT~CONVERT')
          EXPORTING
            source = val
          RECEIVING
            result = result.
      CATCH cx_sy_dyn_call_illegal_class.

        DATA(conv_out_class) = 'CL_ABAP_CONV_OUT_CE'.
        CALL METHOD (conv_out_class)=>create
          EXPORTING
            encoding = 'UTF-8'
          RECEIVING
            conv     = conv.

        CALL METHOD conv->('CONVERT')
          EXPORTING
            data   = val
          IMPORTING
            buffer = result.
    ENDTRY.



*    result = cl_abap_conv_codepage=>create_out( )->convert( val ).

  ENDMETHOD.

ENDCLASS.
