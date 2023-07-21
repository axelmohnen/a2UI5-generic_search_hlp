CLASS z2ui5_tool_cl_view DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_s_file TYPE z2ui5_tool_t_002.
    TYPES ty_t_file TYPE STANDARD TABLE OF z2ui5_tool_t_002 WITH EMPTY KEY.

    CLASS-METHODS db_create
      IMPORTING
        VALUE(val)    TYPE ty_S_file
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS db_read
      IMPORTING
        name          TYPE clike
      RETURNING
        VALUE(result) TYPE ty_S_file.

    CLASS-METHODS db_update_metadata
      IMPORTING
        val TYPE ty_S_file.

    CLASS-METHODS db_update_data
      IMPORTING
        val TYPE ty_S_file.

    CLASS-METHODS db_delete
      IMPORTING
        name TYPE clike.

    CLASS-METHODS db_read_all
      RETURNING
        VALUE(result) TYPE ty_t_file.

    CLASS-METHODS factory
      IMPORTING
        client          TYPE REF TO z2ui5_if_client
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5_tool_cl_view.

    METHODS display
      IMPORTING
        name TYPE string.

  PROTECTED SECTION.
    DATA  client TYPE REF TO z2ui5_if_client.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_tool_cl_view IMPLEMENTATION.


  METHOD factory.

    r_result = NEW #( ).
    r_result->client = client.

  ENDMETHOD.

  METHOD display.

    DATA(lv_view) = db_read( name )-data.

    SPLIT lv_view AT `{/oUpdate/` INTO TABLE DATA(lt_view).
    DELETE lt_view INDEX 1.

    DATA(lo_app) = CAST object( client->get( )-s_draft-app ).

    LOOP AT lt_view REFERENCE INTO DATA(lr_view).

      SPLIT lr_view->* AT `}` INTO DATA(lv_attribute) DATA(lv_dummy).

      ASSIGN lo_app->(lv_attribute) TO FIELD-SYMBOL(<ref>).
      IF <ref> IS ASSIGNED.
        client->_bind_edit( <ref> ).
      ENDIF.

    ENDLOOP.

    client->view_display( lv_view ).

  ENDMETHOD.

  METHOD db_create.

*    val-id = z2ui5_tool_cl_utility=>get_uuid( ).
*    val-file_size = strlen( val-data ).
    val-name = to_upper( val-name ).
    MODIFY z2ui5_tool_t_002 FROM @( val ).

*    result = val-id.

  ENDMETHOD.

  METHOD db_read.

    DATA(lv_name) = to_upper( name ).

    SELECT SINGLE FROM z2ui5_tool_t_002
        FIELDS *
       WHERE name = @lv_name
     INTO CORRESPONDING FIELDS OF @result.

  ENDMETHOD.

  METHOD db_read_all.

    SELECT FROM z2ui5_tool_t_002
    FIELDS name
    INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.

  METHOD db_delete.

    DELETE FROM z2ui5_tool_t_002 WHERE name = @name.

  ENDMETHOD.

  METHOD db_update_data.

    UPDATE z2ui5_tool_t_002
      SET
         data = @val-data
      WHERE name = @val-name.

  ENDMETHOD.

  METHOD db_update_metadata.

    UPDATE z2ui5_tool_t_002
      SET
         name = @val-name,
*        file_format = @val-file_format,
        descr = @val-descr
      WHERE name = @val-name.

  ENDMETHOD.

ENDCLASS.
