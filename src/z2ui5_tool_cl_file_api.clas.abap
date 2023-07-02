CLASS z2ui5_tool_cl_file_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_s_file TYPE z2ui5_tool_t_001.
    TYPES ty_t_file TYPE STANDARD TABLE OF z2ui5_tool_t_001 WITH EMPTY KEY.

    CLASS-METHODS create
      IMPORTING
        val TYPE ty_S_file.

    CLASS-METHODS read
      IMPORTING
        id            TYPE clike
      RETURNING
        VALUE(result) TYPE ty_S_file.

    CLASS-METHODS read_all
      RETURNING
        VALUE(result) TYPE ty_t_file.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_tool_cl_file_api IMPLEMENTATION.

  METHOD create.

    MODIFY z2ui5_tool_t_001 FROM @( val ).

  ENDMETHOD.

  METHOD read.

    SELECT SINGLE FROM z2ui5_tool_t_001
        FIELDS *
       WHERE uuid = @id
     INTO CORRESPONDING FIELDS OF @result.

  ENDMETHOD.

  METHOD read_all.

    SELECT FROM z2ui5_tool_t_001
    FIELDS name, file_format, file_size, uuid
    INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.

ENDCLASS.
