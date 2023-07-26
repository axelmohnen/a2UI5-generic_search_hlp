CLASS z2ui5_tool_cl_file_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_s_file TYPE z2ui5_tool_t_001.
    TYPES ty_t_file TYPE STANDARD TABLE OF z2ui5_tool_t_001 WITH EMPTY KEY.

    CLASS-METHODS create
      IMPORTING
        VALUE(val)    TYPE ty_S_file
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS read
      IMPORTING
        id            TYPE clike
      RETURNING
        VALUE(result) TYPE ty_S_file.

    CLASS-METHODS update_metadata
      IMPORTING
        val TYPE ty_S_file.

    CLASS-METHODS update_data
      IMPORTING
        val TYPE ty_S_file.

    CLASS-METHODS delete
      IMPORTING
        id TYPE clike.

    CLASS-METHODS read_all
      RETURNING
        VALUE(result) TYPE ty_t_file.

    CLASS-METHODS get_editor_type
      RETURNING
        VALUE(r_result) TYPE z2ui5_if_client=>ty_t_name_value.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2UI5_TOOL_CL_FILE_API IMPLEMENTATION.


  METHOD create.

    val-id = z2ui5_tool_cl_utility=>get_uuid( ).
    val-file_size = strlen( val-data ).
    MODIFY z2ui5_tool_t_001 FROM @( val ).

    result = val-id.

  ENDMETHOD.


  METHOD delete.

    DELETE FROM z2ui5_tool_t_001 WHERE id = @id.

  ENDMETHOD.


  METHOD get_editor_type.

    DATA(lv_types) = `abap, abc, actionscript, ada, apache_conf, applescript, asciidoc, assembly_x86, autohotkey, batchfile, bro, c9search, c_cpp, cirru, clojure, cobol, coffee, coldfusion, csharp, css, curly, d, dart, diff, django, dockerfile, ` &&
`dot, drools, eiffel, yaml, ejs, elixir, elm, erlang, forth, fortran, ftl, gcode, gherkin, gitignore, glsl, gobstones, golang, groovy, haml, handlebars, haskell, haskell_cabal, haxe, hjson, html, html_elixir, html_ruby, ini, io, jack, jade, java, ja` &&
`vascri` &&
`pt, json, jsoniq, jsp, jsx, julia, kotlin, latex, lean, less, liquid, lisp, live_script, livescript, logiql, lsl, lua, luapage, lucene, makefile, markdown, mask, matlab, mavens_mate_log, maze, mel, mips_assembler, mipsassembler, mushcode, mysql, ni` &&
`x, nsis, objectivec, ocaml, pascal, perl, pgsql, php, plain_text, powershell, praat, prolog, properties, protobuf, python, r, razor, rdoc, rhtml, rst, ruby, rust, sass, scad, scala, scheme, scss, sh, sjs, smarty, snippets, soy_template, space, sql,` &&
` sqlserver, stylus, svg, swift, swig, tcl, tex, text, textile, toml, tsx, twig, typescript, vala, vbscript, velocity, verilog, vhdl, wollok, xml, xquery, terraform, slim, redshift, red, puppet, php_laravel_blade, mixal, jssm, fsharp, edifact,` &&
` csp, cssound_score, cssound_orchestra, cssound_document`.
    SPLIT lv_types AT ',' INTO TABLE DATA(lt_types).

    r_result = VALUE #( FOR row IN lt_types (  n = shift_right( shift_left( row ) ) v = shift_right( shift_left( row ) ) ) ).

  ENDMETHOD.


  METHOD read.

    SELECT SINGLE FROM z2ui5_tool_t_001
        FIELDS *
       WHERE id = @id
     INTO CORRESPONDING FIELDS OF @result.

  ENDMETHOD.


  METHOD read_all.

    SELECT FROM z2ui5_tool_t_001
    FIELDS name, file_format, file_size, id, descr
    INTO CORRESPONDING FIELDS OF TABLE @result.

  ENDMETHOD.


  METHOD update_data.

    UPDATE z2ui5_tool_t_001
      SET
         data = @val-data
      WHERE id = @val-id.

  ENDMETHOD.


  METHOD update_metadata.

    UPDATE z2ui5_tool_t_001
      SET
         name = @val-name,
        file_format = @val-file_format,
        descr = @val-descr
      WHERE id = @val-id.

  ENDMETHOD.
ENDCLASS.
