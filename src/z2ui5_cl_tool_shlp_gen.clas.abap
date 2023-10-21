CLASS z2ui5_cl_tool_shlp_gen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    class-METHODS zfc_ddic_search_help
      IMPORTING
        !irparent            TYPE REF TO z2ui5_cl_xml_view
        !resultitabname      TYPE clike OPTIONAL
        !shlpfieldsstrucname TYPE clike OPTIONAL
        !irclient            TYPE REF TO z2ui5_if_client OPTIONAL
        !resultitabevent     TYPE clike OPTIONAL
        !closebuttontext     TYPE clike OPTIONAL
        !searchbuttontext    TYPE clike OPTIONAL
        !searchevent         TYPE clike OPTIONAL
        !isshlp              TYPE any OPTIONAL
        ircontroller         TYPE REF TO object OPTIONAL
        shlpid               TYPE string OPTIONAL
      RETURNING
        VALUE(result)        TYPE REF TO z2ui5_cl_xml_view ##NEEDED.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_cl_tool_shlp_gen IMPLEMENTATION.



  METHOD zfc_ddic_search_help.

    TYPES ty_fields TYPE SORTED TABLE OF char30 WITH UNIQUE KEY table_line.

    TYPES:
      BEGIN OF ty_ddshtextsearch,
        request(60) TYPE c,
        fields      TYPE ty_fields,
      END OF ty_ddshtextsearch.


    TYPES:
      BEGIN OF ty_ddshselops,
        shlpname(30)  TYPE c,
        shlpfield(30) TYPE c,
        sign(1)       TYPE c,
        option(2)     TYPE c,
        low(45)       TYPE c,
        high(45)      TYPE c,
      END OF ty_ddshselops.

    TYPES t_ty_ddshselops TYPE TABLE OF ty_ddshselops WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_ddshfprops,
        fieldname(30)  TYPE c,
        shlpinput(1)   TYPE c,
        shlpoutput(1)  TYPE c,
        shlpselpos(2)  TYPE n,
        shlplispos(2)  TYPE n,
        shlpseldis(1)  TYPE c,
        defaultval(21) TYPE c,
      END OF ty_ddshfprops.

    TYPES t_ty_ddshfprops TYPE TABLE OF ty_ddshfprops WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_ddfields,
        tabname(30)     TYPE c,
        fieldname(30)   TYPE c,
        langu(1)        TYPE c,
        position(4)     TYPE n,
        offset(6)       TYPE n,
        domname(30)     TYPE c,
        rollname(30)    TYPE c,
        checktable(30)  TYPE c,
        leng(6)         TYPE n,
        intlen(6)       TYPE n,
        outputlen(6)    TYPE n,
        decimals(6)     TYPE n,
        datatype(4)     TYPE c,
        inttype(1)      TYPE c,
        reftable(30)    TYPE c,
        reffield(30)    TYPE c,
        precfield(30)   TYPE c,
        authorid(3)     TYPE c,
        memoryid(20)    TYPE c,
        logflag(1)      TYPE c,
        mask(20)        TYPE c,
        masklen(4)      TYPE n,
        convexit(5)     TYPE c,
        headlen(2)      TYPE n,
        scrlen1(2)      TYPE n,
        scrlen2(2)      TYPE n,
        scrlen3(2)      TYPE n,
        fieldtext(60)   TYPE c,
        reptext(55)     TYPE c,
        scrtext_s(10)   TYPE c,
        scrtext_m(20)   TYPE c,
        scrtext_l(40)   TYPE c,
        keyflag(1)      TYPE c,
        lowercase(1)    TYPE c,
        mac(1)          TYPE c,
        genkey(1)       TYPE c,
        noforkey(1)     TYPE c,
        valexi(1)       TYPE c,
        noauthch(1)     TYPE c,
        sign(1)         TYPE c,
        dynpfld(1)      TYPE c,
        f4availabl(1)   TYPE c,
        comptype(1)     TYPE c,
        lfieldname(132) TYPE c,
        ltrflddis(1)    TYPE c,
        bidictrlc(1)    TYPE c,
        outputstyle(2)  TYPE n,
        nohistory(1)    TYPE c,
        ampmformat(1)   TYPE c,
      END OF ty_ddfields.

    TYPES t_ty_ddfields TYPE TABLE OF ty_ddfields WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_ddshifaces,
        shlpfield(30)  TYPE c,
        valtabname(30) TYPE c,
        valfield(132)  TYPE c,
        value(132)     TYPE c,
        internal(1)    TYPE c,
        dispfield(1)   TYPE c,
        f4field(1)     TYPE c,
        topshlpnam(30) TYPE c,
        topshlpfld(30) TYPE c,
      END OF ty_ddshifaces.

    TYPES t_ty_ddshifaces TYPE TABLE OF ty_ddshifaces WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_intdescr,
        issimple(1)         TYPE c,
        hotkey(1)           TYPE c,
        selmtype(1)         TYPE c,
        selmethod(30)       TYPE c,
        texttab(30)         TYPE c,
        selmexit(30)        TYPE c,
        dialogtype(1)       TYPE c,
        ddlanguage(1)       TYPE c,
        ddtext(60)          TYPE c,
        dialoginfo(1)       TYPE c,
        f4state(1)          TYPE c,
        tabname(30)         TYPE c,
        fieldname(30)       TYPE c,
        title(60)           TYPE c,
        history(1)          TYPE c,
        handle              TYPE int4,
        autosuggest(1)      TYPE c,
        fuzzy_search(1)     TYPE c,
        fuzzy_similarity(2) TYPE p DECIMALS 1,
      END OF ty_intdescr.

    TYPES:
      BEGIN OF ty_shlp_descr,
        shlpname(30) TYPE c,
        shlptype(2)  TYPE c,
        intdescr     TYPE ty_intdescr,
        interface    TYPE t_ty_ddshifaces,
        fielddescr   TYPE t_ty_ddfields,
        fieldprop    TYPE t_ty_ddshfprops,
        selopt       TYPE t_ty_ddshselops,
        textsearch   TYPE ty_ddshtextsearch,
      END OF ty_shlp_descr.

    DATA ls_shlp TYPE ty_shlp_descr.

    DATA: lv_grid_form_no     TYPE i,
          lt_arg              TYPE string_table,
          lv_arg_fieldname    TYPE string,
          lv_cell_fieldname   TYPE string,
          lv_path_result_itab TYPE string,
          lv_path_shlp_fields TYPE string,
          lt_fieldprop_sel    TYPE STANDARD TABLE OF ty_ddshfprops WITH EMPTY KEY,
          lt_fieldprop_lis    TYPE STANDARD TABLE OF ty_ddshfprops WITH EMPTY KEY,
          lt_ddffields        TYPE STANDARD TABLE OF ty_ddfields WITH EMPTY KEY,
          shlp_id(30)         TYPE c.

    FIELD-SYMBOLS:
      <lt_result_itab> TYPE ANY TABLE,
      <ls_shlp_fields> TYPE any,
      <lv_field>       TYPE any.

* ---------- Get result itab reference ------------------------------------------------------------
    lv_path_result_itab = 'IRCONTROLLER->' && resultitabname.
    ASSIGN (lv_path_result_itab) TO <lt_result_itab>.
* ---------- Get searchhelp input fields structure reference --------------------------------------
    lv_path_shlp_fields = 'IRCONTROLLER->' && shlpfieldsstrucname.
    ASSIGN (lv_path_shlp_fields) TO <ls_shlp_fields>.

    IF <lt_result_itab> IS NOT ASSIGNED OR
      <ls_shlp_fields> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    IF isshlp IS INITIAL.
      TRY.
          shlp_id = shlpid.

          DATA(lv_function) = `F4IF_GET_SHLP_DESCR`.
          "get shlp data
          CALL FUNCTION lv_function
            EXPORTING
              shlpname = shlp_id
            IMPORTING
              shlp     = ls_shlp.
        CATCH cx_root.
          RETURN.
      ENDTRY.

      lt_fieldprop_sel = ls_shlp-fieldprop.
      lt_fieldprop_lis = ls_shlp-fieldprop.

    ELSE.

      ASSIGN COMPONENT 'FIELDPROP' OF STRUCTURE isshlp TO FIELD-SYMBOL(<fs_fieldprop>).
      IF <fs_fieldprop> IS NOT ASSIGNED.
        RETURN.
      ENDIF.

      lt_fieldprop_sel = <fs_fieldprop>.
      lt_fieldprop_lis = <fs_fieldprop>.

    ENDIF.

* ---------- Set Selection and List properties ----------------------------------------------------

    DELETE lt_fieldprop_sel WHERE shlpselpos IS INITIAL.
    DELETE lt_fieldprop_lis WHERE shlplispos IS INITIAL.
    SORT lt_fieldprop_sel BY shlpselpos.
    SORT lt_fieldprop_lis BY shlplispos.

    IF ls_shlp IS NOT INITIAL.
      FIELD-SYMBOLS <fs_isshlp_fielddescr> TYPE STANDARD TABLE.
      ASSIGN COMPONENT 'FIELDDESCR' OF STRUCTURE ls_shlp TO <fs_isshlp_fielddescr>.
    ELSE.
      ASSIGN COMPONENT 'FIELDDESCR' OF STRUCTURE isshlp TO <fs_isshlp_fielddescr>.
    ENDIF.

    IF <fs_isshlp_fielddescr> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    lt_ddffields = <fs_isshlp_fielddescr>.
* -------------------------------------------------------------------------------------------------
*Searchfield Grid
* -------------------------------------------------------------------------------------------------
    DATA(lr_grid_shlp) = irparent->content( )->toolbar( )->toolbar_spacer(
          )->button( text = searchbuttontext
                     type    = 'Emphasized'
                     press   = irclient->_event( searchevent ) )->get_parent(
            )->grid( 'L3 M3 S3' )->content( 'layout' ).

* ---------- Create 4 forms (grid columns) --------------------------------------------------------
    DATA(lr_form_shlp_1) = lr_grid_shlp->simple_form( )->content( 'form' ).
    DATA(lr_form_shlp_2) = lr_grid_shlp->simple_form( )->content( 'form' ).
    DATA(lr_form_shlp_3) = lr_grid_shlp->simple_form( )->content( 'form' ).
    DATA(lr_form_shlp_4) = lr_grid_shlp->simple_form( )->content( 'form' ).

    LOOP AT lt_fieldprop_sel ASSIGNING FIELD-SYMBOL(<ls_fieldprop_sel>).
* ---------- Init loop data -----------------------------------------------------------------------
      UNASSIGN: <lv_field>.
* ---------- Get corresponding field description --------------------------------------------------
      ASSIGN lt_ddffields[ fieldname = <ls_fieldprop_sel>-fieldname ] TO FIELD-SYMBOL(<ls_fielddescr>).

      IF <ls_fielddescr> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

* ---------- Get field reference ------------------------------------------------------------------
      ASSIGN COMPONENT <ls_fielddescr>-fieldname OF STRUCTURE <ls_shlp_fields> TO <lv_field>.
      IF <lv_field> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

* ---------- Determine grid form number -----------------------------------------------------------
      IF lv_grid_form_no IS INITIAL.
        lv_grid_form_no = 1.
      ELSEIF lv_grid_form_no = 4.
        lv_grid_form_no = 1.
      ELSE.
        lv_grid_form_no = lv_grid_form_no + 1.
      ENDIF.

      CASE lv_grid_form_no.
        WHEN 1.
* ---------- Grid 1--------------------------------------------------------------------------------
* ---------- Set field label ----------------------------------------------------------------------
          lr_form_shlp_1->label( <ls_fielddescr>-scrtext_l ).

* ---------- Set input field ----------------------------------------------------------------------
          CASE <ls_fielddescr>-datatype.
            WHEN 'DATS'.
              lr_form_shlp_1->date_picker( value  = irclient->_bind_edit( <lv_field> ) ).
            WHEN 'TIMS'.
              lr_form_shlp_1->time_picker( value  = irclient->_bind_edit( <lv_field> ) ).
            WHEN OTHERS.
              lr_form_shlp_1->input( value = irclient->_bind_edit( <lv_field> ) ).
          ENDCASE.

        WHEN 2.
* ---------- Grid 2--------------------------------------------------------------------------------
* ---------- Set field label ----------------------------------------------------------------------
          lr_form_shlp_2->label( <ls_fielddescr>-scrtext_l ).

* ---------- Set input field ----------------------------------------------------------------------
          CASE <ls_fielddescr>-datatype.
            WHEN 'DATS'.
              lr_form_shlp_2->date_picker( value  = irclient->_bind_edit( <lv_field> ) ).
            WHEN 'TIMS'.
              lr_form_shlp_2->time_picker( value  = irclient->_bind_edit( <lv_field> ) ).
            WHEN OTHERS.
              lr_form_shlp_2->input( value = irclient->_bind_edit( <lv_field> ) ).
          ENDCASE.

        WHEN 3.
* ---------- Grid 3--------------------------------------------------------------------------------
* ---------- Set field label ----------------------------------------------------------------------
          lr_form_shlp_3->label( <ls_fielddescr>-scrtext_l ).

* ---------- Set input field ----------------------------------------------------------------------
          CASE <ls_fielddescr>-datatype.
            WHEN 'DATS'.
              lr_form_shlp_3->date_picker( value  = irclient->_bind_edit( <lv_field> ) ).
            WHEN 'TIMS'.
              lr_form_shlp_3->time_picker( value  = irclient->_bind_edit( <lv_field> ) ).
            WHEN OTHERS.
              lr_form_shlp_3->input( value = irclient->_bind_edit( <lv_field> ) ).
          ENDCASE.

        WHEN 4.
* ---------- Grid 4--------------------------------------------------------------------------------
* ---------- Set field label ----------------------------------------------------------------------
          lr_form_shlp_4->label( <ls_fielddescr>-scrtext_l ).

* ---------- Set input field ----------------------------------------------------------------------
          CASE <ls_fielddescr>-datatype.
            WHEN 'DATS'.
              lr_form_shlp_4->date_picker( value  = irclient->_bind_edit( <lv_field> ) ).
            WHEN 'TIMS'.
              lr_form_shlp_4->time_picker( value  = irclient->_bind_edit( <lv_field> ) ).
            WHEN OTHERS.
              lr_form_shlp_4->input( value = irclient->_bind_edit( <lv_field> ) ).
          ENDCASE.

      ENDCASE.

      UNASSIGN <ls_fielddescr>.

    ENDLOOP.

* ---------- Create table -------------------------------------------------------------------------
    DATA(lr_table) = irparent->table( items = irclient->_bind_edit( <lt_result_itab> ) ).
* ---------- Create Columns -----------------------------------------------------------------------
    DATA(lr_columns) = lr_table->columns( ).

* ---------- Set column ---------------------------------------------------------------------------
    LOOP AT lt_fieldprop_lis ASSIGNING FIELD-SYMBOL(<ls_fieldprop_lis>).
* ---------- Init loop data -----------------------------------------------------------------------
      UNASSIGN: <ls_fielddescr>.

* ---------- Get corresponding field description --------------------------------------------------
      ASSIGN lt_ddffields[ fieldname = <ls_fieldprop_lis>-fieldname ] TO <ls_fielddescr>.
      IF <ls_fielddescr> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      lr_columns->column( )->text( <ls_fielddescr>-scrtext_l ).
    ENDLOOP.

* ---------- Build export parameter list ----------------------------------------------------------
    LOOP AT lt_fieldprop_lis ASSIGNING <ls_fieldprop_lis> WHERE shlpoutput = abap_true.
* ---------- Init loop data -----------------------------------------------------------------------
      CLEAR: lv_arg_fieldname.

* ---------- Build parameter name -----------------------------------------------------------------
      lv_arg_fieldname = `${` && <ls_fieldprop_lis>-fieldname && `}`.

* ---------- Collect output fields ----------------------------------------------------------------
      APPEND lv_arg_fieldname TO lt_arg.
    ENDLOOP.

    DATA(lr_item) = lr_table->items(
        )->column_list_item( type = 'Navigation'  press = irclient->_event( val    = resultitabevent
                                                                            t_arg  = lt_arg ) ).

* ---------- Set cell content ---------------------------------------------------------------------
    LOOP AT lt_fieldprop_lis ASSIGNING <ls_fieldprop_lis>.
* ---------- Init loop data -----------------------------------------------------------------------
      CLEAR: lv_cell_fieldname.

* ---------- Build cell name ----------------------------------------------------------------------
      lv_cell_fieldname = `{` && <ls_fieldprop_lis>-fieldname && `}`.
      lr_item->cells( )->text( lv_cell_fieldname ).

    ENDLOOP.

    lr_grid_shlp = irparent->buttons( )->button(
          text  = closebuttontext
          press = irclient->_event_client( irclient->cs_event-popup_close ) ).


    result = lr_grid_shlp.

  ENDMETHOD.



ENDCLASS.
