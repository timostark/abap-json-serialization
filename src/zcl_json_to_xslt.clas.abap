CLASS zcl_json_to_xslt DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_serialization_options,
        numc_as_number TYPE xfeld,
      END OF ts_serialization_options .

    METHODS get_xslt_source_code
      IMPORTING
        !iv_type_name             TYPE typename
        !is_serialization_options TYPE zcl_json_to_xslt=>ts_serialization_options
      RETURNING
        VALUE(rv_xslt)            TYPE string .

    METHODS create_xslt_transformation
      IMPORTING
                !iv_type_name             TYPE typename
                !iv_transformation_name   TYPE typename
                !iv_package_name          TYPE devclass OPTIONAL
                !is_serialization_options TYPE zcl_json_to_xslt=>ts_serialization_options
      RETURNING VALUE(rv_trfn_name)       TYPE typename.

    METHODS delete_xslt_transformation
      IMPORTING
        !iv_transformation_name TYPE typename.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_json_to_xslt .
  PROTECTED SECTION.

    DATA mv_xslt TYPE string.
    DATA ms_serialization_options TYPE ts_serialization_options.

    CLASS-DATA: mo_instance TYPE REF TO zcl_json_to_xslt.

    METHODS constructor .

    TYPES: BEGIN OF ts_buffer,
             parent_absolute_name TYPE typename,
             absolute_name        TYPE typename,
             rel_name             TYPE fieldname,
             rel_name_camel_case  TYPE string,
             last_entry           TYPE xfeld,
             xslt_type            TYPE string,
             xslt_format          TYPE string,
             data_index           TYPE int4,
             elem_descr           TYPE REF TO cl_abap_typedescr,
             elem_rel_type_name   TYPE typename,
           END OF ts_buffer,
           tt_buffer TYPE STANDARD TABLE OF ts_buffer WITH DEFAULT KEY.

    DATA mt_buffer TYPE tt_buffer .
  PRIVATE SECTION.

    DATA mv_absolute_name TYPE abap_abstypename .


    METHODS escape
      IMPORTING
                !iv_str TYPE clike
      EXPORTING !ev_str TYPE string.

    METHODS serialize_rec
      IMPORTING
        !iv_root       TYPE string
        !io_elem_descr TYPE REF TO cl_abap_typedescr.

    METHODS get_xslt_type
      IMPORTING
                !io_elem_descr TYPE REF TO cl_abap_typedescr
      RETURNING VALUE(rv_xslt) TYPE string.

    METHODS get_xslt_format
      IMPORTING
                !io_elem_descr TYPE REF TO cl_abap_typedescr
      RETURNING VALUE(rv_xslt) TYPE string.

    METHODS serialize_table
      IMPORTING
        !iv_root       TYPE string
        !io_elem_descr TYPE REF TO cl_abap_typedescr.

    METHODS serialize_structure
      IMPORTING
        !iv_root       TYPE string
        !io_elem_descr TYPE REF TO cl_abap_typedescr.

    METHODS to_camel_case
      IMPORTING
        !iv_name       TYPE string
      RETURNING
        VALUE(rv_name) TYPE string .

    METHODS load_structure_info
      IMPORTING
                !io_elem_descr   TYPE REF TO cl_abap_typedescr
      RETURNING VALUE(rt_buffer)
                  TYPE tt_buffer.
ENDCLASS.



CLASS ZCL_JSON_TO_XSLT IMPLEMENTATION.


  METHOD constructor.
  ENDMETHOD.


  METHOD create_xslt_transformation.
    DATA(lv_xslt) = get_xslt_source_code( iv_type_name = iv_type_name
                                          is_serialization_options = is_serialization_options ).

    DATA: lo_xslt TYPE REF TO cl_o2_api_xsltdesc,
          lv_name TYPE cxsltdesc.
    DATA: ls_attributes TYPE o2xsltattr.

    ls_attributes = VALUE #( devclass = iv_package_name
                             langu = sy-langu
                             author = sy-uname
                             changedby = sy-uname
                             changedon = sy-datum
                             createdon = sy-datum
                             descript = |Auto-Generated based on { mv_xslt }|
                             xsltdesc = iv_transformation_name ).


    cl_o2_api_xsltdesc=>create_new_from_string(
      EXPORTING
        p_source                = lv_xslt
        p_attr                  = ls_attributes
      IMPORTING
        p_obj                   = lo_xslt
      EXCEPTIONS
        action_cancelled        = 1
        error_occured           = 2
        not_authorized          = 3
        object_already_existing = 4
        undefined_name          = 5
        OTHERS                  = 6 ).
    IF sy-subrc <> 0.
      ASSERT 1 = 2.
    ENDIF.

    lo_xslt->save(
      EXCEPTIONS
        action_cancelled      = 1
        error_occured         = 2
        object_invalid        = 3
        object_not_changeable = 4
        permission_failure    = 5
        OTHERS                = 6 ).
    IF sy-subrc <> 0.
      lo_xslt->set_changeable( abap_false ). " unlock
      ASSERT 1 = 2.
    ENDIF.

    lo_xslt->activate(
      EXCEPTIONS
        generate_error    = 1
        storage_error     = 2
        syntax_errors     = 3
        OTHERS            = 5 ).
    IF sy-subrc <> 0.
      lo_xslt->set_changeable( abap_false ). " unlock
      ASSERT 1 = 2.
    ENDIF.

    lo_xslt->set_changeable( abap_false ).
  ENDMETHOD.


  METHOD delete_xslt_transformation.
    DATA: lo_xslt TYPE REF TO cl_o2_api_xsltdesc,
          lv_name TYPE cxsltdesc.


    lv_name = iv_transformation_name.

    cl_o2_api_xsltdesc=>load(
      EXPORTING
        p_xslt_desc        = lv_name
      IMPORTING
        p_obj              = lo_xslt
      EXCEPTIONS
        error_occured      = 1
        not_existing       = 2
        permission_failure = 3
        version_not_found  = 4
        OTHERS             = 5 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_xslt->set_changeable( abap_true ).
    lo_xslt->delete( ).
    lo_xslt->save( ).
  ENDMETHOD.


  METHOD escape.
    ev_str = CONV string( iv_str ).

    IF ev_str CA '\\"\r\n\t'.
      REPLACE ALL OCCURRENCES OF `\` IN ev_str WITH `\\`.
      REPLACE ALL OCCURRENCES OF `"` IN ev_str WITH `\"`.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN ev_str WITH `\r\n`.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN ev_str WITH `\n`.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN ev_str WITH `\t`.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    mo_instance = COND #( WHEN mo_instance IS BOUND THEN mo_instance ELSE NEW #( ) ).
    ro_instance = mo_instance.
  ENDMETHOD.


  METHOD get_xslt_format.
    rv_xslt = ||.

    IF ( io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_num OR
       io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_numeric ) AND
       ms_serialization_options-numc_as_number = abap_true.
      rv_xslt = |option="format(alpha)"|.
    ELSE.
      DATA(lv_rel_name) = io_elem_descr->get_relative_name( ).
      IF lv_rel_name = |XSDBOOLEAN| OR
         lv_rel_name = |XFELD|.
        rv_xslt = |option="format(boolean)"|.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_xslt_source_code.
    CLEAR: mv_xslt.

    ms_serialization_options = is_serialization_options.

    DATA(lo_elemdesc) = cl_abap_typedescr=>describe_by_name( iv_type_name ).
    DATA(lv_root_element) = |{ COND #( WHEN lo_elemdesc->type_kind = cl_abap_typedescr=>typekind_table THEN |array| ELSE |object| ) }|.


    mv_xslt = |<tt:transform template="tmpl1"| && cl_abap_char_utilities=>cr_lf &&
              |  xmlns:tt="http://www.sap.com/transformation-templates">| && cl_abap_char_utilities=>cr_lf &&
              |  <tt:root name="ROOT"/>| && cl_abap_char_utilities=>cr_lf &&
              |  <tt:template name="tmpl1">| && cl_abap_char_utilities=>cr_lf &&
              |    <{ lv_root_element }>| && cl_abap_char_utilities=>cr_lf.

    serialize_rec( EXPORTING io_elem_descr = lo_elemdesc
                             iv_root = |ROOT| ).

    mv_xslt = |{ mv_xslt }| &&
              |    </{ lv_root_element }>| && cl_abap_char_utilities=>cr_lf &&
              |  </tt:template>| && cl_abap_char_utilities=>cr_lf &&
              |</tt:transform>|   && cl_abap_char_utilities=>cr_lf.

    rv_xslt = mv_xslt.
  ENDMETHOD.


  METHOD get_xslt_type.
    rv_xslt = |str|.

    IF io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_int OR
           io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_int1 OR
           io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_int2 OR
           ( (  io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_num OR
                io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_numeric ) AND ms_serialization_options-numc_as_number = abap_true ) OR
           io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_int8.
      rv_xslt = |num|.
    ELSE.
      DATA(lv_rel_name) = io_elem_descr->get_relative_name( ).
      IF lv_rel_name = |XSDBOOLEAN| OR
         lv_rel_name = |XFELD|.
        rv_xslt = |bool|.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD load_structure_info.
    DATA(lv_rel_name_glbl) = io_elem_descr->get_relative_name( ).
    DATA(lt_view) = CAST cl_abap_structdescr( io_elem_descr )->get_included_view( ).
    DATA(lv_lines) = lines( lt_view ).

    LOOP AT lt_view ASSIGNING FIELD-SYMBOL(<ls_view>).
      DATA(lv_tabix) = sy-tabix.
      DATA(lv_last) = COND #( WHEN lv_lines = lv_tabix THEN abap_true ELSE abap_false ).

      DATA(lo_elem_val) = CAST cl_abap_typedescr( <ls_view>-type ).


      INSERT VALUE #(
          parent_absolute_name = mv_absolute_name
          absolute_name = lv_rel_name_glbl
          rel_name = <ls_view>-name
          rel_name_camel_case = to_camel_case( <ls_view>-name )
          data_index = lv_tabix
          last_entry = lv_last
          xslt_type = get_xslt_type( lo_elem_val )
          xslt_format = get_xslt_format( lo_elem_val )
          elem_descr = lo_elem_val ) INTO TABLE rt_buffer ASSIGNING FIELD-SYMBOL(<ls_add>).
    ENDLOOP.
  ENDMETHOD.


  METHOD serialize_rec.
    IF io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_table.
      serialize_table( io_elem_descr = io_elem_descr
                       iv_root = iv_root ).
      RETURN.
    ENDIF.

    IF io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_struct1 OR
       io_elem_descr->type_kind = cl_abap_elemdescr=>typekind_struct2.
      serialize_structure( io_elem_descr = io_elem_descr
                           iv_root = iv_root  ).

      RETURN.
    ENDIF.

    DATA(lv_xslt) = get_xslt_type( io_elem_descr ).
    mv_xslt = |{ mv_xslt }| &&
              |   <{ lv_xslt }>|  && cl_abap_char_utilities=>cr_lf &&
              |     <tt:value ref="{ iv_root }" { get_xslt_format( io_elem_descr ) }/>| && cl_abap_char_utilities=>cr_lf &&
              |   </{ lv_xslt }>|  && cl_abap_char_utilities=>cr_lf.
  ENDMETHOD.


  METHOD serialize_structure.
    DATA(lt_buffer) = load_structure_info( EXPORTING io_elem_descr = io_elem_descr  ).

    LOOP AT lt_buffer ASSIGNING FIELD-SYMBOL(<ls_view>).
      IF <ls_view>-elem_descr->type_kind = cl_abap_typedescr=>typekind_table.
        mv_xslt = |{ mv_xslt }| &&
                  |   <array name="{ <ls_view>-rel_name_camel_case }">|  && cl_abap_char_utilities=>cr_lf &&
                  |     <tt:loop ref="{ iv_root }.{ <ls_view>-rel_name }">| && cl_abap_char_utilities=>cr_lf.


        "child type..
        DATA(lo_child_elem) = CAST cl_abap_tabledescr( <ls_view>-elem_descr )->get_table_line_type( ).

        IF lo_child_elem->type_kind = cl_abap_typedescr=>typekind_struct1 OR
           lo_child_elem->type_kind = cl_abap_typedescr=>typekind_struct2.
          mv_xslt = |{ mv_xslt }| &&
                    |   <object>|  && cl_abap_char_utilities=>cr_lf.
        ELSEIF lo_child_elem->type_kind = cl_abap_typedescr=>typekind_table.
          mv_xslt = |{ mv_xslt }| &&
                    |   <array>|  && cl_abap_char_utilities=>cr_lf.
        ENDIF.

        serialize_rec( EXPORTING io_elem_descr = lo_child_elem
                                 iv_root = |$ref| ).

        IF lo_child_elem->type_kind = cl_abap_typedescr=>typekind_struct1 OR
           lo_child_elem->type_kind = cl_abap_typedescr=>typekind_struct2.
          mv_xslt = |{ mv_xslt }| &&
                    |   </object>|  && cl_abap_char_utilities=>cr_lf.
        ELSEIF lo_child_elem->type_kind = cl_abap_typedescr=>typekind_table.
          mv_xslt = |{ mv_xslt }| &&
                    |   </array>|  && cl_abap_char_utilities=>cr_lf.
        ENDIF.

        mv_xslt = |{ mv_xslt }| &&
                  |     </tt:loop>| && cl_abap_char_utilities=>cr_lf &&
                  |   </array>|  && cl_abap_char_utilities=>cr_lf.
      ELSEIF <ls_view>-elem_descr->type_kind = cl_abap_typedescr=>typekind_struct1 OR
             <ls_view>-elem_descr->type_kind = cl_abap_typedescr=>typekind_struct2.
        mv_xslt = |{ mv_xslt }| &&
                  |   <object name="{ <ls_view>-rel_name_camel_case }">|  && cl_abap_char_utilities=>cr_lf.

        serialize_rec( EXPORTING io_elem_descr = <ls_view>-elem_descr
                                 iv_root = |{ iv_root }.{ <ls_view>-rel_name }| ).

        mv_xslt = |{ mv_xslt }| &&
                  |   </object>|  && cl_abap_char_utilities=>cr_lf.
      ELSE.
        mv_xslt = |{ mv_xslt }| &&
                  |   <{ <ls_view>-xslt_type } name="{ <ls_view>-rel_name_camel_case }">|  && cl_abap_char_utilities=>cr_lf &&
                  |     <tt:value ref="{ iv_root }.{ <ls_view>-rel_name }" { <ls_view>-xslt_format }/>| && cl_abap_char_utilities=>cr_lf &&
                  |   </{ <ls_view>-xslt_type }>|  && cl_abap_char_utilities=>cr_lf.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD serialize_table.

    mv_xslt = |{ mv_xslt }| &&
              |     <tt:loop ref="{ iv_root }">| && cl_abap_char_utilities=>cr_lf.


    DATA(lo_linedesc) = CAST cl_abap_tabledescr( io_elem_descr )->get_table_line_type( ).

    IF lo_linedesc->type_kind = cl_abap_typedescr=>typekind_struct1 OR
       lo_linedesc->type_kind = cl_abap_typedescr=>typekind_struct2.
      mv_xslt = |{ mv_xslt }| &&
                |   <object>|  && cl_abap_char_utilities=>cr_lf.
    ENDIF.

    serialize_rec( EXPORTING io_elem_descr = lo_linedesc
                             iv_root = |$ref| ).

    IF lo_linedesc->type_kind = cl_abap_typedescr=>typekind_struct1 OR
       lo_linedesc->type_kind = cl_abap_typedescr=>typekind_struct2.
      mv_xslt = |{ mv_xslt }| &&
                |   </object>|  && cl_abap_char_utilities=>cr_lf.
    ENDIF.

    mv_xslt = |{ mv_xslt }| &&
              |     </tt:loop>|  && cl_abap_char_utilities=>cr_lf.
  ENDMETHOD.


  METHOD to_camel_case.
    DATA(lv_next_capital) = abap_false.
    DATA(lv_cnt) = strlen( iv_name ).
    DO lv_cnt TIMES.
      DATA(lv_idx) = sy-index - 1.
      DATA(lv_char) = iv_name+lv_idx(1).
      IF lv_char = '_'.
        lv_next_capital = abap_true.
        CONTINUE.
      ENDIF.

      rv_name = |{ rv_name }{ COND #( WHEN lv_next_capital = abap_false THEN to_lower( lv_char ) ELSE to_upper( lv_char ) ) }|.
      CLEAR: lv_next_capital.
    ENDDO.
  ENDMETHOD.
ENDCLASS.
