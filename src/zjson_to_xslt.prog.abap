*&---------------------------------------------------------------------*
*& Report ZJSON_TO_XSLT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zjson_to_xslt.


PARAMETERS: p_type TYPE typename,
            p_numc TYPE xfeld AS CHECKBOX DEFAULT 'X',
            p_gens TYPE xfeld AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.

  DATA(out) = cl_demo_output=>new(
    )->begin_section( `Serialization` ).

  DATA(lv_xslt) = zcl_json_to_xslt=>get_instance( )->get_xslt_source_code( iv_type_name = p_type
                                                                           is_serialization_options = VALUE #( numc_as_number = p_numc ) ).

  IF p_gens = abap_true.
    zcl_json_to_xslt=>get_instance( )->delete_xslt_transformation( EXPORTING iv_transformation_name = p_type ).
    zcl_json_to_xslt=>get_instance( )->create_xslt_transformation( EXPORTING is_serialization_options  = VALUE #( numc_as_number = p_numc )
                                                                             iv_transformation_name = p_type
                                                                             iv_type_name = p_type ).
  ENDIF.

  out->write_xml( lv_xslt ).

  out->display( ).
