# abap-json-serialization
Fastest possible JSON serialization

See blog post https://blogs.sap.com/2022/10/27/abap-fast-json-serialization/ for details.

Execute program ZJSON_TO_XSLT to generate specialized JSON XSLT transformations.
Execute the specificly created transformation using following sample code.

DATA(lo_writer_json) = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
CALL TRANSFORMATION ZSFLIGHT SOURCE root = lt_flights RESULT XML lo_writer_json.
DATA(lv_json) = cl_abap_codepage=>convert_from( lo_writer_json->get_output( ) ).