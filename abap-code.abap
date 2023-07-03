*&---------------------------------------------------------------------*
*& Include          ZCALL_API_2_V3
*&---------------------------------------------------------------------*
FORM send_flight_form.
  DATA:
    ssfctrlop TYPE ssfctrlop,
    ssfcompop TYPE ssfcompop,
    ssfcrescl TYPE ssfcrescl,
    fr_name   TYPE tdsfname,
    fu_name   TYPE rs38l_fnam.

  DATA:
    otf                 TYPE TABLE OF itcoo WITH HEADER LINE,
    lines               TYPE TABLE OF tline WITH HEADER LINE,
    lo_att_content_text TYPE soli_tab,
    content_in          TYPE TABLE OF tline WITH HEADER LINE,
    content_out         TYPE TABLE OF solisti1 WITH HEADER LINE.

  DATA:
        lt_data TYPE TABLE OF sflight.

  REFRESH lt_data.
  SELECT * FROM sflight INTO TABLE lt_data.

  ssfctrlop-no_dialog = 'X'.
  ssfcompop-tddelete  = 'X'.
  ssfcompop-tdimmed   = 'X'.
*  ssfcompop-tddest    = 'X'.
  ssfcompop-tdnoprint = 'X'.
*  ssfcompop-tdest     = 'PDF'.
  ssfcompop-tddest    = 'PDF'.
  ssfctrlop-getotf    = 'X'.
  ssfctrlop-preview   = 'X'.

  CALL FUNCTION '/1BCDWB/SF00000068'
    EXPORTING
*     ARCHIVE_INDEX      =
*     ARCHIVE_INDEX_TAB  =
*     archive_parameters =
      control_parameters = ssfctrlop
*     MAIL_APPL_OBJ      =
*     MAIL_RECIPIENT     =
*     MAIL_SENDER        =
      output_options     = ssfcompop
*     USER_SETTINGS      = 'X'
    IMPORTING
*     document_output_info =
      job_output_info    = ssfcrescl
*     job_output_options =
    TABLES
      lt_data            = lt_data
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  DATA: lc_fm_name      TYPE rs38l_fnam,
        ls_control_par  TYPE ssfctrlop,
        ls_job_output   TYPE ssfcrescl,
        lc_file         TYPE string,
        lt_lines        TYPE TABLE OF tline,
        li_pdf_fsize    TYPE i,
        ls_pdf_string_x TYPE xstring,
        ls_pdf          TYPE char80,
        lt_pdf          TYPE w3mimetabtype.

  REFRESH: otf, lines, lo_att_content_text.

  otf[] = ssfcrescl-otfdata[].

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = li_pdf_fsize
      bin_file              = ls_pdf_string_x
    TABLES
      otf                   = otf[]
      lines                 = lines[]
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = ls_pdf_string_x
    TABLES
      binary_tab = lt_pdf.

  TRY.
      DATA:
        lv_rc     TYPE i,
        lt_files  TYPE filetable,
        lv_action TYPE i.

      IF lines( otf[] ) > 0.
        DATA:
          lv_filesize TYPE w3param-cont_len,
          lv_filetype TYPE w3param-cont_type,
          lt_bin_data TYPE w3mimetabtype.

        DATA gv_string  TYPE xstring.
        DATA gv_filename  TYPE string.

        CLEAR gv_filename.
        gv_filename = |{ sy-datum }{ sy-uzeit }flight.pdf|.

        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_filesize
          IMPORTING
            buffer       = gv_string
          TABLES
            binary_tab   = lines[]
          EXCEPTIONS
            failed       = 1.

        IF sy-subrc <> 0.
          "hammer time
        ENDIF.
        "and part 2
        DATA: gv_base64 TYPE string.
        lv_filesize = xstrlen( gv_string ).
        CALL FUNCTION 'SSFC_BASE64_ENCODE'
          EXPORTING
            bindata = gv_string
            binleng = lv_filesize
          IMPORTING
            b64data = gv_base64.

        DATA(lv_bin_data) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_pdf ).
        DATA(lv_len) = xstrlen( lv_bin_data ).

      ENDIF.

      DATA : lv_status TYPE char50.
      REFRESH it_data.

      CALL METHOD cl_http_client=>create_by_url(
        EXPORTING
          url                = 'http://dms.toekangketik.com/api/s4/uploaddoc'   "API - URL
        IMPORTING
          client             = lo_http_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).

      lo_http_client->request->set_header_field( name = 'content-type'  value = 'multipart/form-data' ).
      lo_http_client->request->set_method( 'POST' ).
      lo_http_client->request->set_content_type( 'application/json' ).

      lo_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
      CALL METHOD lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).
      CALL METHOD lo_http_client->request->if_http_entity~set_content_type
        EXPORTING
          content_type = 'multipart/form-data'.

      CALL METHOD lo_http_client->request->if_http_entity~set_formfield_encoding
        EXPORTING
          formfield_encoding = cl_http_request=>if_http_entity~co_encoding_raw.

      DATA o_part TYPE REF TO if_http_entity.

      DATA(part_val) = |FORM-DATA;name="efile";filename="{ gv_filename }"|.

      o_part = lo_http_client->request->if_http_entity~add_multipart( ).
      CALL METHOD o_part->set_header_field
        EXPORTING
          name  = 'content-disposition'
          value = part_val.

      CALL METHOD o_part->set_content_type
        EXPORTING
          content_type = 'application/pdf'.

      o_part->set_data(
      data   = lv_bin_data
            offset = 0
            length = lv_len
            ).

      " Mengirim Request ke API
      lo_http_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state        = 2 ).

      " Menerima Response dari request
      lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state        = 2
        http_processing_failed    = 3 ).

      " Cek Response dari request.
      DATA lv_response TYPE string.
      lv_response = lo_http_client->response->get_cdata( ).

      cl_demo_output=>begin_section( `API Response`).
*      cl_demo_output=>write( data = json_data ).
      cl_demo_output=>write( data = lv_response ).
      cl_demo_output=>end_section( ).
      cl_demo_output=>display( ).
    CATCH cx_root INTO DATA(e_txt).
      WRITE: / e_txt->get_text( ).
  ENDTRY.
ENDFORM.
