************************************************************************
*                             Co-IT                                    *
************************************************************************
* Programa:  ZHRSF013                                                  *
* Descrição: Atualizaçao de Fotos no RM                                *
* Autor    : Rodney G. Amancio                                         *
* Data     : 03/02/2017                                                *
************************************************************************
*                  Histórico das Modificações                          *
************************************************************************
*    Data    |      Nome       |       Descrição          |  Request   *
*----------------------------------------------------------------------*
* 03/02/2017 | Rodney Amancio  | Código Inicial           | PIDK900096 *
*----------------------------------------------------------------------*
REPORT zhrsfi013.

*----------------------------------------------------------------------*
* Include
*----------------------------------------------------------------------*
INCLUDE zhrsfi003_class_email.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TABLES: zhrt015_ctrl_hst.

TYPES: BEGIN OF y_fotos_rm,
        status        TYPE string,
        codigousuario TYPE char50,
        codcoligada   TYPE string,
        chapa         TYPE string,
        nomecompleto  TYPE string,
*        id            TYPE string,
*        codsistema    TYPE string,
*        imagem        TYPE xstring,
       END OF y_fotos_rm.

*----------------------------------------------------------------------*
* Tabelas Internas
*----------------------------------------------------------------------*
DATA: t_fotos_rm TYPE STANDARD TABLE OF y_fotos_rm.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: v_cx_sql_exception TYPE REF TO cx_sql_exception.

*----------------------------------------------------------------------*
* Objetos
*----------------------------------------------------------------------*
DATA: o_sfsf_tool  TYPE REF TO zcoit_sfsf_tools,
      o_dbcon_tool TYPE REF TO zcoit_dbcon_tools,
      o_log        TYPE REF TO zsfi_log_compensation.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: c_tabela_rm          TYPE string VALUE 'RM.VW_FOTOS_RM_SF',
           c_sentido_integracao TYPE string VALUE 'RM->SF',
           c_e                  TYPE string VALUE 'Erro',
           c_s                  TYPE string VALUE 'Sucesso',
           c_i                  TYPE c      VALUE 'I'.

*----------------------------------------------------------------------*
* Tela de Seleçao
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_chapa FOR zhrt015_ctrl_hst-chapa MODIF ID rm MATCHCODE OBJECT zhrt_chapa.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Início
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CREATE OBJECT o_log.

  PERFORM f_get_rm.

  PERFORM f_get_sf.

  PERFORM f_send_sf.

END-OF-SELECTION.

  PERFORM f_finalizar_log.

*&---------------------------------------------------------------------*
*&      Form  F_GET_ADHOC
*&---------------------------------------------------------------------*
FORM f_get_adhoc .

  DATA: lv_query       TYPE string.

  PERFORM f_message_gui USING 'Selecionando ADHOC do SF' '1'.

ENDFORM.                    " F_GET_ADHOC

*&---------------------------------------------------------------------*
*&      Form  F_PUT_RM
*&---------------------------------------------------------------------*
FORM f_put_rm .
*
*  DATA: lw_lideres_sf   LIKE LINE OF t_lideres_sf,
*        lw_lideres_rm   LIKE LINE OF t_lideres_rm,
*        lv_len          TYPE i,
*        lv_lines        TYPE string,
*        lv_tabix        TYPE string,
*        lv_perc         TYPE i,
*        lv_message      TYPE string,
*        lv_count        LIKE p_count,
*        lv_conn         TYPE dbcon-con_name.
*
**  data: i type i.
**  WHILE i = 0.
**  ENDWHILE.
*
*  IF p_aux EQ abap_true.
**    lv_conn = 'RMPROJ'.
*    lv_conn = 'RMLABORE'.
*  ELSE.
*    lv_conn = 'RMLABORE'.
*  ENDIF.
*
*  TRY.
*      CREATE OBJECT o_dbcon_tool
*        EXPORTING
*          i_conn_name = lv_conn
*          i_log       = o_log.
*    CATCH cx_sql_exception INTO v_cx_sql_exception.
*      o_log->set_single_msg( i_status    = c_e
*                             i_objeto    = c_tabela_rm
*                             i_evento    = 'CONECTAR COM RM'
*                             i_descricao = 'Erro ao conectar com RM'
*                             i_sentido   = c_sentido_integracao  ).
*  ENDTRY.
*
*  CHECK NOT t_lideres_sf IS INITIAL.
*
*  PERFORM f_delete_rm.
*
*  DESCRIBE TABLE t_lideres_sf LINES lv_lines.
*
*  LOOP AT t_lideres_sf INTO lw_lideres_sf.
*
*    ADD 1 TO lv_count.
*
*    lv_tabix = sy-tabix.
*
*    lw_lideres_rm-cpf                       = lw_lideres_sf-userid.
*    lw_lideres_rm-codcoligada               = lw_lideres_sf-username(2).
*
*    lv_len = strlen( lw_lideres_sf-username ).
*    lv_len = lv_len - 5.
*    IF lv_len LT 0.
*      lv_len = 0.
*    ENDIF.
*    lw_lideres_rm-chapa                     = lw_lideres_sf-username+lv_len.
*
*    lw_lideres_rm-respgp                    = lw_lideres_sf-hrid.
*    lw_lideres_rm-liderimediato             = lw_lideres_sf-managerid.
*    lw_lideres_rm-nomeliderimediato         = lw_lideres_sf-manager_firstname.
*    lw_lideres_rm-emailliderimediato        = lw_lideres_sf-manager_email.
*    lw_lideres_rm-usuarioliderimediato      = lw_lideres_sf-manager_username.
*    lw_lideres_rm-codfuncaoliderimediato    = lw_lideres_sf-manager_jobcode.
*    lw_lideres_rm-usuariogerliderimediato   = lw_lideres_sf-managers_manager_username.
*    lw_lideres_rm-nomegerliderimediato      = lw_lideres_sf-managers_manager_firstname.
*    lw_lideres_rm-codfuncaogerliderimediato = lw_lideres_sf-managers_manager_jobcode.
*    lw_lideres_rm-gestorgerimediato         = lw_lideres_sf-managers_manager_managerid.
*    APPEND lw_lideres_rm TO t_lideres_rm.
*
*    IF lw_lideres_rm-chapa IN p_chapa.
*
*      TRY.
*
*          CONCATENATE 'Inserindo Registro'
*                      lv_tabix
*                      'de'
*                      lv_lines
*                 INTO lv_message
*            SEPARATED BY space.
*          lv_perc = ( lv_tabix / lv_lines ) * 100.
*          PERFORM f_message_gui USING lv_message lv_perc.
*
*          IF p_prd EQ abap_true.
*            o_dbcon_tool->execute_insert( EXPORTING i_param = t_lideres_rm i_tabname = c_tabela_rm i_log_detalhado = p_log ).
*          ELSE.
*            o_dbcon_tool->execute_insert( EXPORTING i_param = t_lideres_rm i_tabname = c_tabela_rm_aux i_log_detalhado = p_log ).
*          ENDIF.
*
*          IF lv_count EQ p_count.
*            o_dbcon_tool->commit( ).
*          ENDIF.
*
*          IF p_log EQ abap_true.
*            o_log->set_single_msg( i_status    = c_s
*                                   i_objeto    = c_tabela_rm
*                                   i_evento    = 'INSERT TABELA RM'
*                                   i_descricao = 'Registro inserido no RM'
*                                   i_sentido   = c_sentido_integracao  ).
*          ENDIF.
*
*        CATCH cx_sql_exception INTO v_cx_sql_exception.
*          o_log->set_single_msg( i_status    = c_e
*                                 i_objeto    = c_tabela_rm
*                                 i_evento    = 'INSERT TABELA RM'
*                                 i_descricao = 'Erro ao inserir dados dno RM'
*                                 i_sentido   = c_sentido_integracao  ).
*      ENDTRY.
*
*    ENDIF.
*
*    CLEAR: t_lideres_rm, lw_lideres_rm.
*
*  ENDLOOP.
*
*  TRY.
*      o_dbcon_tool->commit( ).
*      o_log->set_single_msg( i_status    = c_s
*                             i_objeto    = c_tabela_rm
*                             i_evento    = 'COMMIT RM'
*                             i_descricao = 'Commit efetuado com Sucesso no RM'
*                             i_sentido   = c_sentido_integracao  ).
*
*      IF NOT p_file IS INITIAL.
**        DELETE DATASET p_file.
*      ENDIF.
*
*    CATCH cx_sql_exception INTO v_cx_sql_exception.
*      o_log->set_single_msg( i_status    = c_e
*                             i_objeto    = c_tabela_rm
*                             i_evento    = 'COMMIT RM'
*                             i_descricao = 'Erro ao efetuar Commit no RM'
*                             i_sentido   = c_sentido_integracao  ).
*  ENDTRY.

ENDFORM.                    " F_PUT_RM

*&---------------------------------------------------------------------*
*&      Form  F_DELETE_RM
*&---------------------------------------------------------------------*
FORM f_delete_rm .
*
*  TRY.
*
*      IF p_prd EQ abap_true.
*        o_dbcon_tool->execute_delete( i_tabname = c_tabela_rm
*                                    i_commit  = abap_true ).
*      ELSE.
*        o_dbcon_tool->execute_delete( i_tabname = c_tabela_rm_aux
*                                  i_commit  = abap_true ).
*      ENDIF.
*
*      o_log->set_single_msg( i_status    = c_s
*                             i_objeto    = c_tabela_rm
*                             i_evento    = 'DELETE TABELA RM'
*                             i_descricao = 'Dados deletados com Sucesso'
*                             i_sentido   = c_sentido_integracao  ).
*
*    CATCH cx_sql_exception INTO v_cx_sql_exception.
*
*      o_log->set_single_msg( i_status    = c_e
*                             i_objeto    = c_tabela_rm
*                             i_evento    = 'DELETE TABELA RM'
*                             i_descricao = 'Erro ao deletar dados do RM'
*                             i_sentido   = c_sentido_integracao  ).
*
*      o_log->set_single_msg( i_status    = c_e
*                             i_objeto    = c_tabela_rm
*                             i_evento    = 'DELETE TABELA RM'
*                             i_descricao = v_cx_sql_exception->get_text( )
*                             i_sentido   = c_sentido_integracao  ).
*
*  ENDTRY.

ENDFORM.                    " F_DELETE_RM

*&---------------------------------------------------------------------*
*&      Form  F_FINALIZAR_LOG
*&---------------------------------------------------------------------*
FORM f_finalizar_log .

  DATA: lv_message TYPE string,
        lv_id      TYPE string.

  o_log->get_id( IMPORTING r_value = lv_id ).

  lv_message = 'Log de execuçao ->' && lv_id.

  o_log->finalizar( ).

  MESSAGE lv_message TYPE c_i.

*  IF sy-batch EQ abap_true.
  PERFORM f_send_email.
*  ENDIF.

ENDFORM.                    " F_FINALIZAR_LOG

*&---------------------------------------------------------------------*
*&      Form  f_message_gui
*&---------------------------------------------------------------------*
FORM f_message_gui USING i_message i_perc.

  DATA: lv_perc TYPE string.

  IF sy-batch IS INITIAL.

    IF i_perc EQ 0.
      lv_perc = 10.
    ELSE.
      lv_perc = i_perc.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_perc
        text       = i_message.

  ELSE.

    MESSAGE i_message TYPE c_i.
*    WRITE: / i_message.

  ENDIF.

ENDFORM.                    "f_message_gui

*&---------------------------------------------------------------------*
*&      Form  f_send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_send_email.

  TYPES: BEGIN OF y_body,
          msg     TYPE string,
         END OF y_body.

  DATA: lt_config TYPE TABLE OF zhrt002_config,
        lt_email  TYPE TABLE OF string.

  DATA: lw_config TYPE zhrt002_config,
        lw_email  TYPE string,
        lt_subj   TYPE string,
        lt_body   TYPE TABLE OF y_body,
        lw_msg    TYPE y_body,
        lv_lines  TYPE i.

  DATA: lo_email TYPE REF TO lc_email.

  CREATE OBJECT lo_email.

  SELECT *
  INTO TABLE lt_config
  FROM zhrt002_config
  WHERE chave EQ 'EMAIL'
    AND sysid EQ sy-sysid.

  LOOP AT lt_config INTO lw_config.

    SPLIT lw_config-valor AT ';' INTO TABLE lt_email.

    LOOP AT lt_email INTO lw_email.
      lo_email->add_destinatario( lw_email ).
    ENDLOOP.

    CLEAR: lw_email, lt_email[].

  ENDLOOP.

  lw_msg-msg = 'Atualizaçao de Hierarquia efetuada com Sucesso'.
  APPEND lw_msg TO lt_body.

  DESCRIBE TABLE t_fotos_rm LINES lv_lines.
  lw_msg-msg = 'Total de Registros Atualizados ->' && lv_lines.
  APPEND lw_msg TO lt_body.

  lt_subj = 'Integração SF X RM - Atualização de Hierarquia.'.
  lo_email->set_assunto( lt_subj ).
  lo_email->set_body_by_table( EXPORTING it_table = lt_body i_color = '#b20000' ).

  lo_email->send_email(  ).

ENDFORM.                    "f_send_email

*&---------------------------------------------------------------------*
*&      Form  F_UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM f_upload_file .
*
*  DATA: lv_query       TYPE string,
*        lw_string      TYPE string,
*        lv_offset      TYPE i,
*        lv_len         TYPE i,
*        lw_text        TYPE string,
*        lt_text        TYPE TABLE OF string,
*        lv_msg         TYPE string,
*        lv_lines       TYPE i,
*        lw_lideres_sf  LIKE LINE OF t_lideres_sf,
*        lv_file        TYPE string,
*        lt_tab         TYPE TABLE OF string.
*
*  FIELD-SYMBOLS: <fs_field> TYPE any.
*
*  PERFORM f_message_gui USING 'Upload do Arquivo ADHOC do SF' '1'.
*
*  IF p_file(1) NE '/'.
*
*    lv_file = p_file.
*    CALL FUNCTION 'GUI_UPLOAD'
*      EXPORTING
*        filename = lv_file
*      TABLES
*        data_tab = lt_tab.
*
*    LOOP AT lt_tab INTO lw_string.
*
*      FIND FIRST OCCURRENCE OF '"' IN lw_string MATCH OFFSET lv_offset.
*      ADD 1 TO lv_offset.
*
*      lv_len = strlen( lw_string ).
*      lv_len = lv_len - ( lv_offset + 2 ).
*
*      IF lv_len LE 0.
*        CONTINUE.
*      ENDIF.
*
*      SPLIT lw_string+lv_offset(lv_len) AT '","' INTO TABLE lt_text.
*
*      LOOP AT lt_text INTO lw_text.
*        ASSIGN COMPONENT sy-tabix OF STRUCTURE lw_lideres_sf TO <fs_field>.
*        <fs_field> = lw_text.
*      ENDLOOP.
*
*      APPEND lw_lideres_sf TO t_lideres_sf.
*
*    ENDLOOP.
*
*    DELETE t_lideres_sf INDEX 1.
*    DELETE t_lideres_sf WHERE managerid IS INITIAL.
*
*  ELSE.
*
*    OPEN DATASET p_file FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.
*
*    IF sy-subrc EQ 0.
*
*      DO.
*        READ DATASET p_file INTO lw_string.
*        IF sy-subrc NE 0.
*          EXIT.
*        ELSE.
*
*          FIND FIRST OCCURRENCE OF '"' IN lw_string MATCH OFFSET lv_offset.
*          ADD 1 TO lv_offset.
*
*          lv_len = strlen( lw_string ).
*          lv_len = lv_len - ( lv_offset + 2 ).
*
*          IF lv_len LE 0.
*            CONTINUE.
*          ENDIF.
*
*          SPLIT lw_string+lv_offset(lv_len) AT '","' INTO TABLE lt_text.
*
*          LOOP AT lt_text INTO lw_text.
*            ASSIGN COMPONENT sy-tabix OF STRUCTURE lw_lideres_sf TO <fs_field>.
*            <fs_field> = lw_text.
*          ENDLOOP.
*
*          APPEND lw_lideres_sf TO t_lideres_sf.
*
*        ENDIF.
*      ENDDO.
*
*      DELETE t_lideres_sf INDEX 1.
*      DELETE t_lideres_sf WHERE managerid IS INITIAL.
*
**      DELETE DATASET p_file.
*
*      o_log->set_single_msg( i_status    = c_s
*                            i_objeto    = 'LEITURA ADHOC'
*                            i_evento    = 'UPLOAD Arquivo CSV'
*                            i_descricao = 'Arquivo Lido com Sucesso'
*                            i_sentido   = c_sentido_integracao  ).
*
*      o_log->set_single_msg( i_status    = c_s
*                         i_objeto    = 'LEITURA ADHOC'
*                         i_evento    = 'UPLOAD Arquivo CSV'
*                         i_descricao = p_file
*                         i_sentido   = c_sentido_integracao  ).
*
*      DESCRIBE TABLE t_lideres_sf LINES lv_lines.
*      lv_msg = 'Registros lidos =>' && lv_lines.
*      o_log->set_single_msg( i_status    = c_s
*                         i_objeto    = 'LEITURA ADHOC'
*                         i_evento    = 'UPLOAD Arquivo CSV'
*                         i_descricao = lv_msg
*                         i_sentido   = c_sentido_integracao  ).
*
*    ELSE.
*
*      o_log->set_single_msg( i_status    = c_e
*                             i_objeto    = 'LEITURA ADHOC'
*                             i_evento    = 'UPLOAD Arquivo CSV'
*                             i_descricao = 'Erro ao Abrir arquivo do Servidor'
*                             i_sentido   = c_sentido_integracao  ).
*
*      o_log->set_single_msg( i_status    = c_e
*                             i_objeto    = 'LEITURA ADHOC'
*                             i_evento    = 'UPLOAD Arquivo CSV'
*                             i_descricao = p_file
*                             i_sentido   = c_sentido_integracao  ).
*
*    ENDIF.
*
*  ENDIF.

ENDFORM.                    " F_UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  F_GET_RM
*&---------------------------------------------------------------------*
FORM f_get_rm .

  DATA: lv_conn         TYPE dbcon-con_name.

  PERFORM f_message_gui USING 'Selecionando Dados do RM' '1'.

  lv_conn = 'RMLABORE'.

  CREATE OBJECT o_dbcon_tool
    EXPORTING
      i_conn_name = lv_conn
      i_log       = o_log.

  o_dbcon_tool->execute_select( EXPORTING i_view    = c_tabela_rm
                                IMPORTING et_result = t_fotos_rm ).

  DELETE t_fotos_rm WHERE codigousuario NOT IN p_chapa.

ENDFORM.                    " F_GET_RM

*&---------------------------------------------------------------------*
*&      Form  F_SEND_SF
*&---------------------------------------------------------------------*
FORM f_send_sf .

  DATA: lo_upsert_user_photo  TYPE REF TO zsfi_co_si_upsert_user_photo_s,
        lo_catch              TYPE REF TO cx_root,
        lo_catch_sfi          TYPE REF TO zsfi_cx_sfweb_service_fault_ex,
        lo_sfsf_tool          TYPE REF TO zcoit_sfsf_tools,
        lo_log                TYPE REF TO zsfi_log_compensation.

  DATA: lt_sfobject       TYPE TABLE OF zsfi_sfobject_photos.

  DATA: ls_sfobject       LIKE LINE OF lt_sfobject,
        ls_request        TYPE zsfi_user_photo_source,
        ls_response       TYPE zsfi_upsert_response1,
        ls_result         TYPE zsfi_object_edit_result,
        ls_fotos_rm       LIKE LINE OF t_fotos_rm.

  DATA: lv_msgty        TYPE bal_s_msg-msgty,
        lv_total_lines  TYPE sy-tabix,
        lv_append_lines TYPE sy-tabix,
        lv_sessionid    TYPE string,
        lv_tabix        TYPE sy-tabix,
        lv_msg          TYPE string,
        lv_sem_imagem   TYPE string.

  zcoit_employee_photo=>get_photo( EXPORTING i_coligada = '99'
                                             i_chapa    = '999999'
                                             i_base64   = abap_true
                                   IMPORTING e_result   = lv_sem_imagem ).

  CREATE OBJECT lo_sfsf_tool
    EXPORTING
      i_log = lo_log.

  CREATE OBJECT lo_upsert_user_photo.

  lv_total_lines = lines( t_fotos_rm ).

  LOOP AT t_fotos_rm INTO ls_fotos_rm.

    PERFORM f_message_gui USING 'Resgatando Fotos' '1'.

    zcoit_employee_photo=>get_photo( EXPORTING i_coligada = ls_fotos_rm-codcoligada
                                               i_chapa    = ls_fotos_rm-chapa
                                               i_base64   = abap_true
                                     IMPORTING e_result   = ls_sfobject-data ).

    IF ls_sfobject-data NE lv_sem_imagem.
      ls_sfobject-user_external_id  = ls_fotos_rm-codigousuario.
      ls_sfobject-type              = 'UserPhotoSource'.
      APPEND ls_sfobject TO lt_sfobject.
      CLEAR ls_sfobject.

      ADD 1 TO lv_append_lines.
    ENDIF.

    IF lines( lt_sfobject ) EQ '10' OR
       lv_append_lines      EQ lv_total_lines.

      ls_request-user_photo_source-session_id      = lo_sfsf_tool->get_sessionid( ).
      ls_request-user_photo_source-upsert-type     = 'UserPhotoSource'.
      ls_request-user_photo_source-upsert-sfobject = lt_sfobject.

      TRY.

          PERFORM f_message_gui USING 'Salvando no SF' '1'.

          CALL METHOD lo_upsert_user_photo->si_upsert_user_photo_source
            EXPORTING
              output = ls_request
            IMPORTING
              input  = ls_response.

*          WAIT UP TO 5 SECONDS.

        CATCH cx_ai_system_fault INTO lo_catch.
          lv_msg = 'Erro ao salvar no SF->' && lo_catch->get_text( ).
          o_log->set_single_msg( i_status    = c_e
                   i_objeto    = 'SF'
                   i_evento    = 'SALVAR SF'
                   i_descricao = lv_msg
                   i_sentido   = c_sentido_integracao  ).

        CATCH zsfi_cx_sfweb_service_fault_ex INTO lo_catch_sfi.
          lv_msg = 'Erro ao salvar no SF->' && lo_catch_sfi->get_text( ).
          o_log->set_single_msg( i_status    = c_e
                   i_objeto    = 'SF'
                   i_evento    = 'SALVAR SF'
                   i_descricao = lv_msg
                   i_sentido   = c_sentido_integracao  ).

        CATCH cx_ai_application_fault INTO lo_catch.
          lv_msg = 'Erro ao salvar no SF->' && lo_catch->get_text( ).
          o_log->set_single_msg( i_status    = c_e
                   i_objeto    = 'SF'
                   i_evento    = 'SALVAR SF'
                   i_descricao = lv_msg
                   i_sentido   = c_sentido_integracao  ).

      ENDTRY.

      CLEAR: lt_sfobject[].

    ENDIF.

  ENDLOOP.

  PERFORM f_message_gui USING 'Registros atualizados com Sucesso' '1'.

  DATA: l_msg TYPE string.
  l_msg = 'Qtd. Registros ->' && lv_total_lines.

  PERFORM f_message_gui USING l_msg '1'.

ENDFORM.                    " F_SEND_SF

**&---------------------------------------------------------------------*
**&      Form  F_DOWNLOAD_FILE
**&---------------------------------------------------------------------*
*FORM f_download_file .
*
*  DATA: t_foto TYPE TABLE OF rsrawstring.
*
*  DATA: ls_fotos_rm LIKE LINE OF t_fotos_rm.
*
*  LOOP AT t_fotos_rm INTO ls_fotos_rm.
*    APPEND ls_fotos_rm-imagem TO t_foto.
*    EXIT.
*  ENDLOOP.
*
*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
**   BIN_FILESIZE                    =
*      filename                        = 'C:\Users\Admin\Desktop\testeccr.jpg'
*     filetype                        = 'ASC'
**   APPEND                          = ' '
**   WRITE_FIELD_SEPARATOR           = ' '
**   HEADER                          = '00'
**   TRUNC_TRAILING_BLANKS           = ' '
**   WRITE_LF                        = 'X'
**   COL_SELECT                      = ' '
**   COL_SELECT_MASK                 = ' '
**   DAT_MODE                        = ' '
**   CONFIRM_OVERWRITE               = ' '
**   NO_AUTH_CHECK                   = ' '
**   CODEPAGE                        = ' '
**   IGNORE_CERR                     = ABAP_TRUE
**   REPLACEMENT                     = '#'
**   WRITE_BOM                       = ' '
**   TRUNC_TRAILING_BLANKS_EOL       = 'X'
**   WK1_N_FORMAT                    = ' '
**   WK1_N_SIZE                      = ' '
**   WK1_T_FORMAT                    = ' '
**   WK1_T_SIZE                      = ' '
**   WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
**   SHOW_TRANSFER_STATUS            = ABAP_TRUE
**   VIRUS_SCAN_PROFILE              = '/SCET/GUI_DOWNLOAD'
** IMPORTING
**   FILELENGTH                      =
*    TABLES
*      data_tab                        = t_foto
**   FIELDNAMES                      =
** EXCEPTIONS
**   FILE_WRITE_ERROR                = 1
**   NO_BATCH                        = 2
**   GUI_REFUSE_FILETRANSFER         = 3
**   INVALID_TYPE                    = 4
**   NO_AUTHORITY                    = 5
**   UNKNOWN_ERROR                   = 6
**   HEADER_NOT_ALLOWED              = 7
**   SEPARATOR_NOT_ALLOWED           = 8
**   FILESIZE_NOT_ALLOWED            = 9
**   HEADER_TOO_LONG                 = 10
**   DP_ERROR_CREATE                 = 11
**   DP_ERROR_SEND                   = 12
**   DP_ERROR_WRITE                  = 13
**   UNKNOWN_DP_ERROR                = 14
**   ACCESS_DENIED                   = 15
**   DP_OUT_OF_MEMORY                = 16
**   DISK_FULL                       = 17
**   DP_TIMEOUT                      = 18
**   FILE_NOT_FOUND                  = 19
**   DATAPROVIDER_EXCEPTION          = 20
**   CONTROL_FLUSH_ERROR             = 21
*            .
*
*
*
*ENDFORM.                    " F_DOWNLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  F_GET_SF
*&---------------------------------------------------------------------*
FORM f_get_sf .

  DATA: lo_upsert_user_photo  TYPE REF TO zsfi_co_si_upsert_user_photo_s,
        lo_catch              TYPE REF TO cx_root,
        lo_catch_sfi          TYPE REF TO zsfi_cx_sfweb_service_fault_ex,
        lo_sfsf_tool          TYPE REF TO zcoit_sfsf_tools,
        lo_log                TYPE REF TO zsfi_log_compensation.

  DATA: lo_query          TYPE REF TO zsf1_co_si_query_user_photo_so, "*********
        lo_request        TYPE zsfi_query_request1,
        lo_response       TYPE zsf1_query_user_photo_source_1.

  DATA: lt_sfobject       TYPE TABLE OF zsf1_sfobject_user_photo_sourc, "*********
        ls_sfobject       LIKE LINE OF lt_sfobject.

  DATA: l_select TYPE string.

  PERFORM f_message_gui USING 'Lendo dados do SF' '1'.

  l_select =  'select userexternalid, data from userphoto where photoType = ' && text-003 && 'liveProfile' && text-003 && ' and userexternalid in ('.

  DATA: w_fotos_rm       LIKE LINE OF t_fotos_rm,
        l_query          TYPE string,
        lv_total_lines   TYPE sy-tabix,
        lv_append_lines  TYPE sy-tabix,
        lv_count         TYPE sy-tabix,
        l_userexternalid TYPE string,
        l_tabix          TYPE sy-tabix.

  CREATE OBJECT lo_sfsf_tool
    EXPORTING
      i_log = lo_log.

  lv_total_lines = lines( t_fotos_rm ).

  LOOP AT t_fotos_rm INTO w_fotos_rm.

    l_userexternalid = l_userexternalid && ',' && text-003 && w_fotos_rm-codigousuario && text-003.

    ADD 1 TO lv_count.
    ADD 1 TO lv_append_lines.

    IF lv_count             EQ lo_sfsf_tool->get_config_single( 'BATCHSIZE' ) OR
       lv_append_lines      EQ lv_total_lines.

      CLEAR l_query.
      l_query = l_select && l_userexternalid+1 && ')'.

      lo_request-query_request-session_id         = lo_sfsf_tool->get_sessionid( ).
      lo_request-query_request-query-query_string = l_query.

      CREATE OBJECT lo_query.

      TRY.

          CALL METHOD lo_query->si_query_user_photo_source
            EXPORTING
              output = lo_request
            IMPORTING
              input  = lo_response.

          APPEND LINES OF lo_response-query_user_photo_source_respon-query_response-result-sfobject TO lt_sfobject.

        CATCH cx_ai_system_fault INTO lo_catch.

        CATCH zsfi_cx_sfweb_service_fault_ex INTO lo_catch_sfi.
*        go_log->add_msg_from_string( EXPORTING i_msg   = lo_catch_sfi->standard-fault_text
*                                               i_class = '1'
*                                               i_msgty = 'A' ).

        CATCH cx_ai_application_fault INTO lo_catch.
*        go_log->add_msg_from_string( EXPORTING i_msg   = lo_catch->get_text( )
*                                       i_class = '1'
*                                       i_msgty = 'A' ).

      ENDTRY.

      CLEAR: lv_count, l_userexternalid.

    ENDIF.

  ENDLOOP.

  SORT lt_sfobject BY user_external_id.

  LOOP AT t_fotos_rm INTO w_fotos_rm.

    l_tabix = sy-tabix.

    READ TABLE lt_sfobject INTO ls_sfobject WITH KEY user_external_id = w_fotos_rm-codigousuario BINARY SEARCH.

    IF sy-subrc EQ 0.
      DELETE t_fotos_rm INDEX l_tabix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_GET_SF
