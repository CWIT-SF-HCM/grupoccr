************************************************************************
*                             Co-IT                                    *
************************************************************************
* Programa:  ZHRSF012                                                  *
* Descrição: Atualizaçao dos Lideres do SF no RM                       *
* Autor    : Rodney G. Amancio                                         *
* Data     : 03/02/2017                                                *
************************************************************************
*                  Histórico das Modificações                          *
************************************************************************
*    Data    |      Nome       |       Descrição          |  Request   *
*----------------------------------------------------------------------*
* 03/02/2017 | Rodney Amancio  | Código Inicial           | PIDK900096 *
*----------------------------------------------------------------------*
REPORT zhrsfi012.

*----------------------------------------------------------------------*
* Include
*----------------------------------------------------------------------*
INCLUDE zhrsfi003_class_email.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TABLES: zhrt015_ctrl_hst.

TYPES: BEGIN OF y_lideres_sf,
        userid                      TYPE string,
        username                    TYPE string,
*        title                       TYPE string,
        hrid                        TYPE string,
        managerid                   TYPE string,
        manager_firstname           TYPE string,
        manager_email               TYPE string,
        manager_username            TYPE string,
        manager_jobcode             TYPE string,
        managers_manager_username   TYPE string,
        managers_manager_firstname  TYPE string,
        managers_manager_managerid  TYPE string,
        managers_manager_jobcode    TYPE string,
       END OF y_lideres_sf,

       BEGIN OF y_lideres_rm,
        cpf                           TYPE string,
        codcoligada                   TYPE string,
        chapa                         TYPE string,
        respgp                        TYPE string,
        liderimediato                 TYPE string,
        nomeliderimediato             TYPE string,
        emailliderimediato            TYPE string,
        usuarioliderimediato          TYPE string,
        codfuncaoliderimediato        TYPE string,
        usuariogerliderimediato       TYPE string,
        nomegerliderimediato          TYPE string,
        codfuncaogerliderimediato     TYPE string,
        gestorgerimediato             TYPE string,
       END OF y_lideres_rm.

*----------------------------------------------------------------------*
* Tabelas Internas
*----------------------------------------------------------------------*
DATA: t_lideres_sf TYPE STANDARD TABLE OF y_lideres_sf,
      t_lideres_rm TYPE STANDARD TABLE OF y_lideres_rm.

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
CONSTANTS: c_tabela_rm          TYPE string VALUE 'RM.Z_LIDERES_SF',
           c_sentido_integracao TYPE string VALUE 'SF->RM',
           c_e                  TYPE string VALUE 'Erro',
           c_s                  TYPE string VALUE 'Sucesso',
           c_i                  TYPE c      VALUE 'I'.

*----------------------------------------------------------------------*
* Tela de Seleçao
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_chapa FOR zhrt015_ctrl_hst-chapa MODIF ID rm MATCHCODE OBJECT zhrt_chapa.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_file TYPE rlgrap-filename.
PARAMETERS: p_log AS CHECKBOX DEFAULT space.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Início
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CREATE OBJECT o_log.

  IF p_file IS INITIAL.
    PERFORM f_get_adhoc.
  ELSE.
    PERFORM f_upload_file.
  ENDIF.

  PERFORM f_put_rm.

END-OF-SELECTION.

  PERFORM f_finalizar_log.

*&---------------------------------------------------------------------*
*&      Form  F_GET_ADHOC
*&---------------------------------------------------------------------*
FORM f_get_adhoc .

  DATA: lv_query       TYPE string.

  PERFORM f_message_gui USING 'Selecionando ADHOC do SF' '1'.

  CREATE OBJECT o_sfsf_tool
    EXPORTING
      i_log = o_log.

  IF sy-sysid EQ 'PID'.
    CONCATENATE 'SELECT employee_USERS_SYS_ID,employee_USERS_SYS_USERNAME,employee_title_TITLE,employee_USERS_SYS_HRUID,employee_USERS_SYS_MANAGERUID,employee_manager_USERS_SYS_FIRSTNAME,'
                'employee_manager_USERS_SYS_EMAIL,employee_manager_USERS_SYS_USERNAME,employee_manager_USERS_SYS_JOBCODE,employee_managers_manager_USERS_SYS_USERNAME,employee_managers_manager_USERS_SYS_FIRSTNAME,'
                'employee_managers_manager_USERS_SYS_MANAGERUID,employee_managers_manager_USERS_SYS_JOBCODE FROM AdhocReport_14824'
           INTO lv_query.
  ELSE.
    CONCATENATE 'SELECT employee_USERS_SYS_ID,employee_USERS_SYS_USERNAME,employee_USERS_SYS_HRUID,employee_USERS_SYS_MANAGERUID,employee_manager_USERS_SYS_FIRSTNAME,'
                'employee_manager_USERS_SYS_EMAIL,employee_manager_USERS_SYS_USERNAME,employee_manager_USERS_SYS_JOBCODE,employee_managers_manager_USERS_SYS_USERNAME,employee_managers_manager_USERS_SYS_FIRSTNAME,'
                'employee_managers_manager_USERS_SYS_MANAGERUID,employee_managers_manager_USERS_SYS_JOBCODE FROMAdhocReport_38162'
           INTO lv_query.
  ENDIF.

  o_sfsf_tool->get_adhoc_from_sf( EXPORTING i_query = lv_query IMPORTING et_table = t_lideres_sf ).

  DELETE t_lideres_sf WHERE managerid IS INITIAL.

ENDFORM.                    " F_GET_ADHOC

*&---------------------------------------------------------------------*
*&      Form  F_PUT_RM
*&---------------------------------------------------------------------*
FORM f_put_rm .

  DATA: lw_lideres_sf   LIKE LINE OF t_lideres_sf,
        lw_lideres_rm   LIKE LINE OF t_lideres_rm,
        lv_len          TYPE i,
        lv_lines        TYPE string,
        lv_tabix        TYPE string,
        lv_perc         TYPE i,
        lv_message      TYPE string.

  TRY.
      CREATE OBJECT o_dbcon_tool
        EXPORTING
          i_log = o_log.
    CATCH cx_sql_exception INTO v_cx_sql_exception.
      o_log->set_single_msg( i_status    = c_e
                             i_objeto    = c_tabela_rm
                             i_evento    = 'CONECTAR COM RM'
                             i_descricao = 'Erro ao conectar com RM'
                             i_sentido   = c_sentido_integracao  ).
  ENDTRY.

  CHECK NOT t_lideres_sf IS INITIAL.

  PERFORM f_delete_rm.

  DESCRIBE TABLE t_lideres_sf LINES lv_lines.

  LOOP AT t_lideres_sf INTO lw_lideres_sf.

    lv_tabix = sy-tabix.

    lw_lideres_rm-cpf                       = lw_lideres_sf-userid.
    lw_lideres_rm-codcoligada               = lw_lideres_sf-username(2).

    lv_len = strlen( lw_lideres_sf-username ).
    lv_len = lv_len - 5.
    IF lv_len LT 0.
      lv_len = 0.
    ENDIF.
    lw_lideres_rm-chapa                     = lw_lideres_sf-username+lv_len.

    lw_lideres_rm-respgp                    = lw_lideres_sf-hrid.
    lw_lideres_rm-liderimediato             = lw_lideres_sf-managerid.
    lw_lideres_rm-nomeliderimediato         = lw_lideres_sf-manager_firstname.
    lw_lideres_rm-emailliderimediato        = lw_lideres_sf-manager_email.
    lw_lideres_rm-usuarioliderimediato      = lw_lideres_sf-manager_username.
    lw_lideres_rm-codfuncaoliderimediato    = lw_lideres_sf-manager_jobcode.
    lw_lideres_rm-usuariogerliderimediato   = lw_lideres_sf-managers_manager_username.
    lw_lideres_rm-nomegerliderimediato      = lw_lideres_sf-managers_manager_firstname.
    lw_lideres_rm-codfuncaogerliderimediato = lw_lideres_sf-managers_manager_jobcode.
    lw_lideres_rm-gestorgerimediato         = lw_lideres_sf-managers_manager_managerid.
    APPEND lw_lideres_rm TO t_lideres_rm.

    IF lw_lideres_rm-chapa IN p_chapa.

      TRY.

          CONCATENATE 'Inserindo Registro'
                      lv_tabix
                      'de'
                      lv_lines
                 INTO lv_message
            SEPARATED BY space.
          lv_perc = ( lv_tabix / lv_lines ) * 100.
          PERFORM f_message_gui USING lv_message lv_perc.

          o_dbcon_tool->execute_insert( EXPORTING i_param = t_lideres_rm i_tabname = c_tabela_rm i_log_detalhado = p_log ).

          IF p_log EQ abap_true.
            o_log->set_single_msg( i_status    = c_s
                                   i_objeto    = c_tabela_rm
                                   i_evento    = 'INSERT TABELA RM'
                                   i_descricao = 'Registro inserido no RM'
                                   i_sentido   = c_sentido_integracao  ).
          ENDIF.

        CATCH cx_sql_exception INTO v_cx_sql_exception.
          o_log->set_single_msg( i_status    = c_e
                                 i_objeto    = c_tabela_rm
                                 i_evento    = 'INSERT TABELA RM'
                                 i_descricao = 'Erro ao inserir dados dno RM'
                                 i_sentido   = c_sentido_integracao  ).
      ENDTRY.

    ENDIF.

    CLEAR: t_lideres_rm, lw_lideres_rm.

  ENDLOOP.

  TRY.
      o_dbcon_tool->commit( ).
      o_log->set_single_msg( i_status    = c_s
                             i_objeto    = c_tabela_rm
                             i_evento    = 'COMMIT RM'
                             i_descricao = 'Commit efetuado com Sucesso no RM'
                             i_sentido   = c_sentido_integracao  ).

    CATCH cx_sql_exception INTO v_cx_sql_exception.
      o_log->set_single_msg( i_status    = c_e
                             i_objeto    = c_tabela_rm
                             i_evento    = 'COMMIT RM'
                             i_descricao = 'Erro ao efetuar Commit no RM'
                             i_sentido   = c_sentido_integracao  ).
  ENDTRY.

ENDFORM.                    " F_PUT_RM

*&---------------------------------------------------------------------*
*&      Form  F_DELETE_RM
*&---------------------------------------------------------------------*
FORM f_delete_rm .

  TRY.

      o_dbcon_tool->execute_delete( i_tabname = c_tabela_rm
                                  i_commit  = abap_true ).

      o_log->set_single_msg( i_status    = c_s
                             i_objeto    = c_tabela_rm
                             i_evento    = 'DELETE TABELA RM'
                             i_descricao = 'Dados deletados com Sucesso'
                             i_sentido   = c_sentido_integracao  ).

    CATCH cx_sql_exception INTO v_cx_sql_exception.

      o_log->set_single_msg( i_status    = c_e
                             i_objeto    = c_tabela_rm
                             i_evento    = 'DELETE TABELA RM'
                             i_descricao = 'Erro ao deletar dados do RM'
                             i_sentido   = c_sentido_integracao  ).

      o_log->set_single_msg( i_status    = c_e
                             i_objeto    = c_tabela_rm
                             i_evento    = 'DELETE TABELA RM'
                             i_descricao = v_cx_sql_exception->get_text( )
                             i_sentido   = c_sentido_integracao  ).

  ENDTRY.

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

  PERFORM f_send_email.

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

  DESCRIBE TABLE t_lideres_sf LINES lv_lines.
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

  DATA: lv_query       TYPE string,
        lw_string      TYPE string,
        lv_offset      TYPE i,
        lv_len         TYPE i,
        lw_text        TYPE string,
        lt_text        TYPE TABLE OF string,
        lv_msg         TYPE string,
        lv_lines       TYPE i,
        lw_lideres_sf  LIKE LINE OF t_lideres_sf.

  FIELD-SYMBOLS: <fs_field> TYPE any.

  PERFORM f_message_gui USING 'Upload do Arquivo ADHOC do SF' '1'.

  OPEN DATASET p_file FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.

  IF sy-subrc EQ 0.

    DO.
      READ DATASET p_file INTO lw_string.
      IF sy-subrc NE 0.
        EXIT.
      ELSE.

        FIND FIRST OCCURRENCE OF '"' IN lw_string MATCH OFFSET lv_offset.
        ADD 1 TO lv_offset.

        lv_len = strlen( lw_string ).
        lv_len = lv_len - ( lv_offset + 2 ).

        IF lv_len LE 0.
          CONTINUE.
        ENDIF.

        SPLIT lw_string+lv_offset(lv_len) AT '","' INTO TABLE lt_text.

        LOOP AT lt_text INTO lw_text.
          ASSIGN COMPONENT sy-tabix OF STRUCTURE lw_lideres_sf TO <fs_field>.
          <fs_field> = lw_text.
        ENDLOOP.

        APPEND lw_lideres_sf TO t_lideres_sf.

      ENDIF.
    ENDDO.

    DELETE t_lideres_sf INDEX 1.
    DELETE t_lideres_sf WHERE managerid IS INITIAL.

    DELETE DATASET p_file.

    o_log->set_single_msg( i_status    = c_s
                          i_objeto    = 'LEITURA ADHOC'
                          i_evento    = 'UPLOAD Arquivo CSV'
                          i_descricao = 'Arquivo Lido com Sucesso'
                          i_sentido   = c_sentido_integracao  ).

    o_log->set_single_msg( i_status    = c_s
                       i_objeto    = 'LEITURA ADHOC'
                       i_evento    = 'UPLOAD Arquivo CSV'
                       i_descricao = p_file
                       i_sentido   = c_sentido_integracao  ).

    DESCRIBE TABLE t_lideres_sf LINES lv_lines.
    lv_msg = 'Registros lidos =>' && lv_lines.
    o_log->set_single_msg( i_status    = c_s
                       i_objeto    = 'LEITURA ADHOC'
                       i_evento    = 'UPLOAD Arquivo CSV'
                       i_descricao = lv_msg
                       i_sentido   = c_sentido_integracao  ).

  ELSE.

    o_log->set_single_msg( i_status    = c_e
                           i_objeto    = 'LEITURA ADHOC'
                           i_evento    = 'UPLOAD Arquivo CSV'
                           i_descricao = 'Erro ao Abrir arquivo do Servidor'
                           i_sentido   = c_sentido_integracao  ).

    o_log->set_single_msg( i_status    = c_e
                           i_objeto    = 'LEITURA ADHOC'
                           i_evento    = 'UPLOAD Arquivo CSV'
                           i_descricao = p_file
                           i_sentido   = c_sentido_integracao  ).

  ENDIF.

ENDFORM.                    " F_UPLOAD_FILE