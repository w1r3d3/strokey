;*******************************************************************************************************************
;*
;* constant data include file
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************


;*******************************************************************************************************************
;*
;* GETS INCLUDED INTO THE CODE
;*
;*******************************************************************************************************************
.CSEG


;------------------------------------------------------------------------------------------------
command_strtbl: .db   0x04,   KEYC_H, KEYC_E, KEYC_L, KEYC_P,\
                      0x04,   KEYC_S, KEYC_T, KEYC_A, KEYC_T,\
                      0x03,   KEYC_P, KEYC_W, KEYC_D,\
                      0x03,   KEYC_S, KEYC_T, KEYC_P,\
                      0x03,   KEYC_L, KEYC_O, KEYC_G,\
                      0x03,   KEYC_L, KEYC_G, KEYC_T,\
                      0x03,   KEYC_L, KEYC_G, KEYC_S,\
                      0x03,   KEYC_Q, KEYC_E, KEYC_R,\
                      0x03,   KEYC_C, KEYC_L, KEYC_R,\
                      0x07,   KEYC_M, KEYC_E, KEYC_M, KEYC_T, KEYC_E, KEYC_S, KEYC_T,\
                      0x04,   KEYC_E, KEYC_X, KEYC_I, KEYC_T,\
                      0x00

;------------------------------------------------------------------------------------------------
command_jmptbl: rjmp  command_help
                rjmp  command_stat
                rjmp  command_pwd
                rjmp  command_stp
                rjmp  command_log
                rjmp  command_lgt
                rjmp  command_lgs
                rjmp  command_qer
                rjmp  command_clr
                rjmp  command_mtst
                rjmp  command_exit

;------------------------------------------------------------------------------------------------
;0123456789abcdef
number_lookup:  .db   KEYC_0, KEYC_1, KEYC_2, KEYC_3, KEYC_4,\
                      KEYC_5, KEYC_6, KEYC_7, KEYC_8, KEYC_9,\
                      KEYC_A, KEYC_B, KEYC_C, KEYC_D, KEYC_E,\
                      KEYC_F




;*******************************************************************************************************************
;*
;* GETS INCLUDED INTO THE EEPROM
;*
;*******************************************************************************************************************
.ESEG


;------------------------------------------------------------------------------------------------
;[RET]strokey v1.0en[RET]www.strokey.de[RET]
ee_init_msg:    .db   KEYC_RET,\
                      KEYC_S, KEYC_T, KEYC_R, KEYC_O, KEYC_K, KEYC_E, KEYC_Y, KEYC_SPACE,\
                      KEYC_V, VERSION_MAJOR, KEYC_DOT, VERSION_MINOR, VERSION_LANGUAGE,\
                      KEYC_RET,\
                      KEYC_W, KEYC_W, KEYC_W, KEYC_DOT,\
                      KEYC_S, KEYC_T, KEYC_R, KEYC_O, KEYC_K, KEYC_E, KEYC_Y,\
                      KEYC_DOT, KEYC_C, KEYC_O, KEYC_M, KEYC_RET,\
                      0

;------------------------------------------------------------------------------------------------
;success
ee_success_msg: .db   KEYC_S, KEYC_U, KEYC_C, KEYC_C, KEYC_E, KEYC_S, KEYC_S, 0

;------------------------------------------------------------------------------------------------
;error
ee_error_msg:   .db   KEYC_E, KEYC_R, KEYC_R, KEYC_O, KEYC_R, 0

;------------------------------------------------------------------------------------------------
;abort
ee_abort_msg:   .db   KEYC_A, KEYC_B, KEYC_O, KEYC_R, KEYC_T, 0

;------------------------------------------------------------------------------------------------
;status ec
ee_status_msg:  .db   KEYC_S, KEYC_T, KEYC_A, KEYC_T, KEYC_U, KEYC_S, KEYC_SPACE,\
                      KEYC_E, KEYC_C, 0

;------------------------------------------------------------------------------------------------
;[RET]logsz[SPACE][SPACE]
ee_logsize_msg: .db   KEYC_RET, KEYC_L, KEYC_O, KEYC_G, KEYC_S, KEYC_Z, KEYC_SPACE, KEYC_SPACE, 0

;------------------------------------------------------------------------------------------------
;pwd[SPACE]
ee_pwd_msg:     .db   KEYC_P, KEYC_W, KEYC_D, KEYC_SPACE, 0

;------------------------------------------------------------------------------------------------
;stat,pwd,stp,log,lgt,lgs,qer,clr
ee_help_msg:    .db   KEYC_S, KEYC_T, KEYC_A, KEYC_T, KEYC_COMA,\
                      KEYC_P, KEYC_W, KEYC_D, KEYC_COMA,\
                      KEYC_S, KEYC_T, KEYC_P, KEYC_COMA,\
                      KEYC_L, KEYC_O, KEYC_G, KEYC_COMA,\
                      KEYC_L, KEYC_G, KEYC_T, KEYC_COMA,\
                      KEYC_L, KEYC_G, KEYC_S, KEYC_COMA,\
                      KEYC_Q, KEYC_E, KEYC_R, KEYC_COMA,\
                      KEYC_C, KEYC_L, KEYC_R,\
                      0
