;************************************************************************************************
;*
;* EQUATES
;*
;* copyright (c) 2005-2008 wiRe
;*
;************************************************************************************************
.equ DEBUG = 0                      ;enable/disable debug mode

.equ XTAL = 8000                    ;XTAL frequency in KHz
.equ TIMER0A = 20*XTAL/1000         ;20us timer-value

;equ VERSION_MAJOR  = KEYC_1        ;our major version number
;equ VERSION_MINOR  = KEYC_0        ;our minor version number
;equ VERSION_LANGUAGE = KEYC_E      ;our language id

.equ PS2RX_STATUS_IDLE    = 0       ;idle state (no transfer)
.equ PS2RX_STATUS_TxD2H   = 1       ;device2host transfer
.equ PS2RX_STATUS_TxH2D_A = 2       ;host2device transfer
.equ PS2RX_STATUS_TxH2D_B = 3       ;host2device transfer, wait for stopbit
.equ PS2RX_STATUS_TxH2D_C = 4       ;host2device transfer, wait for ACK
.equ PS2RX_STATUS_STOPPED = -1      ;stop reception

.equ PS2TX_STATUS_IDLE    = 0xFF    ;nothing to send, check only for host
.equ PS2TX_STATUS_TxD2H   = 0x80    ;start transmission: device2host
.equ PS2TX_STATUS_TxH2D   = 0x40    ;start transmission: host2device
.equ PS2TX_STATUS_MASK    = 0x3F    ;counter mask

.equ PS2RX_BUF_SIZE = (1<<6)        ;size of Rx buffer (power-of-two values only!)
.equ INPUT_BUF_SIZE = 16            ;size of input buffer

                                    ;error codes
.equ ERROR_SUCCESS        = 0x00    ;
.equ ERROR_FLASH_INIT     = 0xF1    ;
.equ ERROR_FLASH_BUS      = 0xFB    ;
.equ ERROR_FLASH_VERIFY   = 0xF7    ;
.equ ERROR_FLASH_OVRFLW   = 0xF0    ;
.equ ERROR_INPUT_OVRFLW   = 0x0F    ;

.equ LOGMODE_STP = KEYC_DOT         ;logging modes
.equ LOGMODE_LOG = KEYC_L           ;
.equ LOGMODE_LGT = KEYC_T           ;
.equ LOGMODE_LGS = KEYC_S           ;

.equ LOG_IDLE_TIME  = 45            ;idle time in seconds before logging starts
.equ LOG_SEQ_LENGTH = 255           ;max characters to log before sequence finishs

                                    ;control ids (bytes) written to the flash...
.equ LOGID_ERROR    = 0xFC          ;
.equ LOGID_RESTART  = 0xFD          ;
.equ LOGID_AWAKE    = 0xFE          ;
.equ LOGID_ERASED   = 0xFF          ;
