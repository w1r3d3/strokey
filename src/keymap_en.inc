;*******************************************************************************************************************
;*
;* KEYMAP INCLUDE FILE FOR STROKEY
;*
;* keyboard: english-set2
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************
.message "KEYMAP: ENGLISH-SET2"




;*******************************************************************************************************************
;*
;* BASE, LIMIT INPUT RANGE AND CONTROL KEYCODES
;*
;*******************************************************************************************************************
.equ KEYC_BASE_INDEX  = 0x15  ;the key code of the first recognized input character
.equ KEYC_LIMIT_INDEX = 0x4E  ;the key code of the last recognized input character

.equ KEYC_BREAK = 0xF0
.equ KEYC_EXT0  = 0xE0
.equ KEYC_EXT1  = 0xE1


;*******************************************************************************************************************
;*
;* SPECIAL KEYS
;*
;*******************************************************************************************************************
.equ KEYC_TAB       = 0x0D
.equ KEYC_RET       = 0x5A
.equ KEYC_BKSP      = 0x66
.equ KEYC_ESC       = 0x76
.equ KEYC_SHIFT     = 0x12

.equ KEYC_ACCENT    = 0x0E  ;'`'
.equ KEYC_SPACE     = 0x29  ;' '
.equ KEYC_COMA      = 0x41  ;','
.equ KEYC_DOT       = 0x49  ;'.'
.equ KEYC_SLASH     = 0x4A  ;'/'
.equ KEYC_SEMICOLON = 0x4C  ;';'
.equ KEYC_MINUS     = 0x4E  ;'-'
.equ KEYC_APOSTROPH = 0x52  ;'\''
.equ KEYC_BBRK_OP   = 0x54  ;'['
.equ KEYC_EQUAL     = 0x55  ;'='
.equ KEYC_BBRK_CL   = 0x5B  ;']'
.equ KEYC_BKSLASH   = 0x5D  ;'\'


;*******************************************************************************************************************
;*
;* INPUT KEYS
;*
;*******************************************************************************************************************
.equ KEYC_Q = 0x15
.equ KEYC_1 = 0x16
.equ KEYC_Z = 0x1A
.equ KEYC_S = 0x1B
.equ KEYC_A = 0x1C
.equ KEYC_W = 0x1D
.equ KEYC_2 = 0x1E
.equ KEYC_C = 0x21
.equ KEYC_X = 0x22
.equ KEYC_D = 0x23
.equ KEYC_E = 0x24
.equ KEYC_4 = 0x25
.equ KEYC_3 = 0x26
.equ KEYC_V = 0x2A
.equ KEYC_F = 0x2B
.equ KEYC_T = 0x2C
.equ KEYC_R = 0x2D
.equ KEYC_5 = 0x2E
.equ KEYC_N = 0x31
.equ KEYC_B = 0x32
.equ KEYC_H = 0x33
.equ KEYC_G = 0x34
.equ KEYC_Y = 0x35
.equ KEYC_6 = 0x36
.equ KEYC_M = 0x3A
.equ KEYC_J = 0x3B
.equ KEYC_U = 0x3C
.equ KEYC_7 = 0x3D
.equ KEYC_8 = 0x3E
.equ KEYC_K = 0x42
.equ KEYC_I = 0x43
.equ KEYC_O = 0x44
.equ KEYC_0 = 0x45
.equ KEYC_9 = 0x46
.equ KEYC_L = 0x4B
.equ KEYC_P = 0x4D


;*******************************************************************************************************************
;*
;* LANGUAGE VERSION
;*
;*******************************************************************************************************************
.equ VERSION_LANGUAGE = KEYC_E      ;our language id
