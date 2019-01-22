;*******************************************************************************************************************
;*
;* Global Register Variables
;* r26..r31 is used for XL, XH, YL, YH, ZL and ZH registers
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************
.def zero   = r0        ;zero register (not hardwired!)

.def ss     = r1      	;saved registers during interrupts
.def sat    = r2        ;(dont modify!)
.def s0     = r3        ;
.def s1     = r4        ;
.def s2     = r5        ;

.def flash_addr_a = r6  ;special-case register
.def flash_addr_b = r7  ;special-case register
.def flash_addr_c = r8  ;special-case register

.def ps2tx_status = r9  ;special-case register
.def ps2tx_data   = r10 ;special-case register
.def ps2tx_rdata  = r11 ;special-case register
.def ps2tx_parity = r12 ;special-case register

.def ps2rx_parity = r13 ;special-case register
.def ps2rx_shft_l = r14 ;special-case register
.def ps2rx_shft_h = r15 ;special-case register

.def at     = r16       ;temporary registers
.def t0     = r17       ;
.def t1     = r18       ;
.def t2     = r19       ;
.def t3     = r20       ;
.def v0     = r21       ;function return value
.def a0     = r22       ;function arguments
.def a1     = r23       ;
.def a2     = r24       ;
.def a3     = r25       ;
