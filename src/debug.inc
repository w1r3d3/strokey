;*******************************************************************************************************************
;*
;* ENABLE(=1) OR DISABLE(=0) DEBUG FUNCTIONS (dbg_XXX)
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************
.ifndef DEBUG
  .equ DEBUG = 0
.endif


;*******************************************************************************************************************
;*
;* PORTS DEFINITIONS
;*
;*******************************************************************************************************************
.equ DBG_DATA       = PORTA1  ; debug data (output)
.equ DBG_CLK        = PORTA0  ; debug clock (output)
.equ DBG_PORT       = PORTA   ; debug port
.equ DBG_DDR        = DDRA    ;
.equ DBG_PIN        = PINA    ;


;*******************************************************************************************************************
;*
;* MACROS
;*
;*******************************************************************************************************************
.if DEBUG

  .message "*** DEBUG BUILD ***"
  
  .macro dbg_init
    sbi DBG_DDR, DBG_CLK          ;set pin to output
    cbi DBG_PORT, DBG_CLK         ;clear data bit
    sbi DBG_DDR, DBG_DATA         ;set pin to output
    cbi DBG_PORT, DBG_DATA        ;clear data bit

    dbg_beep                      ;beep once
    dbg_puts dbg_msg              ;and print init string
  .endm

  .macro dbg_putc
    rcall _dbg_putc
  .endm

  .macro dbg_puts
    ldi zl, low(@0)
    ldi zh, high(@0)
    rcall _dbg_puts
  .endm

  .macro dbg_beep
    rcall _dbg_beep
  .endm

  .macro dbg_dump ;(length, addr)
    ldi a0, @0
    ldi zl, low (@1)
    ldi zh, high(@1)
    rcall _dbg_dump
  .endm

  .macro dbg_putv
    push a0
    ldi a0, 1
    dbg_putc
    pop a0
    dbg_putc
  .endm

.else

  .message "*** RELEASE BUILD ***"

  .macro dbg_init
  .endm

  .macro dbg_putc
  .endm

  .macro dbg_puts
  .endm

  .macro dbg_beep
  .endm

  .macro dbg_dump
  .endm

.endif
  
