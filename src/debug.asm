;*******************************************************************************************************************
;*
;* title:   debug support
;* file:    debug.asm
;* version: 1.0 (01/18/2006)
;* author:  wiRe (wiRe _at_ gmx _dot_ net)
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************


;*******************************************************************************************************************
;*
;* GETS INCLUDED INTO THE CODE SEGMENT
;*
;*******************************************************************************************************************
.CSEG




;*******************************************************************************************************************
              .if     DEBUG
dbg_msg:      .db     13,13,"StroKey v1.0",13,0
              .endif
;*******************************************************************************************************************




;*******************************************************************************************************************
;*
;* FUNCTION     dbg_putc: send debug character to pc
;* INPUT        a0 = character code
;* WASTE        flags, at, a0
;*
;*******************************************************************************************************************
                .if   DEBUG
_dbg_putc:      push  t0
                push  t1

                cbi   DBG_PORT, DBG_CLK         ;send sync signal...
                sbi   DBG_PORT, DBG_DATA        ;  data=1
                rcall _dbg_delay                ;wait for a stable data signal
                sbi   DBG_PORT, DBG_CLK         ;  clock->1
                rcall _dbg_delay                ;wait for the client to receive this bit

                ldi   t0, 0                     ;init parity counter
                ldi   t1, 8                     ;send 8 bits
_dbg_putc_lp:   cbi   DBG_PORT, DBG_DATA        ;data=0
                lsl   a0                        ;shift register
                brcc  _dbg_putc_cc              ;MSB=1?
                sbi   DBG_PORT, DBG_DATA        ;  yes -> data=1
                inc   t0                        ;         and increment parity
_dbg_putc_cc:   rcall _dbg_delay                ;wait for a stable data signal
                cbi   DBG_PORT, DBG_CLK         ;clock->0
                rcall _dbg_delay                ;wait for the client to receive this bit
                cbi   DBG_PORT, DBG_DATA        ;data=0
                rcall _dbg_delay                ;wait for a stable data signal
                sbi   DBG_PORT, DBG_CLK         ;clock->1
                rcall _dbg_delay                ;wait for the client
                dec   t1                        ;loop for every bit
                brne  _dbg_putc_lp

                sbrs  t0, 0                     ;parity even?
                sbi   DBG_PORT, DBG_DATA        ;  yes -> set data bit
                rcall _dbg_delay                ;wait for a stable data signal
                cbi   DBG_PORT, DBG_CLK         ;clock->0

                rcall _dbg_delay
                pop   t1
                pop   t0
                ret

_dbg_delay:     ldi   at, 6                     ;waste some time (~1/2ms)
_dbg_delay_lp:  dec   zero
                brne  _dbg_delay_lp
                dec   at
                brne  _dbg_delay_lp
                ret
                .endif


;*******************************************************************************************************************
;*
;* FUNCTION     dbg_puts: send debug message string to pc
;* INPUT        z = points to zero-terminated string
;* WASTE        flags, at, a0, z
;*
;*******************************************************************************************************************
                .if   DEBUG
_dbg_puts:      lpm   a0, z+
                tst   a0
                breq  _dbg_puts_ret
                dbg_putc
                rjmp  _dbg_puts
_dbg_puts_ret:  ret
                .endif


;*******************************************************************************************************************
;*
;* FUNCTION     dbg_beep: outputs typical debug sound (to remember user to turn on debugger)
;* WASTE        flags, at, t0, a0, a1, a2, time
;*
;*******************************************************************************************************************
                .if   DEBUG
_dbg_beep:      ldi   a0, 33                  ;beep: 500Hz, 33ms
                ldi   a1, LOW(XTAL*1000/500)
                ldi   a2, HIGH(XTAL*1000/500)
                rcall beep
                ldi   a0, 66                  ;beep: 700Hz, 50ms
                ldi   a1, LOW(XTAL*1000/700)
                ldi   a2, HIGH(XTAL*1000/700)
                rcall beep
                ret
                .endif


;*******************************************************************************************************************
;*
;* FUNCTION     dbg_dump: dumps data from the memory to debug output
;* INPUT        a0 = length
;*              z -> data
;* WASTE        flags, at, t0, a0, z
;*
;*******************************************************************************************************************
                .if   DEBUG
_dbg_dump:      mov   t0, a0

                cpi   t0, 0
                breq  _dbg_dump_ret
_dbg_dump_lp:   ldi   a0, 1
                dbg_putc
                ld    a0, z+
                dbg_putc
                dec   t0
                brne  _dbg_dump_lp

_dbg_dump_ret:  ldi   a0, 13
                dbg_putc
                ret
                .endif
