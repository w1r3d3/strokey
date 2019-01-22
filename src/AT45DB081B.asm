;*******************************************************************************************************************
;*
;* title:   support for external flash AT45DB081B
;* file:    AT45DB081B.asm
;* version: 1.0 (03/16/2006)
;* author:  wiRe (wiRe _at_ gmx _dot_ net)
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************
.message "FLASH: AT45DB081B"




;*******************************************************************************************************************
.equ FLASH_NUM_BLOCKS = 512   ;blocks per flash
.equ FLASH_BLOCK_SIZE = 2048  ;exactly: 2112
.equ FLASH_SIZE       = FLASH_NUM_BLOCKS * FLASH_BLOCK_SIZE
.equ FLASH_FLUSH      = 0xFF  ;needed to flush the flash buffers


;*******************************************************************************************************************
.equ FLASH_TX_FAST = 0      ;enable/disable fast'n'large flash access code
.equ FLASH_NUM_PAGES = 8    ;pages per block
.equ FLASH_PAGE_SIZE = 256  ;exactly: 264


;*******************************************************************************************************************
.equ FLASH_CMD_READ_STATUS = 0xD7
.equ FLASH_CMD_BLOCK_ERASE = 0x50
.equ FLASH_CMD_READ_MMP    = 0xD2
.equ FLASH_CMD_READ_MMP2B1 = 0x53
.equ FLASH_CMD_WRITE_B1    = 0x84
.equ FLASH_CMD_PROG_B1     = 0x88
.equ FLASH_CMD_COMP_B1     = 0x60




;************************************************************************************************
;*
;* GETS INCLUDED INTO THE DATA SEGMENT
;*
;************************************************************************************************
.DSEG
flash_page:     .byte 2




;*******************************************************************************************************************
;*
;* GETS INCLUDED INTO THE CODE SEGMENT
;*
;*******************************************************************************************************************
.CSEG




;************************************************************************************************
;*
;* FUNCTION     flash_tx_init
;* DESCRIPTION  selects chip before transfer starts
;* INPUT        at = byte to send
;* OUTPUT       v0 = received byte
;* WASTE        flags, at, v0
;*
;************************************************************************************************
flash_tx_init:  sbi   FLASH_PORT, FLASH_CS    ;selects chip before transfer
               ;rjmp  flash_tx                ;continue with the transfer as usual


;************************************************************************************************
;*
;* FUNCTION     flash_tx
;* DESCRIPTION  sends and receives one byte using USI (universal serial interface)
;* INPUT        at = byte to send
;* OUTPUT       v0 = received byte
;* WASTE        flags, at, v0
;*
;************************************************************************************************
.if FLASH_TX_FAST

flash_tx:       out   USIDR, at               ;write data to send
                ldi   at, (1<<USIWM0)|(1<<USITC)
                ldi   v0, (1<<USIWM0)|(1<<USITC)|(1<<USICLK)
                out   USICR, at               ;clock data out/in...
                out   USICR, v0               ;  (8 clocks)
                out   USICR, at
                out   USICR, v0
                out   USICR, at
                out   USICR, v0
                out   USICR, at
                out   USICR, v0
                out   USICR, at
                out   USICR, v0
                out   USICR, at
                out   USICR, v0
                out   USICR, at
                out   USICR, v0
                out   USICR, at
                out   USICR, v0
                in    v0, USIDR               ;read received data
flash_tx_ret:   ret                           ;return

.else

flash_tx:       out   USIDR, at               ;write data to send
                ldi   v0, 8
flash_tx_lp:    sbi   USICR, USITC            ;clock data out/in...
                sbi   USICR, USICLK
                sbi   USICR, USITC
                dec   v0
                brne  flash_tx_lp
                in    v0, USIDR               ;read received data
flash_tx_ret:   ret                           ;return

.endif


;************************************************************************************************
;*
;* MACRO        flash_tx_finish
;* DESCRIPTION  deselect flash chip after command was transmitted
;* WASTE        n/a
;*
;************************************************************************************************
.macro          flash_tx_finish
                cbi   FLASH_PORT, FLASH_CS    ;deselect chip after transfer
.endm


;************************************************************************************************
;*
;* FUNCTION     flash_tx_cmd
;* DESCRIPTION  send usual command with two parameters and 8 dont-care bits
;* INPUT        at = command id
;*              xh = first parameter
;*              xl = second parameter
;* OUTPUT       v0 = last byte read
;* WASTE        flags, at, v0
;*
;************************************************************************************************
flash_tx_cmd:   rcall flash_tx_init           ;small sub-routine for sending commands
                mov   at, xh                  ;send page number
                rcall flash_tx                ;
                mov   at, xl                  ;
                rcall flash_tx                ;
                mov   at, zero                ;
                rcall flash_tx_n_fin          ;send 8 dont-care bits and deselect chip
               ;rjmp  flash_tx_wait           ;wait until command gets finished


;************************************************************************************************
;*
;* FUNCTION     flash_tx_wait
;* DESCRIPTION  wait until the chip becomes ready (using RDY/#BUSY line instead of status reg.)
;* WASTE        time
;*
;************************************************************************************************
flash_tx_wait:  rcall flash_status            ;detect device, read status
                andi  v0, 0b00111100          ;verify chips id
                cpi   v0, 0b00100100          ;success?
                brne  flash_tx_wait_e         ;  no -> dont wait for nothing

flash_tx_wait_b:sbic  FLASH_PIN, FLASH_RDY    ;busy?
                rjmp  flash_tx_wait_b         ;  no -> loop
flash_tx_wait_r:sbis  FLASH_PIN, FLASH_RDY    ;ready?
                rjmp  flash_tx_wait_r         ;  no -> loop
flash_tx_wait_e:ret




;************************************************************************************************
;*
;* FUNCTION     flash_status
;* DESCRIPTION  read status byte from flash
;* OUTPUT       v0 = status byte
;* WASTE        flags, at, v0
;*
;************************************************************************************************
flash_status:   ldi   at, FLASH_CMD_READ_STATUS
                rcall flash_tx_init
                ldi   at, 0xFF
flash_tx_n_fin: rcall flash_tx
                flash_tx_finish
                ret




;************************************************************************************************
;*
;* FUNCTION     flash_read
;* DESCRIPTION  read one byte from flash at the given address
;* INPUT        yl:xh:xl = 20 bit address
;* OUTPUT       v0 = byte
;* WASTE        flags, at, v0, t0
;*
;************************************************************************************************
flash_read:     ldi   at, FLASH_CMD_READ_MMP  ;send command
                rcall flash_tx_init           ;

                mov   t0, xh                  ;send page number
                mov   at, yl                  ;
                lsl   t0                      ;
                rol   at                      ;
                rcall flash_tx                ;
                mov   at, t0                  ;
                rcall flash_tx                ;

                mov   at, xl                  ;send page offset
                rcall flash_tx                ;

                rcall flash_tx                ;send 32 dont-care bits...
                rcall flash_tx                ;
                rcall flash_tx                ;
                rcall flash_tx                ;

                rjmp  flash_tx_n_fin          ;read data, finish transfer and return




;************************************************************************************************
;*
;* FUNCTION     flash_prog
;* DESCRIPTION  program one byte (place must have been erased before)
;* INPUT        a0 = byte to program (dont waste!)
;*                   0xFF performs a flush instead of writing
;*              flash_addr_c:flash_addr_b:flash_addr_a = 20 bit address
;* OUTPUT       a0 = programmed byte
;*              v0 = a0 for success, otherwise error
;* WASTE        flags, at, v0, x
;*
;************************************************************************************************
flash_prog:     mov   xl, flash_addr_b        ;encode page number into xh:xl
                mov   xh, flash_addr_c        ;shift in one dont-care bit
                lsl   xl                      ;
                rol   xh                      ;

                lds   at, flash_page+0        ;page already loaded?
                cp    at, flash_addr_b        ;
                brne  flash_prog_rld          ;  no -> reload
                lds   at, flash_page+1        ;
                cp    at, flash_addr_c        ;
                breq  flash_prog_wrt          ;  yes -> write

flash_prog_rld: ldi   at, FLASH_CMD_READ_MMP2B1 ;load page to buffer 1
                rcall flash_tx_cmd              ;

                sts   flash_page+0, flash_addr_b  ;store new flash page number
                sts   flash_page+1, flash_addr_c  ;

flash_prog_wrt: cpi   a0, FLASH_FLUSH         ;perform flush?
                breq  flash_prog_flu          ;  yes -> flush

                ldi   at, FLASH_CMD_WRITE_B1  ;change byte inside buffer 1
                rcall flash_tx_init           ;
                rcall flash_tx                ;send 15 dont-care bits...
                clr   at                      ;the 16th bit must contain zero
                rcall flash_tx                ;
                mov   at, flash_addr_a        ;send page offset
                rcall flash_tx                ;
                mov   at, a0                  ;write data to buffer
                rcall flash_tx_n_fin          ;  and deselect chip to close command

                mov   at, flash_addr_a        ;finished whole page?
               ;andi  at, 255                 ;
                cpi   at, 255                 ;
                breq  flash_prog_flu          ;  yes -> flush buffer
                mov   v0, a0                  ;  no -> return success
                ret                           ;

flash_prog_flu: ldi   at, FLASH_CMD_PROG_B1   ;program buffer 1 to page
                rcall flash_tx_cmd            ;
                ldi   at, FLASH_CMD_COMP_B1   ;compare buffer 1 to page
                rcall flash_tx_cmd            ;
                rcall flash_status            ;read result of compare
                mov   at, a0                  ;
                sbrc  v0, 6                   ;data matched (COMP-bit cleared)?
                inc   at                      ;  no -> return error (v0 != a0)
                mov   v0, at                  ;set return value
                ret                           ;and return




;************************************************************************************************
;*
;* FUNCTION     flash_erase
;* DESCRIPTION  erase one block
;* INPUT        z = block number
;* OUTPUT       v0 = first byte of the erased block (0xFF on success)
;* WASTE        flags, at, v0, t0, yl, x
;*
;************************************************************************************************
flash_erase:    movw  xh:xl, zh:zl            ;load z into x
                ldi   at, 4                   ;shift xh:xl 4 bits left
flash_erase_s4: lsl   xl                      ;
                rol   xh                      ;
                dec   at                      ;
                brne  flash_erase_s4          ;

                ldi   at, FLASH_CMD_BLOCK_ERASE ;erase one block
                rcall flash_tx_cmd              ;

                clr   xl                      ;return success
                mov   xh, zl                  ;
                mov   yl, zh                  ;
                lsl   xh                      ;yl:xh << 3
                rol   yl                      ;
                lsl   xh                      ;
                rol   yl                      ;
                lsl   xh                      ;
                rol   yl                      ;
                rjmp  flash_read              ;




;************************************************************************************************
;*
;* FUNCTION     flash_get_size
;* DESCRIPTION  searches last free byte of flash and returns its address
;* OUTPUT       flash_addr_c:flash_addr_b:flash_addr_a = 20 bit address of first free byte
;* WASTE        flags, at, t0, v0, yl, x
;*
;************************************************************************************************
flash_get_size: mov   yl, zero                ;start with zero
                ldx   0                       ;
flash_gs_srbl:  rcall flash_read              ;search first free 256byte-block
                cpi   v0, 0xFF                ;found?
                breq  flash_gs_srbk           ;  yes -> search backwards
                sec                           ;increment address (+256)
                adc   xh, zero                ;
                adc   yl, zero                ;
                cpi   yl, FLASH_SIZE >> 16    ;reached end of flash?
                brsh  flash_gs_ret            ;  yes -> return
                rjmp  flash_gs_srbl           ;  no -> continue block search

flash_gs_srbk:  sbiw  xh:xl, 1                ;search backwards, decrement address
                sbc   yl, zero                ; < 0?
                brmi  flash_gs_inc            ;   yes -> found the first byte
                rcall flash_read              ;read
                cpi   v0, 0xFF                ;found free byte?
                breq  flash_gs_srbk           ;  yes -> search on backwards
flash_gs_inc:   adiw  xh:xl, 1                ;increment address
                adc   yl, zero                ;
flash_gs_ret:   mov   flash_addr_c, yl        ;set return values
                mov   flash_addr_b, xh        ;
                mov   flash_addr_a, xl        ;
                ret                           ;and return




;************************************************************************************************
;*
;* MACRO        flash_init
;* DESCRIPTION  reset flash
;*
;************************************************************************************************
.macro          flash_init
                cbi   FLASH_PORT, FLASH_RESET ;disable reset

               ;clr   flash_addr_c            ;clear flash address
               ;clr   flash_addr_b            ;
               ;clr   flash_addr_a            ;

                ldi   at, 0xFF                ;init flash page, let it point nowhere
                sts   flash_page+0, at        ;
                sts   flash_page+1, at        ;

               .if !FLASH_TX_FAST
                ldi   at, (1<<USIWM0)|(0<<USICS0)
                out   USICR, at
               .endif

                ldi   t0, 255                 ;retry some time
  flash_init_t: rcall flash_status            ;detect device, read status, success?
                andi  v0, 0b10111100          ;  (test chips id and if its ready)
                cpi   v0, 0b10100100          ;
                breq  flash_init_s            ;  yes -> success
               ;ldi   a0, 10                  ;  no ->
               ;rcall delay                   ;        delay some time
                dec   t0                      ;        all retries finshed?
                brne  flash_init_t            ;        no -> retry
  flash_init_e: ldi   at, ERROR_FLASH_INIT    ;        yes -> critical error
                rcall error                   ;               store error number
                dbg_beep                      ;               beep during debug mode
               ;rjmp  reset                   ;               reset device
                rjmp  flash_init_r

  flash_init_s: rcall flash_get_size          ;find first free byte
  flash_init_r:
.endm
