;*******************************************************************************************************************
;*
;* title:   support for external flash SST49LF020
;* file:    SST49LF020.asm
;* version: 1.0 (01/18/2006)
;* author:  wiRe (wiRe _at_ gmx _dot_ net)
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************
.message "FLASH: SST49LF020"




;*******************************************************************************************************************
.equ FLASH_NUM_BLOCKS = 16
.equ FLASH_BLOCK_SIZE = 0x4000
.equ FLASH_SIZE       = FLASH_NUM_BLOCKS * FLASH_BLOCK_SIZE
;equ FLASH_FLUSH      = 0xFF     ;no need to flush the flash buffers here


;*******************************************************************************************************************
.equ FLASH_VENDOR_ID  = 0xBF
.equ FLASH_DEVICE_ID  = 0x61




;*******************************************************************************************************************
;*
;* GETS INCLUDED INTO THE CODE SEGMENT
;*
;*******************************************************************************************************************
.CSEG




;************************************************************************************************
;*
;* FUNCTION     flash_frmout
;* DESCRIPTION  starts a new frame and writes address
;* INPUT        yl:xh:xl = 20 bit address
;*              t0 = memory access type (0b0101 = read, 0b0110 = write)
;* WASTE        flags, at
;*
;************************************************************************************************
flash_frmout:   ldi   at, (1<<FLASH_RST)
                rcall flash_clkout            ;start cycle

                mov   at, t0                  ;memory type cycle
                rcall flash_clklad            ;

                rcall flash_clklad            ;address cycle #1 (data ignored)
                rcall flash_clklad            ;address cycle #2 (data ignored)
                rcall flash_clklad            ;address cycle #3 (data ignored)
                mov   at, yl
                rcall flash_clklad            ;address cycle #4 [19:16]
                mov   at, xh
                swap  at
                rcall flash_clklad            ;address cycle #5 [15:12]
                mov   at, xh
                rcall flash_clklad            ;address cycle #6 [11:8]
                mov   at, xl
                swap  at
                rcall flash_clklad            ;address cycle #7 [7:4]
                mov   at, xl
                rjmp  flash_clklad            ;address cycle #8 [3:0]


;************************************************************************************************
;*
;* FUNCTION     flash_sync
;* DESCRIPTION  process two tar cycles and switch to input, then process
;*              one sync cycle, waiting for the acknowledge signal (endless otherwise)
;* WASTE        flags, at, t0
;*
;************************************************************************************************
flash_sync:     ldi   at, (1<<FLASH_RST)+(1<<FLASH_LFRAME)+0b1111

                rcall flash_clkout            ;turn-around cycle #1
                ldi   t0, 0b01110000          ;set LAD pins to input
                out   FLASH_DDR, t0           ;
                rcall flash_clkout            ;turn-around cycle #2 (enables pull-up resitors on LAD-pins)

flash_sync_lp:  in    t0, FLASH_PIN           ;sync cycle
                andi  t0, 15
                breq  flash_sync_ack          ;received some ack? -> leave loop
                cpi   t0, 0x0A                ;explicit error?
                brne  flash_sync_lp           ;  no -> loop

                ldi   at, ERROR_FLASH_BUS     ;  yes -> we got an error
                rcall error                   ;
                ldi   at, (1<<FLASH_RST)+(1<<FLASH_LFRAME)+0b1111   ;(restore at)

flash_sync_ack: rjmp  flash_clkout            ;finish sync cycle (enables pull-up resitors on LAD-pins)


;************************************************************************************************
;*
;* FUNCTION     flash_clklad, flash_clkout
;* DESCRIPTION  some little help-functions
;* INPUT        at = output value (flash_clklad -> LAD only)
;* WASTE        flags, at
;*
;************************************************************************************************
flash_clklad:   andi  at, 15                  ;extract lad data from at and set control pins
                ori   at, (1<<FLASH_RST)+(1<<FLASH_LFRAME)
flash_clkout:   out   FLASH_PORT, at          ;CLK->0
                sbi   FLASH_PORT, FLASH_LCLK  ;CLK->1, latch data
flash_ret:      ret




;************************************************************************************************
;*
;* FUNCTION     flash_read
;* DESCRIPTION  read one byte from SST49 at the given address
;* INPUT        yl:xh:xl = 20 bit address
;* OUTPUT       v0 = byte
;* WASTE        flags, at, t0, v0
;*
;************************************************************************************************
flash_read:     ldi   t0, 0b0101              ;send memory read frame
                rcall flash_frmout

                rcall flash_sync              ;process tar cycles (-> input) and sync cycle
                                              ;dont change at register, its needed by flash_tar

                in    v0, FLASH_PIN           ;data cycle #1 [3:0]
                andi  v0, 15
                rcall flash_clkout            ;finish data cycle #1

                in    t0, FLASH_PIN           ;data cycle #2 [7:4]
                andi  t0, 15
                swap  t0
                or    v0, t0
                rcall flash_clkout            ;finish data cycle #2
                
                rjmp  flash_tar               ;process tar cycles (-> output)


;************************************************************************************************
;*
;* FUNCTION     flash_write
;* DESCRIPTION  write one byte to the given address on the SST49
;* INPUT        yl:xh:xl = 20 bit address
;*              a0 = byte to write
;* WASTE        flags, at, t0
;*
;************************************************************************************************
flash_write:    ldi   t0, 0b0110              ;send memory write frame
                rcall flash_frmout
                
                mov   at, a0                  ;data cycle #1 [3:0]
                rcall flash_clklad            ;
                mov   at, a0                  ;data cycle #2 [7:4]
                swap  at                      ;
                rcall flash_clklad            ;

                rcall flash_sync              ;process tar cycles (-> input) and sync cycle

               ;rjmp  flash_tar               ;process tar cycles (-> output)
flash_tar:      rcall flash_clkout            ;turn-around cycle #1
                ldi   t0, 0b01111111          ;set all pins to output
                out   FLASH_DDR, t0           ;
                rjmp  flash_clkout            ;turn-around cycle #2




;************************************************************************************************
;*
;* FUNCTION     flash_cmd
;* DESCRIPTION  send command header
;* WASTE        flags, at, t0, a0, yl, x
;*
;************************************************************************************************
flash_cmd:      ldx   0x5555                  ;write(0x5555, 0xAA)
                mov   yl, zero
                ldi   a0, 0xAA
                rcall flash_write
                ldx   0x2AAA                  ;write(0x2AAA, 0x55)
                ldi   a0, 0x55
                rjmp  flash_write


;************************************************************************************************
;*
;* FUNCTION     flash_enter
;* DESCRIPTION  enter special mode (software id, programming, ...)
;* WASTE        flags, at, t0, t1, a0, yl, x
;*
;************************************************************************************************
flash_enter:    mov   t1, a0
                rcall flash_cmd
                ldx   0x5555                  ;write(0x5555, a0)
                mov   a0, t1
                rjmp  flash_write


;************************************************************************************************
;*
;* FUNCTION     flash_prog
;* DESCRIPTION  program one byte (place must have been erased before)
;* INPUT        a0 = byte to program (dont waste!)
;*              flash_addr_c:flash_addr_b:flash_addr_a = 20 bit address
;* OUTPUT       a0 = programmed byte
;*              v0 = a0 for success, otherwise error
;* WASTE        flags, at, v0, t0..t2, yl, x
;*
;************************************************************************************************
flash_prog:    .ifdef FLASH_FLUSH
                cpi   a0, FLASH_FLUSH
                brne  flash_prog_prg
                mov   v0, a0
                ret
               .endif

flash_prog_prg: mov   t2, a0
                ldi   a0, 0xA0                ;enter byte programming mode
                rcall flash_enter

                mov   yl, flash_addr_c        ;write one byte
                movw  xh:xl, flash_addr_b:flash_addr_a
                mov   a0, t2                  ;restore a0
                rcall flash_write

                ldi   at, ((XTAL*20)+2999)/3000   ;delay 20us
flash_prog_del: dec   at                      ;1 cycle
                brne  flash_prog_del          ;2 cycles

                rjmp  flash_read              ;read byte for verification


;************************************************************************************************
;*
;* FUNCTION     flash_erase
;* DESCRIPTION  erase one block (16k = 4000h bytes)
;* INPUT        z = block number
;* OUTPUT       v0 = first byte of block after erase
;* WASTE        flags, at, t0, t1, a0, a1, yl, x
;*
;************************************************************************************************
flash_erase:    ldi   a0, 0x80                ;enter erase mode
                rcall flash_enter

                rcall flash_cmd               ;send second command
                mov   yl, zl                  ;build address of block
                mov   xh, zero                ;  yl:xh:xl = zl << 14
                mov   xl, zero                ;
                lsr   yl                      ;
                ror   xh                      ;
                lsr   yl                      ;
                ror   xh                      ;
                ldi   a0, 0x50                ;erase block
                rcall flash_write

                ldi   a0, 25                  ;delay 25ms
                rcall delay                   ;

                rjmp  flash_read              ;read one byte before further programming




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
;* DESCRIPTION  reset flash and read vendor/product id
;*
;************************************************************************************************
.macro          flash_init
               ;clr   flash_addr_c            ;clear flash address
               ;clr   flash_addr_b            ;
               ;clr   flash_addr_a            ;

                ldi   at, (1<<FLASH_RST)+(1<<FLASH_LFRAME)
                out   FLASH_PORT, at          ;init i/o
               ;ldi   a0, 1                   ;wait 1ms to finish reset
               ;rcall delay                   ;

                ldi   a0, 0x90                ;software id entry
                rcall flash_enter

                ldx   0                       ;wait at least 150ns (two instructions)
                rcall flash_read              ;before reading the software ids
                mov   t2, v0
                inc   xl
                rcall flash_read
                mov   t3, v0

                ldi   a0, 0xF0                ;software id exit
                rcall flash_write

                cpi   t2, FLASH_VENDOR_ID     ;compare id's, equal?
                brne  flash_init_e            ;
                cpi   t3, FLASH_DEVICE_ID     ;
                breq  flash_init_s            ;   yes -> success
  flash_init_e: ldi   at, ERROR_FLASH_INIT    ;   no -> 
                rcall error                   ;         critical error
                dbg_beep                      ;         beep during debug mode
                rjmp  flash_init_r
  flash_init_s:
                rcall flash_get_size          ;find first free byte
  flash_init_r:
.endm
