;*******************************************************************************************************************
;*
;* port definitions for board v1.0
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************
.message "BOARD VERSION: 1.0"


;************************************************************************************************
;*
;* VERSION
;*
;************************************************************************************************
.equ VERSION_MAJOR  = KEYC_1        ;major version number
.equ VERSION_MINOR  = KEYC_0        ;minor version number


;************************************************************************************************
;*
;* FLASH TYPE SELECTION
;*
;************************************************************************************************
.equ FLASH_SST49LF020 = 1


;************************************************************************************************
;*
;* PORTS DEFINITIONS
;*
;************************************************************************************************
.equ JMP1           = PORTA0  ;JMP1: enter terminal directly (input/int.pullup)
.equ JMP1_PIN       = PINA    ;
.equ JMP2           = PORTA1  ;JMP2: choose german layout (input/int.pullup)
.equ JMP2_PIN       = PINA    ;

.equ PS2CTRL        = PORTB5 	;ps2 control switch (enabled on input through ext. pullup resistor)
.equ PS2CTRL_PORT   = PORTB   ;connected to IC4066 (C1,C4)
.equ PS2CTRL_DDR    = DDRB    ;

.equ SPK            = PORTB4 	;speaker (output)
.equ SPK_PIN        = PINB    ;

.equ PS2I_CLK       = PORTB0  ;device/keyboard clock (i/o)
.equ PS2I_DATA      = PORTB2 	;device/keyboard data (i/o)
.equ PS2I_DDR       = DDRB    ;
.equ PS2I_PORT      = PORTB   ;
.equ PS2I_PIN       = PINB    ;

.equ PS2O_CLK       = PORTB1 	;host/pc clock (i/o)
.equ PS2O_DATA      = PORTB3 	;host/pc data (i/o)
.equ PS2O_DDR       = DDRB    ;
.equ PS2O_PORT      = PORTB   ;
.equ PS2O_PIN       = PINB    ;

.equ FLASH_RST      = PORTD6 	;flash reset (o)
.equ FLASH_LCLK     = PORTD5 	;flash lclk (o)
.equ FLASH_LFRAME   = PORTD4 	;flash lframe (o)
.equ FLASH_LAD3     = PORTD3 	;flash data (i/o)
.equ FLASH_LAD2     = PORTD2 	;flash data (i/o)
.equ FLASH_LAD1     = PORTD1 	;flash data (i/o)
.equ FLASH_LAD0     = PORTD0 	;flash data (i/o)
.equ FLASH_DDR      = DDRD    ;
.equ FLASH_PORT     = PORTD   ;
.equ FLASH_PIN      = PIND    ;


;************************************************************************************************
;*
;* INITIAL REGISTER DEFINITIONS (SETUP DURING INIT_DEVICE FUNCTION)
;*
;************************************************************************************************
.equ DDRA_INIT      = 0
.equ PORTA_INIT     = (1<<JMP1)+(1<<JMP2)     ;enable pullup-resistors for jumpers

.equ DDRB_INIT      = (1<<SPK)                ;set SPK to output
.equ PORTB_INIT     = 0

.equ DDRD_INIT      = 0x7F                    ;whole port is output
.equ PORTD_INIT     = 0                       ;all pins are zero

.equ PCMSK_INIT     = (1<<PS2I_CLK)           ;call PCInterrupt on every change of the CLK signal
.equ GIMSK_INIT     = (1<<PCIE)

.equ WD_INIT        = 0b01000110              ;initialize watchdog timer interrupt (period = 1s)

.equ PS2RX_STATUS_INIT = PS2RX_STATUS_IDLE
