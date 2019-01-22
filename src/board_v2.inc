;*******************************************************************************************************************
;*
;* port definitions for board v2.0
;*
;* copyright (c) 2005-2008 wiRe
;*
;*******************************************************************************************************************
.message "BOARD VERSION: 2.0"


;************************************************************************************************
;*
;* VERSION
;*
;************************************************************************************************
.equ VERSION_MAJOR  = KEYC_2        ;major version number
.equ VERSION_MINOR  = KEYC_0        ;minor version number


;************************************************************************************************
;*
;* FLASH TYPE SELECTION
;*
;************************************************************************************************
.equ FLASH_AT45DB081B = 1


;************************************************************************************************
;*
;* PORTS DEFINITIONS
;*
;************************************************************************************************
.equ JMP1           = PORTA0  ;JMP1: enter terminal directly (input/int.pullup)
.equ JMP1_PIN       = PINA    ;
.equ JMP2           = PORTA1  ;JMP2: choose german layout (input/int.pullup)
.equ JMP2_PIN       = PINA    ;

.equ PS2CTRL        = PORTD6 	;ps2 control switch (enabled on input through ext. pullup resistor)
.equ PS2CTRL_PORT   = PORTD   ;connected to IC4066 (C1,C4)
.equ PS2CTRL_DDR    = DDRD    ;

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

.equ FLASH_RDY      = PORTD2 	;flash ready (i)
.equ FLASH_RESET    = PORTD1 	;flash reset (o, inverted)
.equ FLASH_CS       = PORTD0 	;flash chip select (o, inverted)
.equ FLASH_DDR      = DDRD    ;
.equ FLASH_PORT     = PORTD   ;
.equ FLASH_PIN      = PIND    ;

.equ FLASH_SCK      = PORTB7 	;flash serial clock (o)
.equ FLASH_SO       = PORTB6 	;flash serial data output (o)
.equ FLASH_SI       = PORTB5 	;flash serial data input (i)
;equ FLASH_S_DDR    = DDRB    ;
;equ FLASH_S_PORT   = PORTB   ;
;equ FLASH_S_PIN    = PINB    ;


;************************************************************************************************
;*
;* INITIAL REGISTER DEFINITIONS (SETUP DURING INIT_DEVICE FUNCTION)
;*
;************************************************************************************************
.equ DDRA_INIT      = 0                       ;no output pins
.equ PORTA_INIT     = (1<<JMP1)+(1<<JMP2)     ;enable pullup-resistors for jumpers

.equ DDRB_INIT      = (1<<SPK)+(1<<FLASH_SCK)+(1<<FLASH_SO)   ;set output pins
.equ PORTB_INIT     = 0                                       ;

.equ DDRD_INIT      = (1<<FLASH_RESET)+(1<<FLASH_CS)  ;set output pins
.equ PORTD_INIT     = (1<<FLASH_RDY)+(1<<FLASH_RESET) ;enable pullup-resitor on RDY/#BUSY pin

.equ PCMSK_INIT     = (1<<PS2I_CLK)           ;call PCInterrupt on every change of the CLK signal
.equ GIMSK_INIT     = (1<<PCIE)

.equ WD_INIT        = 0b01000110              ;initialize watchdog timer interrupt (period = 1s)

.equ PS2RX_STATUS_INIT = PS2RX_STATUS_IDLE
