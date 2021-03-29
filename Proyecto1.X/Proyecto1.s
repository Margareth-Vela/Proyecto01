    ;Archivo:	    Proyecto1.s
    ;Dispositivo:   PIC16F887
    ;Autor:	    Margareth Vela
    ;Compilador:    pic-as(v2.31), MPLABX V5.45
    ;
    ;Programa:	    Proyecto Semáforo de 3 vías
    ;Hardware:	    Displays 7 seg en PORTC, transistores en PORTD,
    ;		    leds en PORTA, PORTE y PORTB & push buttons en PORTB
    ;Creado:  21 mar 2021
    ;Última modificación: abr, 2021
    
PROCESSOR 16F887
#include <xc.inc>

; CONFIG1
  CONFIG  FOSC = INTRC_NOCLKOUT ; Oscilador interno sin salidas
  CONFIG  WDTE = OFF            ; WDT disabled (reinicio dispositivo del pic)
  CONFIG  PWRTE = ON            ; PWRT enabled (espera de 72ms al iniciar)
  CONFIG  MCLRE = OFF           ; El pin de MCLR se utiliza como I/O
  CONFIG  CP = OFF              ; Sin protección de código
  CONFIG  CPD = OFF             ; Sin protección de datos
  CONFIG  BOREN = OFF           ; Sin reinicio cuándo el voltaje de alimentacion baja de 4v
  CONFIG  IESO = OFF            ; Reinicio sin cambio de reloj de interno a externo
  CONFIG  FCMEN = OFF           ; Cambio de reloj externo a interno en caso de fallo
  CONFIG  LVP = ON              ; Programacion en bajo voltaje permitida

; CONFIG2
  CONFIG  BOR4V = BOR40V        ; Reinicio abajo de 4V, (BOR21V=2.1V)
  CONFIG  WRT = OFF             ; Protección de autoescritura por el programa desactivada

;-------------------------------------------------------------------------------
; Macro
;-------------------------------------------------------------------------------
resetTMR0 macro 
    banksel PORTA
    movlw   250 ;Número inicial del tmr0
    movwf   TMR0    
    bcf	    T0IF    ; Se limpia la bandera
    endm
    
;-------------------------------------------------------------------------------
; Variables
;-------------------------------------------------------------------------------
Global var, flags, nibble, displays, unidades, decenas, centenas, var_temp
PSECT udata_bank0  ;Variables en banco 0
    var:	DS 1 
    flags:	DS 1
    unidades:	DS 1
    decenas:	DS 1
    centenas:	DS 1
    unidades_1:	DS 1
    decenas_1:	DS 1
    centenas_1:	DS 1
    var_temp:	DS 1
    nibble:	DS 2
    displays:	DS 2
        
PSECT udata_shr ;Share memory
    W_TEMP:	    DS 1 ;1 byte
    STATUS_TEMP:    DS 1 ;1 byte

;-------------------------------------------------------------------------------
; Vector Reset
;-------------------------------------------------------------------------------
PSECT resetvector, class=code, delta=2, abs
ORG 0x0000   ;Posición 0000h para el reset
resetvector:
    PAGESEL setup
    goto setup

;-------------------------------------------------------------------------------
; Vector de interrupción
;-------------------------------------------------------------------------------
PSECT intVect, class=code, delta=2, abs
ORG 0x0004   ;Posición 0004h para el vector interrupción
push:
    movwf   W_TEMP
    swapf   STATUS, 0
    movwf   STATUS_TEMP
 
isr:
    btfsc   RBIF	; Si está encendida la bandera, entonces 
    call    int_IOCB	; incrementa o decrementa el puerto A y el display
    btfsc   T0IF	; Si está encendida la bandera, entonces
    call    int_tmr0	; va a la subrutina del TMR0
    
pop:
    swapf   STATUS_TEMP
    movwf   STATUS
    swapf   W_TEMP, 1
    swapf   W_TEMP, 0
    retfie		;Final de la interrupción
;-------------------------------------------------------------------------------
; Sub rutinas para interrupciones
;-------------------------------------------------------------------------------
int_IOCB:
    banksel PORTA
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    incf    PORTA     ; incrementa el PORTA
    btfss   PORTB, 1  ; Si está presionado el push del bit 1, 
    decf    PORTA     ; decrementa el PORTA
    bcf	    RBIF      ; Se limpia la bandera de IOC
    return
    
int_tmr0:
    resetTMR0			; Reiniciar el TMR0
    clrf    PORTD		; Se reinician los displays
    btfsc   flags, 0		; Revisa el bit de la bandera que 
    goto    display_1		; enciende el display 1
    btfsc   flags, 1		; Revisa el bit de la bandera que 
    goto    display_unidades	; enciende el display de unidades
    btfsc   flags, 2		; Revisa el bit de la bandera que 
    goto    display_decenas	; enciende el display de decenas
    btfsc   flags, 3		; Revisa el bit de la bandera que 
    goto    display_centenas	; enciende el display de centenas
    
display_0:
    movf    displays+0, 0	; El primer byte de la variable display va al registro W
    movwf   PORTC		; Ese valor se coloca en el PORTC
    bsf	    PORTD, 0		; Enciende el bit del PORTD que está conectado al transistor
    goto    siguiente_display	; para que se encienda el display 0
    
display_1:
    movf    displays+1, 0	; El segundo byte de la variable display va al registro W
    movwf   PORTC		; Ese valor se coloca en el PORTC
    bsf	    PORTD, 1		; Enciende el bit del PORTD que está conectado al transistor
    goto    siguiente_display	; para que se encienda el display 1
    
display_unidades:
    movf    unidades_1, 0	; La variable de unidades va al registro W
    movwf   PORTC		; Ese valor se coloca en el PORTC
    bsf	    PORTD, 2		; Enciende el bit del PORTD que está conectado al transistor
    goto    siguiente_display	; para que se encienda el display de unidades
    
display_decenas:
    movf    decenas_1, 0	; La variable de decenas va al registro W
    movwf   PORTC		; Ese valor se coloca en el PORTC
    bsf	    PORTD, 3		; Enciende el bit del PORTD que está conectado al transistor
    goto    siguiente_display	; para que se encienda el display de decenas
    
display_centenas:		
    movf    centenas_1, 0	; La variable de centenas va al registro W
    movwf   PORTC		; Ese valor se coloca en el PORTC
    bsf	    PORTD, 4		; Enciende el bit del PORTD que está conectado al transistor
    goto    siguiente_display	; para que se encienda el display de centenas
    
siguiente_display:
    movf    flags, 0	; Mueve la variable de flags al registro W
    andlw   0x0f	; Se coloca un and para que sea solamente de 4 bits
    incf    flags	; Incrementar la bandera para que cambie de display    
    return
    
;-------------------------------------------------------------------------------
; Código Principal 
;-------------------------------------------------------------------------------
PSECT code, delta=2, abs
ORG 0x0800 ;Posición para el código
 
tabla:
    clrf    PCLATH
    bsf	    PCLATH, 0
    andlw   0x0F
    addwf   PCL		; PC = offset + PCL 
    retlw   00111111B	;0
    retlw   00000110B	;1
    retlw   01011011B	;2
    retlw   01001111B	;3
    retlw   01100110B	;4
    retlw   01101101B	;5
    retlw   01111101B	;6
    retlw   00000111B	;7
    retlw   01111111B	;8
    retlw   01101111B	;9
    retlw   01110111B	;A
    retlw   01111100B	;b
    retlw   00111001B	;C
    retlw   01011110B	;d
    retlw   01111001B	;E
    retlw   01110001B	;F

;-------------------------------------------------------------------------------
; Configuraciones
;-------------------------------------------------------------------------------
setup:
    call config_reloj	; Configuración del reloj
    call config_io	; Configuración de I/O
    call config_int	; Configuración de enable interrupciones
    call config_IOC	; Configuración IOC del puerto B
    call config_tmr0    ; Configuración inicial del tmr0
    banksel PORTA
    
loop: 
    movf   PORTA, 0	; Mueve el valor del contador al registro W
    movwf   var		; Mueve el valor a una variable
    call    separar_nibbles 
    call    preparar_displays
    movf    PORTA,0	; Mueve el valor del contador al registro W
    movwf   var_temp	; Mueve el valor a una variable temporal
    call    Contador_decimal	
    goto    loop
    
;-------------------------------------------------------------------------------
; Subrutinas para loop principal
;-------------------------------------------------------------------------------
 separar_nibbles:
    movf    var, 0	; Mueve el valor de la variable al registro W
    andlw   0x0f	; Solamente toma los primeros 4 bits de la variable
    movwf   nibble	; Mueve el valor de la variable a nibble
    swapf   var, 0	; Cambia los bytes de la variable var
    andlw   0x0f	; Solamente toma los primeros 4 bits de la variable
    movwf   nibble+1	; Mueve el valor de la variable al segundo byte de nibble
    return

preparar_displays:
    movf    nibble, 0	
    call    tabla
    movwf   displays	; Se guarda el valor del primer byte de nibble en el primer byte display
    movf    nibble+1, 0
    call    tabla
    movwf   displays+1	; Se guarda el valor del segundo byte de nibble en el segundo byte display
    return 

Contador_decimal:
    clrf    unidades	; Se limpian las variables a utilizar 
    clrf    decenas
    clrf    centenas
    
    movlw 100		; Revisión centenas
    subwf var_temp,1	; Se restan 100 a la variable temporal 
    btfsc STATUS, 0	; Revisión de la bandera de Carry
    incf centenas, 1 ; Si C=1 entonces es menor a 100 y no incrementa la variable
    btfsc STATUS, 0 ; Revisión de la bandera de Carry para saber si volver 
    goto $-4	    ; a realizar la resta 
    addwf var_temp,1 ; Se regresa la variable temporal a su valor original
    
    movlw 10	     ; Revisión decenas
    subwf var_temp,1 ; Se restan 10 a la variable temporal
    btfsc STATUS, 0 ;Revisión de la bandera de Carry
    incf decenas, 1 ;Si C=1 entonces es menor a 10 y no incrementa la variable
    btfsc STATUS, 0 ;Revisión de la bandera de Carry
    goto $-4
    addwf var_temp,1 ; Se regresa la variable temporal a su valor original
    
    ;Resultado unidades
    movf var_temp, 0 ; Se mueve lo restante en la variable temporal a la
    movwf unidades   ; variable de unidades
    
    call preparar_displays_decimal ; Se mueven los valores a los displays
    
    return
    
preparar_displays_decimal:
    clrf    unidades_1	; Se limpian las variables
    clrf    decenas_1
    clrf    centenas_1
    
    movf    centenas, 0	
    call    tabla	; Se obtiene el valor correspondiente para el display
    movwf   centenas_1	; y se coloca en la variable que se utiliza en el cambio
			; de displays (Interrupción TMR0)
    
    movf    decenas, 0
    call    tabla
    movwf   decenas_1
    
    movf    unidades, 0
    call    tabla
    movwf   unidades_1
    
    return
;-------------------------------------------------------------------------------
; Subrutinas de configuración
;-------------------------------------------------------------------------------
config_io:
    banksel ANSEL ;Banco 11
    clrf    ANSEL ;Pines digitales
    clrf    ANSELH
    
    banksel TRISA ;Banco 01
    clrf    PORTA
    bsf	    TRISB, 0 ;Push button de incremento
    bsf	    TRISB, 1 ;Push button de decremento 
    clrf    TRISC    ;Display multiplexados 7seg 
    clrf    TRISD    ;Alternancia de displays
    
    bcf	    OPTION_REG, 7 ;Habilitar pull-ups
    bsf	    WPUB, 0 
    bsf	    WPUB, 1
    
    banksel PORTA ;Banco 00
    clrf    PORTA ;;Comenzar contador binario en 0
    clrf    PORTC ;Comenzar displays en 0
    clrf    PORTD ;Comenzar la alternancia de displays en 0
    clrf    var_temp ;Se limpia la variable temporal utilizada para el contador decimal
    return
 
config_reloj:
    banksel OSCCON
    bsf	    IRCF2  ;IRCF = 110 frecuencia= 4MHz
    bsf	    IRCF1
    bcf	    IRCF0
    bsf	    SCS	   ;Reloj interno
    return

config_int:
    bsf	GIE	; Se habilitan las interrupciones globales
    bsf	RBIE	; Se habilita la interrupción de las resistencias pull-ups 
    bcf	RBIF	; Se limpia la bandera
    bsf	T0IE    ; Se habilitan la interrupción del TMR0
    bcf	T0IF    ; Se limpia la bandera
    return

config_IOC:
    banksel TRISA
    bsf	    IOCB, 0 ;Se habilita el Interrupt on change de los pines
    bsf	    IOCB, 1 ;
    
    banksel PORTA
    movf    PORTB, 0 ; Termina condición de mismatch
    bcf	    RBIF     ; Se limpia la bandera
    return
;-------------------------------------------------------------------------------
; Subrutinas para TMR0
;-------------------------------------------------------------------------------
config_tmr0:
    banksel TRISA
    bcf	    T0CS    ;Reloj intero
    bcf	    PSA	    ;Prescaler al TMR0
    bsf	    PS2
    bsf	    PS1
    bsf	    PS0	    ;PS = 111  prescaler = 1:256 
    banksel PORTA
    resetTMR0       ;Se reinicia el TMR0
    return
end
