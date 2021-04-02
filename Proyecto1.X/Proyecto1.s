    ;Archivo:	    Proyecto1.s
    ;Dispositivo:   PIC16F887
    ;Autor:	    Margareth Vela
    ;Compilador:    pic-as(v2.31), MPLABX V5.45
    ;
    ;Programa:	    Proyecto Semáforo de 3 vías
    ;Hardware:	    Displays 7 seg en PORTC, transistores en PORTD,
    ;		    leds en PORTA, PORTE y PORTB & push buttons en PORTB
    ;Creado:	    21 mar 2021
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
    movlw   254	    ;Número inicial del tmr0
    movwf   TMR0    
    bcf	    T0IF    ; Se limpia la bandera
    endm

reiniciar_tmr1 macro	; Reinicio de Timer1
    Banksel PORTA   
    movlw   0x85   ; Cargar valor de registro W
    movwf   TMR1H  ; Mover el valor de W al byte más significativo del TMR1
    movlw   0xEE   ; Cargar valor de registro W, valor inicial del tmr0
    movwf   TMR1L  ; Mover el valor de W al byte menos significativo del TMR1
    bcf	    TMR1IF    ; Limpiar bit de interrupción por overflow (Bit de INTCON)	
    endm
    
;-------------------------------------------------------------------------------
; Variables
;-------------------------------------------------------------------------------
Global modo
PSECT udata_bank0  ;Variables en banco 0
    modo:	DS 1	;bandera de modo
    var_modo:	DS 1	;Para configurar el display de modo
    modo_d:	DS 1	;Para display de decenas de modo
    modo_u:	DS 1	;Para display de unidades de modo
    
    sem1_time:	DS 1	;tiempos de configuración
    sem2_time:	DS 1
    sem3_time:	DS 1
    
    unidades:	DS 1	;Para sacar el valor en decimal
    decenas:	DS 1
        
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
    call    int_IOC  	; incrementa o decrementa el puerto A y el display
    
    btfsc   T0IF        ; Si hay overflow en el TMR0,
    call    int_tmr0
    
pop:
    swapf   STATUS_TEMP
    movwf   STATUS
    swapf   W_TEMP, 1
    swapf   W_TEMP, 0
    retfie
    
;-------------------------------------------------------------------------------
; Sub rutinas para interrupciones
;-------------------------------------------------------------------------------
int_tmr0:
    banksel PORTA
    resetTMR0			; Reiniciar el TMR0
    incf    PORTD		; Se reinician los displays
    goto pop ; la bendita linea que faltaba :(
    ;te dejo luego platicamos :)
    
int_IOC:
    btfsc   modo, 0
    call    modo_1_int
    btfsc   modo, 1
    call    modo_2_int
    btfsc   modo, 2
    call    modo_3_int
    btfsc   modo, 3
    call    modo_4_int
    goto    modo_0_int
   goto pop
    
modo_0_int:
    btfss   PORTB, 2  ; Si está presionado el push del bit 1, 
    call    sig_modo     ; decrementa el PORTA
    bcf	    RBIF      ; Se limpia la bandera de IOC
    return

modo_1_int: 
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    incf    sem1_time     ; incrementa el PORTA
    btfss   PORTB, 1  ; Si está presiona el push del bit 0,
    decf    sem1_time    ; incrementa el PORTA
    btfss   PORTB, 2  ; Si está presionado el push del bit 1, 
    call    sig_modo  ; decrementa el PORTA
    bcf	    RBIF      ; Se limpia la bandera de IOC
    return
    
modo_2_int: 
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    incf    sem2_time     ; incrementa el PORTA
    btfss   PORTB, 1  ; Si está presionado el push del bit 1, 
    decf    sem2_time
    btfss   PORTB, 2  ; Si está presionado el push del bit 1, 
    call    estado_3     ; decrementa el PORTA
    bcf	    RBIF      ; Se limpia la bandera de IOC
    return
        
estado_3:
    bcf	    modo, 0	
    bcf	    modo, 1
    bsf     modo, 2
    bcf	    modo, 3
    return
    
modo_3_int:
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    incf    sem3_time     ; incrementa el PORTA
    btfss   PORTB, 1  ; Si está presionado el push del bit 1,
    decf    sem3_time
    btfss   PORTB, 2
    call    estado_4    ; decrementa el PORTA
    bcf	    RBIF      ; Se limpia la bandera de IOC
    return
    
estado_4:
    bcf	    modo, 0	
    bcf	    modo, 1
    bcf     modo, 2
    bsf	    modo, 3
    return
    
modo_4_int:
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    incf    PORTA     ; incrementa el PORTA
    btfss   PORTB, 1  ; Si está presionado el push del bit 1
    decf    PORTA
    btfss   PORTB, 2
    clrf    modo     ; decrementa el PORTA
    bcf	    RBIF      ; Se limpia la bandera de IOC
    return
    
sig_modo:
    incf    modo	; Incrementar la bandera para que cambie de display    
    return
    

    
;-------------------------------------------------------------------------------
; Código Principal 
;-------------------------------------------------------------------------------
PSECT code, delta=2, abs
ORG 0x0100 ;Posición para el código

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
    call config_tmr0
    call config_IOC	; Configuración IOC del puerto B
    
    
loop: 
    
    btfsc   modo, 0
    goto    modo_1
    btfsc   modo, 1
    goto    modo_2
    btfsc   modo, 2
    goto    modo_3
    btfsc   modo, 3
    goto    modo_4
    goto    modo_0
    
    goto    loop
    
;-------------------------------------------------------------------------------
; Subrutinas para loop principal
;-------------------------------------------------------------------------------
modo_0:
    bcf	    PORTB, 5
    bcf	    PORTB, 6
    bcf	    PORTB, 7    
    goto    loop
    
modo_1:
    bsf	    PORTB, 5
    bcf	    PORTB, 6
    bcf	    PORTB, 7
    movf    sem1_time, w
    movwf   var_modo
    call    Modo_config
    call    preparar_displays
    goto    loop
    
modo_2:
    bcf	    PORTB, 5
    bsf	    PORTB, 6
    bcf	    PORTB, 7
    goto    loop
modo_3:
    bcf	    PORTB, 5
    bcf	    PORTB, 6
    bsf	    PORTB, 7
    goto    loop
modo_4:
    bsf	    PORTB, 5
    bsf	    PORTB, 6
    bsf	    PORTB, 7
    goto    loop
    
Modo_config:
    clrf    unidades	; Se limpian las variables a utilizar 
    clrf    decenas
    clrf    modo_d
    clrf    modo_u
        
    movlw 10	     ; Revisión decenas
    subwf var_modo,1 ; Se restan 10 a la variable temporal
    btfsc STATUS, 0 ;Revisión de la bandera de Carry
    incf decenas, 1 ;Si C=1 entonces es menor a 10 y no incrementa la variable
    btfsc STATUS, 0 ;Revisión de la bandera de Carry
    goto $-4
    addwf var_modo,1 ; Se regresa la variable temporal a su valor original
    
    ;Resultado unidades
    movf var_modo, 0 ; Se mueve lo restante en la variable temporal a la
    movwf unidades   ; variable de unidades   
      
    movf    decenas, 0	
    call    tabla	; Se obtiene el valor correspondiente para el display
    movwf   modo_d	; y se coloca en la variable que se utiliza en el cambio
			; de displays (Interrupción TMR0)
    
    movf    unidades, 0
    call    tabla
    movwf   modo_u
    return

preparar_displays:
    btfsc   PORTD, 0		; Revisa el bit de la bandera que 
    goto    display_mod_u	; enciende el display 1
    btfsc   PORTD, 1		; Revisa el bit de la bandera que 
    goto    display_mod_d
    
display_mod_d:
    movf    modo_d, 0	; El primer byte de la variable display va al registro W
    movwf   PORTC		; Ese valor se coloca en el PORTC
    return
    
display_mod_u:
    movf    modo_u, 0	; El segundo byte de la variable display va al registro W
    movwf   PORTC		; Ese valor se coloca en el PORTC
    return
    
;-------------------------------------------------------------------------------
; Subrutinas de configuración
;-------------------------------------------------------------------------------
config_io:
    banksel ANSEL ;Banco 11
    clrf    ANSEL ;Pines digitales
    clrf    ANSELH
    
    banksel TRISA ;Banco 01
    clrf    TRISA    ;Salida de Semáforo 1 y 2
    bsf	    TRISB, 0 ;Push button de incremento
    bsf	    TRISB, 1 ;Push button de decremento 
    bsf	    TRISB, 2 ;Push button de modo de configuración 
    bcf	    TRISB, 5 ;Salida de modo 1 
    bcf	    TRISB, 6 ;Salida de modo 2
    bcf	    TRISB, 7 ;Salida de modo 3
    clrf    TRISC    ;Display multiplexados 7seg 
    clrf    TRISD    ;Alternancia de displays
    clrf    TRISE    ;Salida de Semáforo 3
    
    bcf	    OPTION_REG, 7 ;Habilitar pull-ups
    bsf	    WPUB, 0 ;Push button de incremento
    bsf	    WPUB, 1
    bsf	    WPUB, 2
    
    banksel PORTA ;Banco 00
    clrf    PORTA ;Comenzar contador binario en 0
    clrf    PORTC ;Comenzar displays en 0
    clrf    PORTD ;Comenzar la alternancia de displays en 0
    
    bcf	    PORTB, 5 ;Salida de modo 1 
    bcf	    PORTB, 6 ;Salida de modo 2
    bcf	    PORTB, 7 ;Salida de modo 3
    
    clrf    modo
    movlw   10
    movwf   sem1_time
    return
 
config_reloj:
    banksel OSCCON
    bsf	    IRCF2  ;IRCF = 100 frecuencia= 1MHz
    bcf	    IRCF1
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
    bsf	    IOCB, 1
    bsf	    IOCB, 2
    
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
 /*
config_tmr1:
    Banksel PORTA  
    bsf	    TMR1ON  ; Se habilita el TMR1
    bcf	    TMR1CS  ; Seleccion del reloj interno
    bsf	    T1CKPS0
    bsf	    T1CKPS1 ; Prescaler a 1:8
    reiniciar_tmr1
    return
   
config_tmr2:
    banksel PORTA
    bsf	    TMR2ON  ; Se habilita el TMR2
    bsf	    T2CKPS1
    bsf	    T2CKPS0 ; Prescaler a 1:16
    bsf	    TOUTPS3
    bsf	    TOUTPS2
    bsf	    TOUTPS1
    bsf	    TOUTPS0 ;Postscaler 1:16
    bcf	    TMR2IF  ; Limpiar bit de interrupción por overflow (Bit de INTCON)
    Banksel TRISA   
    movlw   244   ; Cargar valor de registro W, valor de PR2
    movwf   PR2	
    return */
end
