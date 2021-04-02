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
; Macros
;-------------------------------------------------------------------------------       
reiniciar_tmr0 macro	; Reinicio de Timer0
    Banksel PORTA	; Acceder al Bank 0
    movlw   254		; Cargar valor de registro W, valor inicial del tmr0
    ; t_deseado=(4*t_oscilación)(256-TMR0)(Preescaler)
    movwf   TMR0	; Mover el valor de W a TMR0 por interrupción
    bcf	    T0IF	; Limpiar bandera de interrupción por overflow	
    endm
    
reiniciar_tmr1 macro	; Reinicio de Timer1
    Banksel PORTA	; Acceder al Bank 0
    movlw   0x85	; Cargar valor de registro W, valor inicial del tmr1
    movwf   TMR1H	; Mover el valor de W a TMR1H
    movlw   0xEE	; Cargar valor de registro W, valor inicial del tmr1
    movwf   TMR1L	; Mover el valor de W a TMR1L
    bcf	    TMR1IF	; Limpiar bandera de interrupción por overflow	
    endm
 
reiniciar_tmr2 macro	; Reinicio de Timer2
    Banksel PORTA	; Acceder al Bank 0
    movlw   0xFF	; Cargar valor de registro W, valor inicial del tmr2
    movwf   PR2		; Mover el valor de W a PR2
    bcf	    TMR2IF	; Limpiar bandera de interrupción por overflow	
    endm
    
;-------------------------------------------------------------------------------
; Variables 
;-------------------------------------------------------------------------------
PSECT udata_bank0  ; Variables en banco 0
    modo:	DS 1	;bandera de modo
    var_modo:	DS 1	;Para configurar el display de modo
    modo_d:	DS 1	;Para display de decenas de modo
    modo_u:	DS 1	;Para display de unidades de modo
    
    sem_t1:	DS 1  ;Para display de tiempo de semáforo 1
    decenas_s1: DS 1
    unidades_s1:DS 1
    sem1_d:	DS 1
    sem1_u:	DS 1
    
    sem_t2:	DS 1  ;Para display de tiempo de semáforo 2
    decenas_s2: DS 1
    unidades_s2:DS 1
    sem2_d:	DS 1
    sem2_u:	DS 1
    
    sem_t3:	DS 1  ;Para display de tiempo de semáforo 3
    decenas_s3: DS 1
    unidades_s3:DS 1
    sem3_d:	DS 1
    sem3_u:	DS 1
    
    sem1_time:	DS 1	;tiempos de configuración
    sem2_time:	DS 1
    sem3_time:	DS 1
    
    flags:	DS 1
    contador:	DS 1
    contador_temp: DS 1
    unidades:	DS 1	;Para sacar el valor en decimal
    decenas:	DS 1

    
PSECT udata_shr	    ; Variables en Share memory
    W_TEMP:	    DS 1 
    STATUS_TEMP:    DS 1 
    
;-------------------------------------------------------------------------------
; Vector reset
;-------------------------------------------------------------------------------
PSECT resVect, class=CODE, abs, delta=2
ORG 00h
resetVec:
    PAGESEL main
    goto main

;-------------------------------------------------------------------------------
; Vector de interrupción
;-------------------------------------------------------------------------------
PSECT intVect, class=CODE, abs, delta=2
ORG 04h	    ; Posicion para las interrupciones

push:
    movwf   W_TEMP
    swapf   STATUS, W
    movwf   STATUS_TEMP
    
isr:
    btfsc   T0IF	; Testear la bandera de interrupción del TMR0
    call    int_tmr0	; Ir a la subrutina del TMR0
    btfsc   TMR1IF	; Testear la bandera de interrupción del TMR1
    call    int_tmr1	; Ir a la subrutina del TMR1
    btfsc   TMR2IF	; Testear la bandera de interrupción del TMR2
    call    int_tmr2	; Ir a la subrutina del TMR2

pop:
    swapf   STATUS_TEMP, W
    movwf   STATUS
    swapf   W_TEMP, F
    swapf   W_TEMP, W
    retfie
   
;--------------------------------------------------------------------------------
; Sub rutinas para interrupciones
;------------------------------------------------------------------------------- 
int_tmr2:
    
    reiniciar_tmr2
    
    return

int_tmr1:
    reiniciar_tmr1
    decf    sem1_time
    decf    sem2_time
    decf    sem3_time
    return
    
int_tmr0:
    reiniciar_tmr0		; Reiniciar el TMR0
    clrf    PORTD		; Limpiar los displays
    btfsc   flags, 0		; Revisa el bit de la bandera que 
    goto    display_unidades_s1	; enciende el semaforo 1 unidades
    btfsc   flags, 1		; Revisa el bit de la bandera que 
    goto    display_decenas_s1	; enciende el semaforo 1 decenas
    btfsc   flags, 2		; Revisa el bit de la bandera que 
    goto    display_unidades_s2	; enciende el semaforo 2 unidades
    btfsc   flags, 3		; Revisa el bit de la bandera que 
    goto    display_decenas_s2	; enciende el semaforo 2 decenas
    btfsc   flags, 4		; Revisa el bit de la bandera que 
    goto    display_unidades_s3	; enciende el semaforo 3 unidades
    btfsc   flags, 5		; Revisa el bit de la bandera que 
    goto    display_decenas_s3	; enciende el semaforo 3 decenas
    btfsc   flags, 6		; Revisa el bit de la bandera que 
    goto    display_unidades_modo; enciende el semaforo 3 unidades
    btfsc   flags, 7		; Revisa el bit de la bandera que 
    goto    display_decenas_modo; enciende el semaforo 3 decenas
    
display_unidades_s1:
    movf    sem1_u, W	; El primer byte de display va al registro W
    movwf   PORTC		; Colocar el valor en el PORTC
    bsf	    PORTD, 3		; Seleccionar unidades
    movlw   0x2			; Preparar para siguiente display
    movwf   flags
    return
    
display_decenas_s1:
    movf    sem1_d, W	; El segundo byte de display va al registro W
    movwf   PORTC		; Colocar el valor en el PORTC
    bsf	    PORTD, 2		; Seleccionar decenas
    movlw   0x4			; Preparar para siguiente display
    movwf   flags
    return
    
display_unidades_s2:
    movf    sem2_u, W	; El primer byte de display va al registro W
    movwf   PORTC		; Colocar el valor en el PORTC
    bsf	    PORTD, 5		; Seleccionar unidades
    movlw   0x8			; Preparar para siguiente display
    movwf   flags
    return
    
display_decenas_s2:
    movf    sem2_d, W	; El segundo byte de display va al registro W
    movwf   PORTC		; Colocar el valor en el PORTC
    bsf	    PORTD, 4		; Seleccionar decenas
    movlw   0x10		; Preparar para siguiente display
    movwf   flags
    return
    
display_unidades_s3:
    movf    sem3_u, W	; El primer byte de display va al registro W
    movwf   PORTC		; Colocar el valor en el PORTC
    bsf	    PORTD, 7		; Seleccionar unidades
    movlw   0x20		; Preparar para siguiente display
    movwf   flags
    return
    
display_decenas_s3:
    movf    sem3_d, W	; El segundo byte de display va al registro W
    movwf   PORTC		; Colocar el valor en el PORTC
    bsf	    PORTD, 6		; Seleccionar decenas
    movlw   0x40		; Preparar para siguiente display
    movwf   flags
    return
    
display_unidades_modo:
    movf    modo_u, W	; El primer byte de display va al registro W
    movwf   PORTC		; Colocar el valor en el PORTC
    bsf	    PORTD, 1		; Seleccionar unidades
    movlw   0x80		; Preparar para siguiente display
    movwf   flags
    return
    
display_decenas_modo:
    movf    modo_d, W	; El segundo byte de display va al registro W
    movwf   PORTC		; Colocar el valor en el PORTC
    bsf	    PORTD, 0		; Seleccionar decenas
    movlw   0x1			; Preparar para siguiente display
    movwf   flags
    return
    
;-------------------------------------------------------------------------------
; Configuración del microcontrolador
;-------------------------------------------------------------------------------
PSECT code, delta=2, abs
ORG 100h ;Posición para el código

tabla:
    clrf    PCLATH	; PCLATH = 00
    bsf	    PCLATH, 0	; PCLATH = 01
    andlw   0x0F	; Se utilizan solo los 4 bits menos signficativos
    addwf   PCL		; PC = PCL + PCLATH
    retlw   00111111B	; 0
    retlw   00000110B	; 1
    retlw   01011011B	; 2
    retlw   01001111B	; 3	
    retlw   01100110B	; 4
    retlw   01101101B	; 5
    retlw   01111101B	; 6
    retlw   00000111B	; 7
    retlw   01111111B	; 8
    retlw   01101111B	; 9
    retlw   01110111B	; A
    retlw   01111100B	; B
    retlw   00111001B	; C
    retlw   01011110B	; D
    retlw   01111001B	; E
    retlw   01110001B	; F
  
;-------------------------------------------------------------------------------
; Configuraciones
;-------------------------------------------------------------------------------
main:
    call    config_io		; Configurar entradas y salidas
    call    config_reloj	; Configurar el reloj (oscilador)
    call    config_tmr0		; Configurar el registro de TMR0
    call    config_tmr1		; Configurar el registro de TMR1
    call    config_tmr2		; Configurar el registro de TMR2
    call    config_int		; Configuración de las interrupciones
    call    tiempos	; Cargan los valores iniciales
        
loop: 
    call    display_semaforo1	; Preparar el dato decimal del display del s1
    call    display_semaforo2	; Preparar el dato decimal del display del s2
    call    display_semaforo3	; Preparar el dato decimal del display del s3
    ;btfsc   bandera, 0; en tu loop vas a verificar que via tiene la bandera
    ;call via1  ; y vas a asignar el valor de cont_viax = vartemp del display
    goto    loop
    
;-------------------------------------------------------------------------------
; Subrutinas para loop principal
;-------------------------------------------------------------------------------
/*via1:
    movf cont_via1, w
    movwf contador_temp
    call modo_config
    return*/
    
display_semaforo1:	    
    movf    sem1_time, 0	    ; Mueve el valor del contador al registro W
    movwf   sem_t1	    ; Mueve el valor a una variable temporal
    
    clrf    unidades_s1	    ; Se limpian las variables a utilizar 
    clrf    decenas_s1
    
    movlw 10		    ; Revisión decenas
    subwf sem_t1,1	    ; Se restan 10 a la variable temporal
    btfsc STATUS, 0	    ; Revisión de la bandera de Carry
    incf decenas_s1, 1	    ; Si C=1 entonces es menor a 10 y no incrementa la variable
    btfsc STATUS, 0	    ; Revisión de la bandera de Carry
    goto $-4
    addwf sem_t1,1	    ; Se regresa la variable temporal a su valor original
    
    movf sem_t1, 0	    ; Se mueve lo restante en la variable temporal a la
    movwf unidades_s1	    ; variable de unidades
    
    clrf    sem1_d    ; Se limpian las variables
    clrf    sem1_u
    
    movf    decenas_s1, 0
    call    tabla
    movwf   sem1_d
    
    movf    unidades_s1, 0
    call    tabla
    movwf   sem1_u
    
    return 
    
display_semaforo2:   
    movf    sem2_time, 0	; Mueve el valor del contador al registro W
    movwf   sem_t2	; Mueve el valor a una variable temporal
    
    clrf    unidades_s2	; Se limpian las variables a utilizar 
    clrf    decenas_s2
    
    movlw 10		; Revisión decenas
    subwf sem_t2,1	; Se restan 10 a la variable temporal
    btfsc STATUS, 0	; Revisión de la bandera de Carry
    incf decenas_s2, 1	; Si C=1 entonces es menor a 10 y no incrementa la variable
    btfsc STATUS, 0	; Revisión de la bandera de Carry
    goto $-4
    addwf sem_t2,1	; Se regresa la variable temporal a su valor original
    
    movf sem_t2, 0 ; Se mueve lo restante en la variable temporal a la
    movwf unidades_s2   ; variable de unidades
    
    clrf    sem2_d	; Se limpian las variables
    clrf    sem2_u
    
    movf    decenas_s2, 0
    call    tabla
    movwf   sem2_d
    
    movf    unidades_s2, 0
    call    tabla
    movwf   sem2_u
    
    return 
    
display_semaforo3:  
    
    movf    sem3_time, 0	; Mueve el valor del contador al registro W
    movwf   sem_t3	; Mueve el valor a una variable temporal
    
    clrf    unidades_s3	; Se limpian las variables a utilizar 
    clrf    decenas_s3
    
    movlw 10		; Revisión decenas
    subwf sem_t3,1	; Se restan 10 a la variable temporal
    btfsc STATUS, 0	; Revisión de la bandera de Carry
    incf decenas_s3, 1	; Si C=1 entonces es menor a 10 y no incrementa la variable
    btfsc STATUS, 0	; Revisión de la bandera de Carry
    goto $-4
    addwf sem_t3,1	; Se regresa la variable temporal a su valor original
    
    movf sem_t3, 0 ; Se mueve lo restante en la variable temporal a la
    movwf unidades_s3   ; variable de unidades
    
    clrf    sem3_d	; Se limpian las variables
    clrf    sem3_u
    
    movf    decenas_s3, 0
    call    tabla
    movwf   sem3_d
    
    movf    unidades_s3, 0
    call    tabla
    movwf   sem3_u
    
    return 

;-------------------------------------------------------------------------------
; Subrutinas de configuración
;-------------------------------------------------------------------------------
config_io:
    banksel ANSEL   ; Acceder al Bank 3
    clrf    ANSEL   ; Selección de pines digitales
    clrf    ANSELH  ; Selección de pines digitales
    
    banksel TRISA   ; Acceder al Bank 1
    clrf    TRISA   ; Luces de semaforos
    clrf    TRISC   ; Display 7seg 
    clrf    TRISD   ; Multiplexado de displays
    clrf    TRISE   ; Luces indicadoras
    
    banksel PORTA   ; Acceder al Bank 3
    clrf    PORTA   ; Comenzar luces del semaforo apagado
    clrf    PORTC   ; Comenzar displays apagados
    clrf    PORTD   ; Comenzar el multiplexado apagado
    clrf    PORTE   ; Comenzar luces de indicadores apagados
    return
    
tiempos:   ; Se cargan los valores iniciales de los semaforos
    movlw   10
    movwf   sem1_time
    movlw   15
    movwf   sem2_time
    movlw   20
    movwf   sem3_time
    return
    
config_int:
    Banksel PORTA   ; Acceder al Bank 0
    bsf	GIE	    ; Se habilitan las interrupciones globales
    bsf	PEIE	    ; Se habilitan las interrupciones perifericas
    bsf	T0IE	    ; Se habilitan la interrupción del TMR0
    bcf	T0IF	    ; Se limpia la bandera
    
    Banksel TRISA   ; Acceder al Bank 1
    bsf	TMR1IE	    ; Se habilitan la interrupción del TMR1 Registro PIE1
    bsf	TMR2IE	    ; Se habilitan la interrupción del TMR2 Registro PIE1
    Banksel PORTA   ; Acceder al Bank 0
    
    bcf	TMR1IF	    ; Se limpia la bandera Registro PIR1
    bcf	TMR2IF	    ; Se limpia la bandera Registro PIR1
    return  
   
config_reloj:	
    Banksel OSCCON  ; Acceder al Bank 1
    bsf	    IRCF2
    bcf	    IRCF1
    bcf	    IRCF0   ; Configuración del oscilador a 1MHz
    bsf	    SCS	    ; Seleccionar el reloj interno
    return
    
config_tmr0:
    Banksel TRISA   ; Acceder al Bank 1
    bcf	    T0CS    ; Tmr0 funciona con reloj interno
    bcf	    PSA	    ; Prescaler asignado a Timer0
    bsf	    PS2	    
    bsf	    PS1
    bsf	    PS0	    ; Prescaler de 1:2
    reiniciar_tmr0  ; Reiniciar conteo del tmr0
    return

config_tmr1:
    Banksel PORTA   ; Acceder al Bank 0
    bsf	    TMR1ON  ; Habilitar Timer1
    bcf	    TMR1CS  ; Selección del reloj interno
    bsf	    T1CKPS0
    bsf	    T1CKPS1 ; Prescaler de 1:8
    reiniciar_tmr1  ; Reiniciar conteo del tmr1
    return 
    
config_tmr2:
    banksel PORTA   ; Acceder al Bank 0
    bsf TMR2ON	    ; Timer2 is on
    bsf TOUTPS3	    ; Postscaler de 1:16
    bsf TOUTPS2
    bsf TOUTPS1
    bsf TOUTPS0
    bsf T2CKPS1	    ; Prescaler de 1:16
    bsf TOUTPS0
    reiniciar_tmr2  ; Reiniciar conteo del tmr2
    return  
end