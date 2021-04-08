    ;Archivo:	    Proyecto1.s
    ;Dispositivo:   PIC16F887
    ;Autor:	    Margareth Vela
    ;Compilador:    pic-as(v2.31), MPLABX V5.45
    ;
    ;Programa:	    Proyecto Semáforo de 3 vías
    ;Hardware:	    Displays 7 seg en PORTC, transistores en PORTD,
    ;		    leds en PORTA, PORTE y PORTB & push buttons en PORTB
    ;Creado:	    21 mar 2021
    ;Última modificación: 05 abr, 2021
    
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
    banksel PORTA	
    movlw   255		; Cargar valor de registro W, valor inicial del tmr0
    movwf   TMR0	; Mover el valor de W a TMR0 por interrupción
    bcf	    T0IF	; Limpiar bandera de interrupción por overflow	
    endm
    
reiniciar_tmr1 macro	; Reinicio de Timer1
    banksel PORTA	
    movlw   0x85	; Cargar valor de registro W, valor inicial del tmr1
    movwf   TMR1H	; Mover el valor de W a TMR1H
    movlw   0xEE	; Cargar valor de registro W, valor inicial del tmr1
    movwf   TMR1L	; Mover el valor de W a TMR1L
    bcf	    TMR1IF	; Limpiar bandera de interrupción por overflow	
    endm
     
;-------------------------------------------------------------------------------
; Variables 
;-------------------------------------------------------------------------------
Global cont_small, sem1_time, sem1_time_amarillo
PSECT udata_bank0	; Variables en banco 0
    vias:	DS 1	;indicar la via 1, 2 o 3
    
    via_sem:	DS 1   ;guarda el valor de tiempo temporal
    bandera_via_1:	DS 1   ;Indica si estoy en verde, parpadeo o amarillo
    sem1_time_temp: DS 1 ;Variable para controlar las leds de semaforo 1
    sem2_time_temp: DS 1 ;Variable para controlar las leds de semaforo 2
    sem3_time_temp: DS 1 ;Variable para controlar las leds de semaforo 3
    
    tiempo_inicial_sem1: DS 1 ;Para guardar los valores iniciales de los semaforos
    tiempo_inicial_sem2: DS 1
    tiempo_inicial_sem3: DS 1
    
    sem1_time_rojo: DS 1  ;Para indicar los tiempos en rojo
    sem2_time_rojo: DS 1
    sem3_time_rojo: DS 1
    
    sem1_time_amarillo: DS 1
    sem2_time_amarillo: DS 1
    sem3_time_amarillo: DS 1
        
    modo:	DS 1	;bandera de modo
    var:	DS 1	;Para configurar el display de modo
    unidades:	DS 1	;Para sacar el valor en decimal del modo
    decenas:	DS 1
    modo_d:	DS 1	;Para display de decenas de modo
    modo_u:	DS 1	;Para display de unidades de modo
    var_reset:  DS 1	;Sirve para el reseteo de semáforos
    
    sem_t1:	DS 1  ;Para display de tiempo de semáforo 1
    decenas_s1: DS 1  ;Para sacar el valor en decimal del tiempo de sem1
    unidades_s1:DS 1  
    sem1_d:	DS 1  ;Para display de decenas de semáforo 1
    sem1_u:	DS 1  ;Para display de unidades de semáforo 1
    
    sem_t2:	DS 1  ;Para display de tiempo de semáforo 2
    decenas_s2: DS 1  ;Para sacar el valor en decimal del tiempo de sem2
    unidades_s2:DS 1
    sem2_d:	DS 1  ;Para display de decenas de semáforo 2
    sem2_u:	DS 1  ;Para display de unidades de semáforo 2
    
    sem_t3:	DS 1  ;Para display de tiempo de semáforo 3
    decenas_s3: DS 1  ;Para sacar el valor en decimal del tiempo de sem3
    unidades_s3:DS 1
    sem3_d:	DS 1  ;Para display de decenas de semáforo 3
    sem3_u:	DS 1  ;Para display de unidades de semáforo 3
    
    sem1_time:	DS 1	;tiempos de semáforos
    sem2_time:	DS 1
    sem3_time:	DS 1
    
    flags:	DS 1	;Para determinar que display se enciende
    cont_small: DS 2	;Para delay de parpadeo

    tiempo_sem_1: DS 1 ;para configurar tiempos 
    tiempo_sem_2: DS 1
    tiempo_sem_3: DS 1
    
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
    btfss   RBIF	; Si está encendida la bandera, entonces 
    goto    pop		; entonces se salta a revisar los modos
    
    btfsc   modo, 0  ;tenemos la bandera de modo 0, 1, 2, 3, 4
    goto    modo_0_int
    btfsc   modo, 1
    goto    modo_1_int
    btfsc   modo, 2
    goto    modo_2_int
    btfsc   modo, 3
    goto    modo_3_int
    btfsc   modo, 4
    goto    modo_4_int
    
pop:
    swapf   STATUS_TEMP, W
    movwf   STATUS
    swapf   W_TEMP, F
    swapf   W_TEMP, W
    RETFIE
   
;-------------------------------------------------------------------------------
; Sub rutinas para interrupciones
;-------------------------------------------------------------------------------
modo_0_int:
    btfss   PORTB, 2  ; Si está presionado el push del bit 2, 
    call    cambiar_modo_a_1     ; cambia el modo a 1
    bcf	    RBIF      ; Se limpia la bandera de IOC
    goto    pop	     
    
cambiar_modo_a_1:
    clrf    modo	;Limpia la bandera de modo
    bsf	    modo, 5	;Enciende el display
    bsf	    modo, 1	;Cambia de modo
    return
    
modo_1_int: 
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    call    incrementar_sem1     ; incrementa la variable de tiempo del sem1
    btfss   PORTB, 1  ; Si está presiona el push del bit 1,
    call    decrementar_sem1    ; decrementa la variable de tiempo del sem1
    btfss   PORTB, 2  ; Si está presionado el push del bit 2,
    call    cambiar_modo_a_2 ;cambia de modo
    bcf	    RBIF      ; Se limpia la bandera de IOC
    goto pop 

incrementar_sem1:
    movf tiempo_sem_1, W    ;Mueve el valor actual del tiempo del sem1 a W
    sublw 20 ;Le resta 20 para saber si ya llegó al valor máximo
    btfss STATUS, 2 ;si ZERO no está encendido entonces incrementa 
    incf  tiempo_sem_1, F
    btfss STATUS, 2 ;y regresa a la subrutina de modo 1
    goto $+3
    movlw 10 ;si ZERO está encendido entonces llegó al valor máximo y 
    movwf tiempo_sem_1	;mueve el valor de 10 a tiempo del sem1
    return  ;regresa a la subrutina de modo 1
    
decrementar_sem1:
    movf tiempo_sem_1, W ;Mueve el valor actual del tiempo del sem2 a W
    sublw 10  ;Le resta 10 para saber si ya llegó al valor mínimo 
    btfss STATUS, 2 ;si ZERO no está encendido entonces decrementa 
    decf tiempo_sem_1, F
    btfss STATUS, 2 ;y regresa a la subrutina de modo 1
    goto $+3 
    movlw 20  ;si ZERO está encendida entonces llegó al valor mínimo y
    movwf tiempo_sem_1 ;mueve el valor de 20 a tiempo del sem1
    return ;regresa a la subrutina de modo 1
    
cambiar_modo_a_2:
    clrf    modo
    bsf	    modo, 2  ; cambia de modo   
    bsf	    modo, 5  ; enciende el display
    return

modo_2_int: 
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    call    incrementar_sem2     ; incrementa la variable de tiempo de sem2
    btfss   PORTB, 1  ; Si está presiona el push del bit 1,
    call    decrementar_sem2    ; decrementa la variable de tiempo de sem2
    btfss   PORTB, 2  ; Si está presionado el push del bit 2,
    call    cambiar_modo_a_3 ;cambia de modo
    bcf	    RBIF      ; Se limpia la bandera de IOC
    goto pop
 
;Se repite el proceso de verificación del valor máx y min del tiempo para
;el semáforo 2 y 3
incrementar_sem2:
    movf tiempo_sem_2, W
    sublw 20
    btfss STATUS, 2 ;si ZERO es 0 entonces incrementa 
    incf  tiempo_sem_2, F
    btfss STATUS, 2 ;si ZERO es 1 entonces mueve el valor de 10 a la variable
    goto $+3
    movlw 10
    movwf tiempo_sem_2
    return
    
decrementar_sem2:
    movf tiempo_sem_2, W
    sublw 10
    btfss STATUS, 2 ;si ZERO es 0 entonces decrementa
    decf tiempo_sem_2, F
    btfss STATUS, 2 ;si ZERO es 1 entonces mueve el valor de 20 a la variable
    goto $+3
    movlw 20
    movwf tiempo_sem_2
    return
    
cambiar_modo_a_3:
    clrf    modo
    bsf	    modo, 3  ; cambia de modo   
    bsf	    modo, 5  ; enciende el display
    return

modo_3_int: 
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    call    incrementar_sem3     ; incrementa la variable de tiempo de sem3
    btfss   PORTB, 1  ; Si está presiona el push del bit 1,
    call    decrementar_sem3    ; decrementa la variable de tiempo de sem3
    btfss   PORTB, 2  ; Si está presionado el push del bit 2,
    call    cambiar_modo_a_4 ;cambia de modo
    bcf	    RBIF      ; Se limpia la bandera de IOC
    goto pop

incrementar_sem3:
    movf tiempo_sem_3, W
    sublw 20
    btfss STATUS, 2 ;si ZERO es 0 entonces incrementa 
    incf  tiempo_sem_3, F
    btfss STATUS, 2 ;si ZERO es 1 entonces mueve el valor de 10 a la variable
    goto $+3
    movlw 10
    movwf tiempo_sem_3
    return
    
decrementar_sem3:
    movf tiempo_sem_3, W
    sublw 10
    btfss STATUS, 2 ;si ZERO es 0 entonces decrementa 
    decf tiempo_sem_3, F
    btfss STATUS, 2 ;si ZERO es 1 entonces mueve el valor de 20 a la variable
    goto $+3
    movlw 20
    movwf tiempo_sem_3
    return
    
cambiar_modo_a_4:
    clrf    modo
    bsf	    modo, 4  ; cambia de modo   
    bsf	    modo, 5  ; enciende el display
    return

modo_4_int: 
    btfss   PORTB, 0  ; Si está presiona el push del bit 0,
    call    aceptar     ; significa que se quiere aceptar los nuevos cambios
    btfss   PORTB, 1  ; Si está presiona el push del bit 1,
    call    denegar    ; significa que se quiere denegar los nuevos cambios
    bcf	    RBIF      ; Se limpia la bandera de IOC
    goto    pop

aceptar:
    movlw 1		;Mueve el valor del tiempo que se mantendrá el reseteo
    movwf var_reset
    bsf   modo, 6       ;Enciende la bandera que hace la rutina de reset en loop
    clrf  vias		;Limpia las banderas para que no se ejecute ningún cambio
    clrf  via_sem	;en los semáforos
    return
    
denegar:    
    movlw   10		    ;Regresa el valor mínimo a las variables de 
    movwf   tiempo_sem_1    ;configuración de tiempo
    movwf   tiempo_sem_2
    movwf   tiempo_sem_3
    clrf    modo
    bcf	    PORTB, 7
    bsf	    modo, 0  ; cambia de modo
    return
        
;-------------------------------------------------------------------------------
; Comienza funcionamiento de TMR1
;-------------------------------------------------------------------------------
int_tmr1:
    reiniciar_tmr1	    ;Se reinicia el TMR1
    btfsc   modo, 6	    ;Revisa si hace la rutina de reseteo
    call    delay_reseteo
    btfsc   vias, 0	    ;Revisa si el semáforo de la vía 1 está en verde
    call    restar_via_1
    btfsc   vias, 1	    ;Revisa si el semáforo de la vía 2 está en verde
    call    restar_via_2    
    btfsc   vias, 2	    ;Revisa si el semáforo de la vía 3 está en verde
    call    restar_via_3
    return

;-------------------------------------------------------------------------------
; Sub rutinas para TMR1
;-------------------------------------------------------------------------------
delay_reseteo:
    decfsz   var_reset, F ;Va a ejecutarse dos veces este delay
    return
    bsf	     modo, 7	  ;Cuando ya haya pasado ese tiempo entonces termina
    return		  ;el reset
    
restar_via_1:
    btfsc bandera_via_1, 3 ;Revisa si está en amarillo
    decf  sem1_time_amarillo
    btfss bandera_via_1, 3 ;Revisa si está en verde
    decf sem1_time	;Decrementar las variables del  tiempo de los
    decf sem1_time_temp	;displays, así como la variable temporal del sem1
    decf sem2_time_rojo
    decf sem3_time_rojo
    
    movlw 6		    ;Revisa si ya llegó a 6 la variable temporal
    subwf sem1_time_temp    
    btfsc bandera_via_1,0   ;Si la bandera de parpadeo está encendida
    addwf sem1_time_temp    ;entonces automáticamente le regresa 6 a la variable
    btfsc STATUS,2	    ;Si ya llegó a 6 entonces se prende la bandera
    call  cambio_a_parpadeo ;que indica que ya comienza el parpadeo 
    btfss bandera_via_1,0   ;Si la bandera de parpadeo está apagada 
    addwf sem1_time_temp    ;entonces automáticamente le regresa 6 a la variable
        
    movlw 0x03		    ;Revisa si ya llegó a 3 la variable temporal
    subwf sem1_time_temp    
    btfsc bandera_via_1,1   ;Si la bandera de amarillo está encendida entonces
    addwf sem1_time_temp    ;automáticamente le regresa 3 a la variable
    btfsc bandera_via_1,1   ;y salta a verificar si ya llegó a 0
    goto  $+5
    btfsc STATUS,2	    ;Si ya llegó a 3 entonces se prende la bandera
    call  cambio_a_amarillo ;que indica que ya comienza el tiempo en amarillo
    btfss bandera_via_1,1   ;Si la bandera de amarillo está apagada entonces 
    addwf sem1_time_temp    ;automáticamente le regresa 3 a la variable
    
    movf sem1_time_temp, W  ;Revisa si ya llegó a 0 la variable temporal
    btfsc STATUS,2	;Si ya es 0 entonces hace el cambio de vías
    call  cambio_de_via  
    return
      
  cambio_a_parpadeo:
    bsf	  bandera_via_1, 0  ;Enciende la bandera que indica que ya comienza 
    addwf sem1_time_temp    ;la rutina de parpadeo
    return
 
    
  cambio_a_amarillo:
    bcf	  bandera_via_1, 0  ;Limpia la bandera de parpadeo y enciende la
    bsf	  bandera_via_1, 3
    bsf	  bandera_via_1, 1  ;bandera de la rutina de amarillo
    addwf sem1_time_temp    ;le regresa 3 a la variable temporal
    return
    
  cambio_de_via:
    bcf	  bandera_via_1, 1  ;Limpia la bandera de amarillo y enciende la 
    bsf	  bandera_via_1, 2  ;bandera de cambio de vía
    return

;Se realiza el mismo procedimiento del semáforo 1 para el semáforo 2 y 3
restar_via_2:
    btfsc bandera_via_1, 3
    decf  sem2_time_amarillo
    btfss bandera_via_1, 3
    decf sem2_time 
    decf sem2_time_temp
    decf sem1_time_rojo
    decf sem3_time_rojo
    
    movlw 6
    subwf sem2_time_temp
    btfsc bandera_via_1,0
    addwf sem2_time_temp
    btfsc STATUS,2
    call  cambio_a_parpadeo_2
    btfss bandera_via_1,0
    addwf sem2_time_temp
        
    movlw 0x03
    subwf sem2_time_temp
    btfsc bandera_via_1,1
    addwf sem2_time_temp
    btfsc bandera_via_1,1
    goto  $+5
    btfsc STATUS,2
    call  cambio_a_amarillo_2
    btfss bandera_via_1,1
    addwf sem2_time_temp
    
    movf sem2_time_temp, W
    btfsc STATUS,2
    call  cambio_de_via_2 ;hacer cambios de vía
    return
    
  cambio_a_parpadeo_2:
    bsf	  bandera_via_1, 0
    addwf sem2_time_temp
    return
 
    
  cambio_a_amarillo_2:
    bcf	  bandera_via_1, 0
    bsf	  bandera_via_1, 3
    bsf	  bandera_via_1, 1
    addwf sem2_time_temp
    return
    
  cambio_de_via_2:
    bcf	  bandera_via_1, 1
    bsf	  bandera_via_1, 2
    return
    
 restar_via_3:
    btfsc bandera_via_1, 3
    decf  sem3_time_amarillo
    btfss bandera_via_1, 3
    decf sem3_time 
    decf sem3_time_temp
    decf sem1_time_rojo
    decf sem2_time_rojo
    
    movlw 6
    subwf sem3_time_temp
    btfsc bandera_via_1,0
    addwf sem3_time_temp
    btfsc STATUS,2
    call  cambio_a_parpadeo_3
    btfss bandera_via_1,0
    addwf sem3_time_temp
        
    movlw 0x03
    subwf sem3_time_temp
    btfsc bandera_via_1,1
    addwf sem3_time_temp
    btfsc bandera_via_1,1
    goto  $+5
    btfsc STATUS,2
    call  cambio_a_amarillo_3
    btfss bandera_via_1,1
    addwf sem3_time_temp
    
    movf sem3_time_temp, W
    btfsc STATUS,2
    call  cambio_de_via_3 ;hacer cambio de vía
    return
    
  cambio_a_parpadeo_3:
    bsf	  bandera_via_1, 0
    addwf sem3_time_temp
    return
 
    
  cambio_a_amarillo_3:
    bcf	  bandera_via_1, 0
    bsf	  bandera_via_1, 3
    bsf	  bandera_via_1, 1
    addwf sem3_time_temp
    return
    
  cambio_de_via_3:
    bcf	  bandera_via_1, 1
    bsf	  bandera_via_1, 2
    return
 
;-------------------------------------------------------------------------------
; Comienza funcionamiento de TMR0
;-------------------------------------------------------------------------------
int_tmr0:
    reiniciar_tmr0		; Reiniciar el TMR0
    clrf    PORTD		; Limpiar PORTD
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
    goto    display_unidades_modo; enciende las unidades del modo
    btfsc   flags, 7		; Revisa el bit de la bandera que 
    goto    display_decenas_modo; enciende las decenas del modo

display_unidades_s1:
    movf    sem1_u, W	; Se mueve el valor de la variable al registro W
    movwf   PORTC	; Colocar el valor en el PORTC
    bsf	    PORTD, 3	; Se setea el transistor 
    clrf    flags	; Preparar para siguiente display
    bsf	    flags, 1
    return
    
display_decenas_s1:
    movf    sem1_d, W	; Se mueve el valor de la variable al registro W
    movwf   PORTC	; Colocar el valor en el PORTC
    bsf	    PORTD, 2	; Se setea el transistor
    clrf    flags	; Preparar para siguiente display
    bsf	    flags, 2
    return
        
display_unidades_s2:
    movf    sem2_u, W	; Se mueve el valor de la variable al registro W
    movwf   PORTC	; Colocar el valor en el PORTC
    bsf	    PORTD, 5	; Se setea el transistor
    clrf    flags	; Preparar para siguiente display
    bsf	    flags, 3
    return
    
display_decenas_s2:
    movf    sem2_d, W	; Se mueve el valor de la variable al registro W
    movwf   PORTC	; Colocar el valor en el PORTC
    bsf	    PORTD, 4	; Se setea el transistor
    clrf    flags	; Preparar para siguiente display
    bsf	    flags, 4
    return
    
display_unidades_s3:
    movf    sem3_u, W	; Se mueve el valor de la variable al registro W
    movwf   PORTC	; Colocar el valor en el PORTC
    bsf	    PORTD, 7	; Se setea el transistor
    clrf    flags	; Preparar para siguiente display
    bsf	    flags, 5
    return

display_decenas_s3:
    movf    sem3_d, W	; Se mueve el valor de la variable al registro W
    movwf   PORTC	; Colocar el valor en el PORTC
    bsf	    PORTD, 6	; Se setea el transistor
    clrf    flags	; Preparar para siguiente display
    bsf	    flags, 6
    return
            
display_unidades_modo:
    btfss   modo, 5 ;Indica si es necesario el display de modo  
    goto    $+4
    movf    modo_u, W	; Se mueve el valor de la variable al registro W
    movwf   PORTC	; Colocar el valor en el PORTC
    bsf	    PORTD, 1	; Se setea el transistor
    clrf    flags	; Preparar para siguiente display
    bsf	    flags, 7
    return
    
display_decenas_modo:
    btfss   modo, 5 ;Indica si es necesario el display de modo
    goto    $+4
    movf    modo_d, W	; Se mueve el valor de la variable al registro W
    movwf   PORTC	; Colocar el valor en el PORTC
    bsf	    PORTD, 0	; Se setea el transistor
    clrf    flags	; Preparar para siguiente display
    bsf	    flags, 0
    return
    
;-------------------------------------------------------------------------------
; Código Principal
;-------------------------------------------------------------------------------
PSECT code, delta=2, abs
ORG 170h ;Posición para el código

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
    call    config_int		; Configuración de las interrupciones
    call    config_IOC		; Configuración del IOC de PORTB
    call    tiempos		; Cargan los valores iniciales
    call    default_tiempos_config  ;Cargan los valores inciales de configuración
    call    default_semaforos	;Enciende la led verde de sem1 y rojas de sem2 y sem3

;-------------------------------------------------------------------------------
; Loop principal
;-------------------------------------------------------------------------------
loop: 
    btfsc   modo, 1 ;Revisa en que modo de configuración estoy
    call    modo_1
    btfsc   modo, 2
    call    modo_2
    btfsc   modo, 3
    call    modo_3
    btfsc   modo, 6 ;Revisa si se ejecuta el reset
    goto    reseteo
    btfsc   vias, 0; Revisa en qué vía/semáforo estoy
    call    via_1  ; 
    btfsc   vias, 1
    call    via_2
    btfsc   vias, 2
    call    via_3
    
    goto    loop
;-------------------------------------------------------------------------------
; Subrutinas para modos de configuración
;------------------------------------------------------------------------------- 
modo_1:
    bsf PORTB, 5    ;Enciende la primer led de configuración
    bcf PORTB, 6
    bcf PORTB, 7
    movf tiempo_sem_1, w ;Mueve el valor de la variable de tiempo de 
    movwf var		 ;configuración de sem1 al display de modo
    call modo_config
    return
    
modo_2:
    bcf PORTB, 5    ;Enciende la segunda led de configuración
    bsf PORTB, 6
    bcf PORTB, 7
    movf tiempo_sem_2, w ;Mueve el valor de la variable de tiempo de 
    movwf var		 ;configuración de sem2 al display de modo
    call modo_config
    return
    
modo_3:
    bcf PORTB, 5    ;Enciende la tercer led de configuración
    bcf PORTB, 6
    bsf PORTB, 7
    movf tiempo_sem_3, w ;Mueve el valor de la variable de tiempo de 
    movwf var		 ;configuración de sem3 al display de modo
    call modo_config
    return

reseteo:
    clrf PORTA	    ;Limpia los puertos de las leds de los semáforos
    clrf PORTE
    bsf  PORTB, 5   ;Enciende todas las leds de modo de configuración
    bsf  PORTB, 6
    bsf  PORTB, 7
    bsf  PORTA, 0   ;Coloca todos los semáforos en rojo
    bsf  PORTA, 3
    bsf  PORTE, 0
    call reseteo_semaforos ;Se colocan todos los displays en 0
    btfss modo, 7   ;Revisa si ya terminó el delay de reseteo
    goto  $-8
    
    movf tiempo_sem_1, W  ;Mueve el valor de los tiempos a cada tiempo actual
    movwf sem1_time	  ;de cada semáforo
    movf tiempo_sem_2, W
    movwf sem2_time
    movf tiempo_sem_3, W
    movwf sem3_time
    
    movlw   10		  ;Le regresa el valor de 10 a los tiempos de 
    movwf   tiempo_sem_1  ;configuración
    movwf   tiempo_sem_2
    movwf   tiempo_sem_3
    
    clrf PORTA	    ;Vuelve a limpiar los puertos
    clrf PORTE
    bsf	 PORTA, 2   ;Coloca el primer semáforo en verde y los otros 2 en rojo
    bsf	 PORTA, 3
    bsf  PORTE, 0
    
    clrf modo	    ;Se limpia la bandera de modo 
    clrf bandera_via_1 ;Se limpipa la bandera de parpadeo y amarillo
    
    bsf  modo, 0    ;Regresa al modo 0
    bsf  vias, 0    ;Empieza la vía 1
    bcf  PORTB, 5   ;Se apagan todas las leds de configuración
    bcf  PORTB, 6
    bcf  PORTB, 7
    goto loop	    ;Regresa al loop principal
    
modo_config:
    clrf    unidades	; Se limpian las variables a utilizar 
    clrf    decenas
        
    movlw 10	     ; Revisión decenas
    subwf var,1 ; Se restan 10 a la variable temporal
    btfsc STATUS, 0 ;Revisión de la bandera de Carry
    incf decenas, 1 ;Si C=1 entonces es menor a 10 y no incrementa la variable
    btfsc STATUS, 0 ;Revisión de la bandera de Carry
    goto $-4
    addwf var,1 ; Se regresa la variable temporal a su valor original
    
    ;Resultado unidades
    movf var, 0 ; Se mueve lo restante en la variable temporal a la
    movwf unidades   ; variable de unidades   
    
    clrf    modo_d
    clrf    modo_u
      
    movf    decenas, 0	
    call    tabla	; Se obtiene el valor correspondiente para el display
    movwf   modo_d	; y se coloca en la variable que se utiliza en el cambio
			; de displays 
    
    movf    unidades, 0
    call    tabla
    movwf   modo_u
    return
    
;-------------------------------------------------------------------------------
; Subrutinas para Semáforos
;-------------------------------------------------------------------------------
via_1:
    btfss via_sem, 0	    ;La primera vez que entra a la rutina guarda
    call  guardar_valor_sem1;el valor del tiempo en verde y en rojo
			    ;para cada semáforo
			    
    call display_semaforo1  ;Se modifican los displays
    call display_semaforo2
    call display_semaforo3
     
    btfsc bandera_via_1, 0 ;Revisa si comienza la rutina de verde_parpadeante
    call  verde_parpadeante
    btfsc bandera_via_1, 1 ;Revisa si comienza la rutina de amarillo
    call  subrutina_de_amarillo
    btfsc bandera_via_1, 2 ;Revisa si comienza la rutina de cambio de vía
    call  cambio_a_via_2
    return
    
verde_parpadeante:
    bcf    PORTA, 2 ;Se apaga la led verde
    call   delay    ;Ambos delays en conjunto hacen aprox. 500 ms
    call   delay
    bsf	   PORTA, 2 ;Se enciende la led verde 
    call   delay
    call   delay
    return

delay:      
    movlw 155		;Se mueve el valor al primer byte de la variable
    movwf cont_small
    movlw 256		;Se mueve el valor al segundo byte de la variable
    movwf cont_small+1
    decfsz cont_small, f ;Se decrementa cada variable
    goto $-1
    decfsz cont_small+1, f
    goto $-1
    return
        
subrutina_de_amarillo:
    bcf PORTA, 2 ;Se apaga la led verde
    bsf	PORTA, 1 ;Se enciende la amarilla
    return
  
cambio_a_via_2:
	bcf PORTA, 1
	bsf PORTA, 0; via1
	bsf PORTA, 5
	bcf PORTA, 3; via2
	clrf vias   ;Se limpia las banderas
	bsf vias, 1 ;cambiar a via 2
	bcf via_sem, 0 ;Se limpia la variable que guarda los valores iniciales
	clrf bandera_via_1
    return
    
guardar_valor_sem1: 
    movf  sem1_time, w	    ;se guarda el valor de sem1_time a una temporal
    movwf sem1_time_temp    ;que sirve para las leds de los semaforos
    movwf tiempo_inicial_sem1
    movwf sem2_time_rojo
    movwf sem3_time_rojo  ;aquí tengo el valor de sem1_time
    movf  sem2_time, w    ;muevo el valor del tiempo del semaforo 2 a w
    addwf sem3_time_rojo  ;le sumo ese valor al tiempo del semaforo 3
    bsf	  via_sem, 0	  ;Prendo la bandera para que no vuelva a guardar los valores
    movlw  3	    
    subwf sem1_time ;Se resta 3 segundos al valor total en verde
    movlw  3
    movwf sem1_time_amarillo ;Se almacenan esos 3 segundos al valor de amarillo
    return 

;Se realiza el mismo procedimiento para las vías 2 y 3    
via_2:
    btfss via_sem, 1
    call  guardar_valor_sem2
     
    call display_semaforo1
    call display_semaforo2
    call display_semaforo3
     
    btfsc bandera_via_1, 0
    call  verde_parpadeante_sem2
    btfsc bandera_via_1, 1
    call  subrutina_de_amarillo_sem2
    btfsc bandera_via_1, 2
    call  cambio_a_via_3
    return
    
verde_parpadeante_sem2:
    bcf    PORTA, 5
    call   delay
    call   delay
    bsf	   PORTA, 5
    call   delay
    call   delay
    return
     
subrutina_de_amarillo_sem2:
    bcf PORTA, 5
    bsf	PORTA, 4
    return
  
cambio_a_via_3:
    bcf PORTA, 4
    bsf PORTA, 3; via2
    bsf PORTE, 2
    bcf PORTE, 0; via3
    bsf vias, 2 ;cambiar a via 3
    bcf vias, 1
    bcf via_sem, 1
    clrf bandera_via_1
    return
    
guardar_valor_sem2: 
    movf  sem2_time, w	    ;se guarda el valor de sem2_time a una temporal
    movwf sem2_time_temp    ;que sirve para las leds de los semaforos
    movwf tiempo_inicial_sem2
    movwf sem3_time_rojo
    movwf sem1_time_rojo  ;aquí tengo el valor de sem2_time
    movf  sem3_time, w    ;muevo el valor del tiempo del semaforo 3 a w
    addwf sem1_time_rojo  ;le sumo ese valor al tiempo del semaforo 1
    bsf	  via_sem, 1
    movlw  3	    
    subwf sem2_time ;Se resta 3 segundos al valor total del semáforo
    movlw 3
    movwf sem2_time_amarillo    ;Se almacenan esos 3 segundos en amarillo
    return 
    
via_3:
    btfss via_sem, 2
    call  guardar_valor_sem3
    
    call display_semaforo1
    call display_semaforo2
    call display_semaforo3
     
    btfsc bandera_via_1, 0
    call  verde_parpadeante_sem3
    btfsc bandera_via_1, 1
    call  subrutina_de_amarillo_sem3
    btfsc bandera_via_1, 2
    call  cambio_a_via_1
    return
    
verde_parpadeante_sem3:
    bcf    PORTE, 2  
    call   delay
    call   delay
    bsf	   PORTE, 2
    call   delay
    call   delay
    return
     
subrutina_de_amarillo_sem3:
    bcf PORTE, 2
    bsf	PORTE, 1
    return
  
cambio_a_via_1:
	bcf PORTA, 0
	bsf PORTA, 2; via1
	bsf PORTE, 0
	bcf PORTE, 1; via3
	bsf vias, 0 ;cambiar a via 3
	bcf vias, 2
	bcf via_sem, 2
	clrf bandera_via_1
	movf tiempo_inicial_sem1, W ;Se mueven los valores iniciales a cada 
	movwf sem1_time		    ;semáforo para que vuelva a comenzar el
	movf tiempo_inicial_sem2, W ;el ciclo
	movwf sem2_time	
	movf tiempo_inicial_sem3, W
	movwf sem3_time
    return
    
guardar_valor_sem3: 
    movf  sem3_time, w	    ;se guarda el valor de sem3_time a una temporal
    movwf sem3_time_temp    ;que sirve para las leds de los semaforos
    movwf tiempo_inicial_sem3
    movwf sem1_time_rojo
    movwf sem2_time_rojo  ;aquí tengo el valor de sem3_time
    movf  tiempo_inicial_sem1, w    ;muevo el valor del tiempo del semaforo 1 a w
    addwf sem2_time_rojo  ;le sumo ese valor al tiempo del semaforo 2
    bsf	  via_sem, 2
    movlw  3	    
    subwf sem3_time ;Se restan 3 segundos al valor total
    movlw 3
    movwf sem3_time_amarillo    ;Se almacenan esos 3 segundos en amarillo
    return 
    
;-------------------------------------------------------------------------------
; Subrutinas para Displays de los semáforos
;-------------------------------------------------------------------------------    
display_semaforo1:
    btfsc   via_sem, 0  ;estoy en vía 1
    call    tiempo_verde_via_1
    btfsc   via_sem, 1 ;estoy en vía 2
    call    tiempo_rojo_via_1
    btfsc   via_sem, 2 ;estoy en vía 3
    call    tiempo_rojo_via_1
        
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
    btfsc   via_sem, 0  ;estoy en vía 1
    call    tiempo_rojo_via_2
    btfsc   via_sem, 1 ;estoy en vía 2
    call    tiempo_verde_via_2
    btfsc   via_sem, 2 ;estoy en vía 3
    call    tiempo_rojo_via_2
    
    
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
    btfsc   via_sem, 0  ;estoy en vía 1
    call    tiempo_rojo_via_3
    btfsc   via_sem, 1 ;estoy en vía 2
    call    tiempo_rojo_via_3
    btfsc   via_sem, 2 ;estoy en vía 3
    call    tiempo_verde_via_3
       
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
; Subrutinas para tiempo de los displays
;-------------------------------------------------------------------------------
tiempo_rojo_via_1:
    movf    sem1_time_rojo, 0	; Mueve el valor del tiempo en rojo del sem1 al registro W
    movwf   sem_t1	; Mueve ese valor a la variable utilizada para 
    return		; el valor en decimal de los displays 
    
tiempo_amarillo_via_1:
    movf    sem1_time_amarillo, 0    ; Mueve el valor del tiempo en verde del sem1 al registro W
    movwf   sem_t1	    ; Mueve el valor a la variable utilizada para
    return
    
tiempo_verde_via_1:
    btfss bandera_via_1, 3
    goto  $+4
    movf  sem1_time_amarillo, 0    ; Mueve el valor del tiempo en amarillo del sem1 al registro W
    movwf sem_t1	    ; Mueve el valor a la variable
    return
    movf  sem1_time, 0    ; Mueve el valor del tiempo en verde del sem1 al registro W
    movwf sem_t1	    ; Mueve el valor a la variable utilizada para
    return		    ;el valor en decimal de los displays
    
tiempo_rojo_via_2:
    movf    sem2_time_rojo, 0	; Mueve el valor del tiempo en rojo del sem2 al registro W
    movwf   sem_t2	; Mueve el valor a la variable utilizada para
    return		;el valor en decimal de los displays
    
tiempo_verde_via_2:
    btfss bandera_via_1, 3
    goto  $+4
    movf  sem2_time_amarillo, 0    ; Mueve el valor del tiempo en amarillo del sem2 al registro W
    movwf sem_t2	    ; Mueve el valor a la variable 
    return
    movf  sem2_time, 0    ; Mueve el valor del tiempo en verde del sem1 al registro W
    movwf sem_t2	    ; Mueve el valor a la variable utilizada para
    return		    ;el valor en decimal de los displays		
 
tiempo_rojo_via_3:
    movf    sem3_time_rojo, 0	; Mueve el valor del tiempo en rojo del sem3 al registro W
    movwf   sem_t3	; Mueve el valor a la variable utilizada para
    return		;el valor en decimal de los displays
        
tiempo_verde_via_3:
    btfss bandera_via_1, 3
    goto  $+4
    movf  sem3_time_amarillo, 0    ; Mueve el valor del tiempo en amarillo del sem3 al registro W
    movwf sem_t3	    ; Mueve el valor a la variable
    return
    movf  sem3_time, 0    ; Mueve el valor del tiempo en verde del sem1 al registro W
    movwf sem_t3	    ; Mueve el valor a la variable utilizada para
    return		    ;el valor en decimal de los displays		
    
reseteo_semaforos:
    movlw 0	    ;Los 3 displays muestran el valor de 0
    movwf sem_t1
    movwf sem_t2
    movwf sem_t3
    call  display_semaforo1
    call  display_semaforo2
    call  display_semaforo3
    return
;-------------------------------------------------------------------------------
; Subrutinas de configuración
;-------------------------------------------------------------------------------
config_io:
    banksel ANSEL   
    clrf    ANSEL   ; Selección de pines digitales
    clrf    ANSELH  ; Selección de pines digitales
    
    banksel TRISA   
    clrf    TRISA   ; Luces de semáforos 1 y 2
    clrf    TRISC   ; Display 7seg 
    clrf    TRISD   ; Multiplexado de displays
    clrf    TRISE   ; Luces de semáforo 3
    
    ;Para modos de configuración
    bsf	    TRISB, 0 ;Push button de incremento
    bsf	    TRISB, 1 ;Push button de decremento 
    bsf	    TRISB, 2 ;Push button de modo de configuración 
    bcf	    TRISB, 5 ;Salida de modo 1 
    bcf	    TRISB, 6 ;Salida de modo 2
    bcf	    TRISB, 7 ;Salida de modo 3
    
    bcf	    OPTION_REG, 7 ;Habilitar pull-ups
    bsf	    WPUB, 0 ;Push button de incremento
    bsf	    WPUB, 1 ;Push button de decremento
    bsf	    WPUB, 2 ;Push button de modo de configuración
    
    banksel PORTA   
    clrf    PORTA   ; Comenzar luces del semaforo 1 y 2 apagado
    clrf    PORTB
    clrf    PORTC   ; Comenzar displays apagados
    clrf    PORTD   ; Comenzar el multiplexado apagado
    clrf    PORTE   ; Comenzar luces del semaforo 3 apagado
    
    bsf	    vias, 0 ;Comienza la vía 1
    bsf	    modo, 0 ;Comienza el modo 0
    clrf    cont_small ;Se limpia la variable del delay
    return
    
default_tiempos_config:
    movlw 10		;Se coloca el tiempo de la configuración de los modos
    movwf tiempo_sem_1
    movwf tiempo_sem_2
    movwf tiempo_sem_3
    return
    
default_semaforos: ;Comienza en verde el primer semáforo
    bsf PORTA, 2; via1 
    bsf PORTA, 3; via2
    bsf PORTE, 0; via3
    return
    
tiempos:   ; Se cargan los valores iniciales de los semáforos
    movlw   10
    movwf   sem1_time
    movwf   sem2_time
    movwf   sem3_time
    return
    
config_int:
    Banksel PORTA   
    bsf	GIE	    ; Se habilitan las interrupciones globales
    bsf	PEIE	    ; Se habilitan las interrupciones perifericas
    bsf	RBIE	; Se habilita la interrupción de las resistencias pull-ups 
    bcf	RBIF	; Se limpia la bandera
    bsf	T0IE	    ; Se habilitan la interrupción del TMR0
    bcf	T0IF	    ; Se limpia la bandera
    
    Banksel TRISA   
    bsf	TMR1IE	    ; Se habilitan la interrupción del TMR1 Registro PIE1
    Banksel PORTA   
    bcf	TMR1IF	    ; Se limpia la bandera del TMR1
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
    
config_reloj:	
    Banksel OSCCON  
    bsf	    IRCF2
    bcf	    IRCF1
    bcf	    IRCF0   ; Configuración del oscilador a 1MHz
    bsf	    SCS	    ; Seleccionar el reloj interno
    return
    
config_tmr0:
    Banksel TRISA   
    bcf	    T0CS    ; TMR0 funciona con reloj interno
    bcf	    PSA	    ; Prescaler asignado a Timer0
    bsf	    PS2	    
    bsf	    PS1
    bsf	    PS0	    ; Prescaler de 1:256
    reiniciar_tmr0  ; Reiniciar conteo del tmr0
    return

config_tmr1:
    Banksel PORTA   
    bsf	    TMR1ON  ; Habilitar Timer1
    bcf	    TMR1CS  ; Selección del reloj interno
    bsf	    T1CKPS0
    bsf	    T1CKPS1 ; Prescaler de 1:8
    reiniciar_tmr1  ; Reiniciar conteo del tmr1
    return 
end