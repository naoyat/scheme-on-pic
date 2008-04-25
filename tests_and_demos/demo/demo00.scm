;;
;; demo11 scm-object
;;
(require "./demo-base")

(define code-lesson6 '(
			   (alloc  Delay)               ; Assign an address to label Delay1
			   (alloc  Display)             ; define a variable to hold the diplay
			   (alloc  LastStableState)     ; keep track of switch state (open-1; closed-0)
			   (alloc  Counter)
     
			 start
			   (BSF    STATUS RP0)         ; select Register Page 1
			   (MOVLW  #xFF)
			   (MOVWF  TRISA)              ; Make PortA all input
			   (CLRF   TRISC)              ; Make PortC all output

			   (BCF    STATUS RP0)         ; address Register Page 2
			   (BSF    STATUS RP1)
			   (MOVLW  #xF7)               ; PortA3 pin is digital
			   (MOVWF  ANSEL)
;			   (BCF    STATUS RP0)         ; address Register Page 0
			   (BCF    STATUS RP1)
     
			   (CLRF   Display)
			   (CLRF   PORTC)

			   (BSF    LastStableState 0)
			   (MOVLW  1)
			   (MOVWF  LastStableState)    ; Assume the Switch is up.
;			   (CLRF   Counter)
			 MainLoop
			   (INCF   Counter F) ;;;
;			   (BTFSS  LastStableState 0)
			   (BTFSC  LastStableState 0)
			   (GOTO   LookingForUp)
			 LookingForDown
			   (INCF   Counter F)
			   (BTFSC  PORTA 3)
			   (CLRF   Counter)
;			   (CLRW)                      ; assume it's not, so clear
;			   (BTFSS  PORTA 3)            ; wait for switch to go low
;			   (INCF   Counter W)          ; if it's low, bump the counter
;			   (MOVWF  Counter)            ; store either the 0 or incremented value
			   (GOTO   EndDebounce)
     
			 LookingForUp
			   (INCF   Counter F)
			   (BTFSS  PORTA 3)
			   (CLRF   Counter)
;			   (CLRW)                      ; assume it's not, so clear
;			   (BTFSC  PORTA 3)            ; wait for switch to go low
;			   (INCF   Counter W)
;			   (MOVWF  Counter)

			 EndDebounce
			   (MOVF   Counter W)          ; have we seen 10 in a row?
			   (XORLW  51)
			   (BTFSS  STATUS Z)
			   (GOTO   Delay1mS)
     
			   (COMF   LastStableState F)  ; after 10 straight, reverse the direction
			   (CLRF   Counter)
			   (BTFSS  LastStableState 0)  ; Was it a key-down press?
			   (GOTO   Delay1mS)           ; no: take no action
     
			   (INCF   Display F)          ; if it's the down direction,
			   (MOVF   Display W)          ; take action on the switch

;			   (CLRF   PORTC F);;;;;;
;			   (MOVLW  15)
;			   (BCF    PORTC 2);;;;;;
			   (MOVWF  PORTC)              ; (increment counter and put on display)
     
			 Delay1mS
			   (MOVLW  71)                 ; delay ~1000uS
			   (MOVWF  Delay)
			   (DECFSZ Delay F)            ; this loop does 215 cycles
			   (GOTO   $-1)
			   (DECFSZ Delay F)            ; This loop does 786 cycles
			   (GOTO   $-1)
			   (GOTO   MainLoop)
;			   (end)
			   ))

;(define *disable-writing* #t)
(macro-asm-test '()
				code-lesson6 #t #t #t 10)
;				code-fib305 #t #t #t 10)

;; 1 1 2 3 5 8 13 21
;; 34 55 89 144 233ぐらいまで
;; 01 01 02 03 05 08 0D 15
;; 22 37 59 90 E9