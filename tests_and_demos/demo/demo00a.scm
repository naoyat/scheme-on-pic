;;
;; demo11 scm-object
;;
(require "./demo-base")

(define code-lesson6 '(
			 start
			   (BSF    STATUS RP0)         ; select Register Page 1
;			   (MOVLW  #xFF)
			   (MOVLW  #x09)
			   (MOVWF  TRISA)              ; Make PortA all input
			   (CLRF   TRISC)              ; Make PortC all output

			   (BCF    STATUS RP0)         ; address Register Page 2
			   (BSF    STATUS RP1)
;			   (MOVLW  #xF7)               ; PortA3 pin is digital
;			   (MOVLW  #xF0)               ; PortA3 pin is digital
;			   (MOVWF  ANSEL)
			   (CLRF   ANSEL)
			   (BCF    STATUS RP0)         ; address Register Page 0
			   (BCF    STATUS RP1)

			   (CLRF   PORTA)              ; Make PortC all output
			   (CLRF   PORTC)              ; Make PortC all output
     
			 loop
			   (MOVF   PORTA W)
			   (MOVWF  PORTC)
;			   (NOP)
;			   (NOP)
;			   (NOP)
;			   (NOP)
;			   (NOP)
			   (GOTO   loop)
			   ))

;(define *disable-writing* #t)
(macro-asm-test '()
				code-lesson6 #t #t #t 10)
;				code-fib305 #t #t #t 10)
