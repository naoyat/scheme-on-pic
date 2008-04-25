(require "./macro-asm-plugin")
; requires wait plugin

;;
;; LED // depends on delay-loop
;;
(define (plug-in:LED)
  ;; command implementations
;  (define (display-3210 reg onsec offsec)
;  (define (display-4bit onsec offsec)
;	`(;(asm   ;(MOVF  ,reg W)
;	  (MOVWF  PORTC)
;	  (wait   ,onsec)
;	  (CLRF   PORTC)
;	  (wait  ,offsec)))
;  (define (display-3210 onsec offsec)
;	`((asm   ;(MOVF  ,reg W)
;			 (MOVWF PORTC))
;	  (wait  ,onsec)
;	  (LED/off)
;	  (wait  ,offsec)))
;  (define (display-7654 reg onsec offsec)
;  (define (display-7654 onsec offsec)
;	`((asm   ;(SWAPF ,reg W)
;			 (MOVWF PORTC))  ;PORTC = W = reg
;	  (wait  ,onsec)
;	  (LED/off)
;	  (wait  ,offsec)))

  (define (LED-off)
	`((set    PORTC 0))) ; off

  (define (display-4bit-value val sec)
	`((-- LED-display-4bit-value ,val)
	  (set    PORTC ,(logand #b00001111 val)) ; on
	  (wait   ,sec)
	  (CLRF   PORTC) ; LED/off
	  ))
  
  (define (display-w)
	`((MOVWF  display-w-keeper) ; W値の保存
	  (CALL   display-8bit-value)
	  (MOVF   display-w-keeper W)
	  ))

  (define (display-register reg)
	`((MOVWF  display-w-keeper) ; W値の保存
	  (MOVF   ,reg W)
	  (CALL   display-8bit-value)
	  (MOVF   display-w-keeper W)
	  ))
  
  (define (display-literal lit)
	`((MOVWF  display-w-keeper) ; W値の保存
	  (MOVLW  ,lit)
	  (CALL   display-8bit-value)
	  (MOVF   display-w-keeper W)
	  ))

  ;;
  (make-plugin
   ;; registers to allocate
   '(display-w-keeper)
   ;; init code
   '((set    TRISC  0))
   ;; subroutines
   `((-- display-8bit-value)
  display-8bit-value
	 (MOVWF  temp)   ; temp = W
;	 ,@(display-7654 0.2 0.2)
	 (SWAPF  temp W)
	 (MOVWF  PORTC)  ; PORTC = w = temp<3:0><7:4>
	 (wait   0.2)
	 (CLRF   PORTC)  ; PORTC = 0
	 (wait   0.2)

;	 ,@(display-7654 0.2 0.2)
	 (SWAPF  temp W)
	 (MOVWF  PORTC)  ; PORTC = w = temp<3:0><7:4>
	 (wait   0.2)
	 (CLRF   PORTC)  ; PORTC = 0
	 (wait   0.2)

;	 ,@(display-3210 0.6 0.2)
	 (MOVF   temp W)
	 (MOVWF  PORTC)  ; PORTC = w = temp
	 (wait   0.6)
	 (CLRF   PORTC)  ; PORTC = 0
	 (wait   0.2)
	 (RETURN))

   ;; plugins
   (list 'LED/display-w
		 (lambda () (display-w)))
   (list 'LED/display-reg
		 (lambda (reg) (display-register reg)))
   (list 'LED/display-literal
		 (lambda (lit) (display-literal lit)))

   (list 'LED ;; display 4-bit value
		 (lambda (val sec) ; (display-4bit-value val sec)))
		   `((-- LED-display-4bit-value ,val)
			 (MOVLW  ,(logand #b00001111 val))
			 (MOVWF  PORTC) ; on
			 (wait   ,sec)
			 (CLRF   PORTC))))

   (list 'LED/off ;; display 4-bit value
		 (lambda () (LED-off)))
   ))
