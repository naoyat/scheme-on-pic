(require "./macro-asm-plugin")

;;
;; [plug-in] A2D
;;
(define (plug-in:A2D)
  ;; command implementations
  (define (do-convert-code . args)
	;; RA5 RA4 RA3 RC5 RC4 RC3 RA0 RA1 RA2 RC0 RC1 RC2 +5V GND
	;;  -  <3>  -   -   -  <7> <0> <1> <2> <4> <5> <6>
	;;         SW1         LED RP1          LED LED LED
	;;
	;; ※RP1 = 可変抵抗
	;;   SW1 = スイッチ

	(let ([port (if (null? args) 'AN0 (car args))] ; CHS
		  [ch 0]
		  [t 8]) ; [1 - 6], Tosc = 2^t => ADCS
	  `(,(let1 clock (case t
					   [(2) #b000]
					   [(4) #b100]
					   [(8) #b001]
					   [(16) #b101]
					   [(32) #b010]
					   [(64) #b110]
					   [(A/D RC) #b011])
		   `(set ADCON1 ,(ash clock 4))) ; A2D Clock Fosc/8B ; 01100000とか

		,(case port
		   [(AN0  RA0)
			(set! ch 0)
			'(asm (BSF TRISA 0) ; AN0 = RA0
				  (BSF ANSEL 0))]
		   [(AN1 RA1)
			(set! ch 1)
			'(asm (BSF TRISA 1) ; AN1 = RA1
				  (BSF ANSEL 1))]
		   [(AN2 RA2)
			(set! ch 2)
			'(asm (BSF TRISA 2) ; AN2 = RA2
				  (BSF ANSEL 2))]
		   [(AN3 RA4)
			(set! ch 3)
			'(asm (BSF TRISA 4) ; AN3 = RA4
				  (BSF ANSEL 3))]
		   [(AN4 RC0)
			(set! ch 4)
			'(asm (BSF TRISC 0) ; AN4 = RC0
				  (BSF ANSEL 4))]
		   [(AN5 RC1)
			(set! ch 5)
			'(asm (BSF TRISC 1) ; AN5 = RC1
				  (BSF ANSEL 5))]
		   [(AN6 RC2)
			(set! ch 6)
		   '(asm (BSF TRISC 2) ; AN6 = RC2
				 (BSF ANSEL 6))]
		   [(AN7 RC3)
			(set! ch 7)
			'(asm (BSF TRISC 3) ; AN7 = RC3
				  (BSF ANSEL 7))]
		   [(AN8 RC6)
			(set! ch 8)
			'(asm (BSF TRISC 6) ; AN8 = RC6
				  (BSF ANSELH 0))]
		   [(AN9 RC7)
			(set! ch 9)
			'(asm (BSF TRISC 7) ; AN9 = RC7
				  (BSF ANSELH 1))]
		   [(AN10 RB4)
			(set! ch 10)
			'(asm (BSF TRISB 4) ; AN10 = RB4
				  (BSF ANSELH 2))]
		   [(AN11 RB5)
			(set! ch 11)
			'(asm (BSF TRISB 5) ; AN11 = RB5
				  (BSF ANSELH 3))])
		
		(set! ADCON0 ,(logior (ash 0 7)  ; ADFM - 0 (Left justified) || 1 (right justified)
							  (ash 0 6)  ; VCFG - 0 (Vdd) || 1 (Vref)
							  (ash ch 2) ; CHS (Channel) - [0-15]
							  (ash 0 1)  ; GO/^DONE - 1 (GO) || DONE (0)
							  1)) ; ADON - turn on the A2D module
		;; wait 5uS for A2D amp to settle and capacitor to charge.
		(asm (NOP)
			 (NOP)
			 (NOP)
			 (NOP)
			 (NOP)

			 (BSF   ADCON0 1)   ; start conversion (1=GO for 690)
			 (BTFSC ADCON0 1)   ; this bit will change to zero when the conversion is complete
			 (GOTO  $-1)
	  ;;	,@([code'change-bank] 0 1)

			 (BSF   STATUS RP0) ; bank 1
			 (MOVF  ADRESL W)
			 (BCF   STATUS RP0)
			 (MOVWF a2d-lower)

			 (MOVF  ADRESH W) ; bank 0
			 (MOVWF a2d-upper)
			 ; Wに上位８ビットが残る仕様
			 ))))

  (make-plugin
   ;; registers to allocate
   '(a2d-upper a2d-lower)
   ;; init code
   '()
   ;; subroutines
   '()
   ;; plugins
   (list 'a2d
		 (lambda args (apply do-convert-code args)))
   ))
