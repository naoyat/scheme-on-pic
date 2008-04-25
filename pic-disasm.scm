;;;
;;; pic-disasm
;;;
;;;   Copyright (c) 2007-2008 naoya_t (naoya.t@aqua.plala.or.jp)
;;;

; １命令を逆アセンブル
(define (pic-disasm-inst inst)
  (define (file-register addr . bank)
	(let1 file-registers
		'(INDF TMR0 PCL STATUS FSR PORTA PORTB PORTC
			   PORTD PORTE PCLATH INTCON PIR1 PIR2 TMR1L TMR1H
			   T1CON TMR2 T2CON SSPBUF SSPCON CCPR1L CCPR1H CCP1CON
			   RCSTA TXREG RCREG CCPR2L CCPR2H CCP2CON ADRESH ADCON0)
	  (if (< addr #x20)
		  (ref file-registers addr)
		  (format "<reg~d>" addr))
	  ))

  (case (ash inst -12)
	((0)
	 (let ([opcode (ash inst -8)]
		   [to-file? (logbit? 7 inst)] ; #t:f #f:W
		   [file (logand inst #b01111111)])
	   (let1 mnemonic
		   (case opcode
			 ((0)
			  (if to-file? 'MOVWF
				  (if (= 0 (logand inst #b11111110011111)) 'NOP
					  (if (= #b00000000001000 inst) 'RETURN
						  (if (= #b00000000001001 inst) 'RETFIE
							  (if (= #b00000001100011 inst) 'SLEEP
								  (if (= #b00000001100100 inst) 'CLRWDT
									  #f)))))))
;			  (cond ((to-file? 'MOVWF) ; Move W to f
;					 ((= 0 (logand inst #b11111110011111)) 'NOP) ; No Operation
;					 ((= #b00000000001000 inst) 'RETURN) ; Return from Subroutine
;					 ((= #b00000000001001 inst) 'RETFIE) ; Return from interrupt
;					 ((= #b00000001100011 inst) 'SLEEP)  ; Go into Standby mode
;					 ((= #b00000001100100 inst) 'CLRWDT) ; Clear Watchdog Timer
;					 (else #f))))
			 ((1) (if to-file?
					  'CLRF ; Clear f
					  'CLRW ; Clear W
					  ))
			 ((2) 'SUBWF) ; Subtract W from f
			 ((3) 'DECF)  ; Decrement f
			 ((4) 'IORWF) ; Inclusive OR W with f
			 ((5) 'ANDWF) ; AND W with f
			 ((6) 'XORWF) ; Exclusive OR W with f
			 ((7) 'ADDWF) ; Add W and f
			 ((8) 'MOVF)  ; Move f
			 ((9) 'COMF)  ; Complement f
			 ((10) 'INCF) ; Increment f
			 ((11) 'DECFSZ) ; Decrement f, Skip if 0
			 ((12) 'RRF) ; Rotate Right f through Carry
			 ((13) 'RLF) ; Rotate Left f through Carry
			 ((14) 'SWAPF) ; Swap nibbles in f
			 ((15) 'INCFSZ) ; Increment f, Skip if 0
			 )

		 (cond [(memq mnemonic '(NOP RETURN RETFIE SLEEP SLRWDT CLRW))
				(format "~a" mnemonic)]
			   [(< opcode 2)
				(format "~a\t~a" mnemonic (file-register file))]
			   [else
				(format "~a\t~a, ~a" mnemonic (file-register file) (if to-file? 'f 'W))])
		 )))
	((1)
	 (let ([opcode (ash inst -10)]
		   [bit (logand (ash inst -7) #x7)]
		   [file (logand inst #x7f)])
	   (let1 mnemonic
		   (case (logand opcode #x3)
			 ((0) 'BCF) ; Bit Clear f
			 ((1) 'BSF) ; Bit Set f
			 ((2) 'BTFSC) ; Bit Test f, Skip if Clear
			 ((3) 'BTFSS) ; Bit Test f, Skip if Set
			 )

		 (let1 bit-name (case file
						  ((3) ; STATUS
						   (ref '(C DC Z ^PD ^TO RP0 RP1 IRP) bit))
						  ((5) ; PORTA
						   (format "RA~d" bit))
						  ((6) ; PORTB
						   (format "RB~d" bit))
						  ((7) ; PORTC
						   (format "RC~d" bit))
						  (else bit))
		   (format "~a \t~a, ~a"
				   mnemonic
				   (file-register file)
				   bit-name)
		   ))))
	((2) ; 100=call 101=goto
	 (let ([opcode (ash inst -11)]
		   [addr (logand inst #x7ff)])
	   (let1 mnemonic
		   (if (= 4 opcode)
			   'CALL ; Call Subroutine
			   'GOTO ; Go to address
			   )
		 (format "~a\t0x~x" mnemonic addr)
		 )))
	((3)
	 (let ([opcode (ash inst -8)]
		   [addr (logand inst #xff)])
	   (let1 mnemonic
		   (case (logand opcode #xf)
			 ((0 1 2 3) 'MOVLW) ; Move literal to W
			 ((4 5 6 7) 'RETLW) ; Return with literal in W
			 ((8) 'IORLW) ; Inclusive OR literal with W
			 ((9) 'ANDLW) ; AND literal with W
			 ((10) 'XORLW) ; Exclusive OR literal with W
			 ((11) '***) ;
			 ((12 13) 'SUBLW) ; Subtract W from literal
			 ((14 15) 'ADDLW) ; Add literal and W
			 )
		 (format "~a\t0x~x" mnemonic addr)
		 )))
	(else #f))
  )

(define (pic-disasm obj-code)
  (let next ([code obj-code]
			 [addr 0]
			 [result '()])
	(if (null? code) (reverse! result)
		(let1 inst (car code)
		  (next (cdr code)
				(+ addr 1)
				(cons (format "~4,'0x: ~4,'0x  ~a"
							  addr inst (pic-disasm-inst inst))
					  result))))))
