;;;
;;; pic-asm
;;;
;;;   Copyright (c) 2008 naoya_t (naoya.t@aqua.plala.or.jp)
;;;

(use srfi-1)
(use srfi-13)
(use util.match)

(define-macro (assoc-cdr obj lst default)
  `(let1 it (assoc ,obj ,lst)
	 (if it (cdr it) ,default)))

;;
;; pass2
;;
(define-macro (build-14bit-opcode-1 op d f) ; ?????? d fffffff
  `(logior (ash (logand #b111111 ,op) 8)
		   (ash (logand 1 ,d) 7)
		   (logand #b1111111 ,f) ))

(define-macro (build-14bit-opcode-2 op b f) ; ???? bbb fffffff
  `(logior (ash (logand #b1111 ,op) 10)
		   (ash (logand #b111 ,b) 7)
		   (logand #b1111111 ,f) ))

(define-macro (build-14bit-opcode-3 op k) ; ?????? kkkkkkkk
  `(logior (ash (logand #b111111 ,op) 8)
		   (logand #b11111111 ,k) ))

(define (pass2 lst)
  (let loop ([rest lst] [ops (make-list (caar lst) 0)])
	(if (null? rest)
		(reverse! ops)
		(let ([addr (caar rest)] [op (cdar rest)])
		  (loop (cdr rest)
				(cons (match op
						[('ADDWF  f d)  (build-14bit-opcode-1 #b000111 d f)]
						[('ANDWF  f d)  (build-14bit-opcode-1 #b000101 d f)]
						[('CLRF   f)    (build-14bit-opcode-1 #b000001 1 f)]
						[('CLRW)        #b00000100000000]
						;;              (build-14bit-opcode-1 #b000001 0 0))
						[('COMF   f d)  (build-14bit-opcode-1 #b001001 d f)]
						[('DECF   f d)  (build-14bit-opcode-1 #b000011 d f)]
						[('DECFSZ f d)  (build-14bit-opcode-1 #b001011 d f)]
						[('INCF   f d)  (build-14bit-opcode-1 #b001010 d f)]
						[('INCFSZ f d)  (build-14bit-opcode-1 #b001111 d f)]
						[('IORWF  f d)  (build-14bit-opcode-1 #b000100 d f)]
						[('MOVF   f d)  (build-14bit-opcode-1 #b001000 d f)]
						[('MOVWF  f)    (build-14bit-opcode-1 #b000000 1 f)]
						[('NOP)         0]
						[('RLF    f d)  (build-14bit-opcode-1 #b001101 d f)]
						[('RRF    f d)  (build-14bit-opcode-1 #b001100 d f)]
						[('SUBWF  f d)  (build-14bit-opcode-1 #b000010 d f)]
						[('SWAPF  f d)  (build-14bit-opcode-1 #b001110 d f)]
						[('XORWF  f d)  (build-14bit-opcode-1 #b000110 d f)]
						
						[('BCF    f b)  (build-14bit-opcode-2 #b0100 b f)]
						[('BSF    f b)  (build-14bit-opcode-2 #b0101 b f)]
						[('BTFSC  f b)  (build-14bit-opcode-2 #b0110 b f)]
						[('BTFSS  f b)  (build-14bit-opcode-2 #b0111 b f)]
						
						[('ADDLW  k)    (build-14bit-opcode-3 #b111110 k)]
						[('ANDLW  k)    (build-14bit-opcode-3 #b111001 k)]
						[('CALL   k)    (logior (ash #b100 11) k)]
;;												(if (symbol? k) (resolve-label k) k))]
						[('CLRWDT)      #b00000001100100]
						[('GOTO   k)    (logior (ash #b101 11) k)]
;;												(if (symbol? k) (resolve-label k) k))]
						[('IORLW  k)    (build-14bit-opcode-3 #b111000 k)]
						[('MOVLW  k)    (build-14bit-opcode-3 #b110000 k)]
						[('RETFIE)      #b00000000001001]
						[('RETLW  k)    (build-14bit-opcode-3 #b110100 k)]
						[('RETURN)      #b00000000001000]
						[('SLEEP)       #b00000001100011]
						[('SUBLW  k)    (build-14bit-opcode-3 #b111100 k)]
						[('XORLW  k)    (build-14bit-opcode-3 #b111010 k)]

						;; デバッグ用
						[('DEBUG:file f)     (build-14bit-opcode-3 #b111011 (logior #x80 f))]
						[('DEBUG:w)          #b11101100000000]
						[('DEBUG:pc)         #b11101100000001]
						[('DEBUG:nextinst)   #b11101100000010]
						[('DEBUG:snapshot)   #b11101100001111]
						[('DEBUG:skip-if-emulator)
						                     #b11101101111111]
						[('DEBUG:scm:w)      #b11101100010000]
						[('DEBUG:scm:w-pp)   #b11101100010001]
						[('DEBUG:scm:stack)  #b11101100010010]
						[('DEBUG:scm:pairs)  #b11101100010011]
						[('DEBUG:scm:int16s) #b11101100010100]
;						[('DEBUG:scm:w)      #b11101100010000]

						[else (print "?? " op) 0]) ops));loop
		  ))))

;;
;; pass1
;;
(define *file-registers* ;; PIC16F690
  (append (map cons
			   '(INDF TMR0 PCL STATUS FSR PORTA PORTB PORTC - - PCLATH INTCON
					  PIR1 PIR2 TMR1L TMR1H T1CON TMR2 T2CON SSPBUF SSPCON
					  CCPR1L CCPR1H CCP1CON
					  RCSTA TXREG RCREG - PWM1CON ECCPAS ADRESH ADCON0)
			   (iota 32))
		  (map cons
			   '(INDF OPTION_REG PCL STATUS FSR TRISA TRISB TRISC - - PCLATH INTCON
					  PIE1 PIE2 PCON OSCCON OSCTUNE - PR2 SSPADD SSPSTAT WPUA IOCA
					  WDTCON TXSTA SPBRG SPBRGH BAUDCTL - - ADRESL ADCON1)
;					  TXSTA SPBRG - - CMCON CVRCON ADRESL ADCON1)
			   (iota 32 #x80))
		  (map cons
			   '(INDF TMR0 PCL STATUS FSR PORTA PORTB PORTC - - PCLATH INTCON
					  EEDAT EEADR EEDATH EEADRH - - - - - WPUB IOCB - VRCON
					  CM1CON0 CM2CON0 CM2CON1 - - ANSEL ANSELH)
			   (iota 32 #x100))
		  (map cons
			   '(INDF OPTION_REG PCL STATUS FSR TRISA TRISB TRISC - - PCLATH INTCON
					  EECON1 EECON2 - -
					  - - - - - - - - - - - - - PSTRCON SRCON -)
			   (iota 32 #x180))
		  ))

(define (pass1 insts)
  (let next ([addr 0]
			 [insts-rest insts]
			 [labels '()]
			 [regs '()]
			 [insts-passed '()])
	(define (resolve-name inst)
	  (let ([addr (car inst)]
			[operator (cadr inst)]
			[operands (cddr inst)])

		(define (resolve-label label)
;		  #?=`(,label ,labels)
		  (if (number? label) label
			  (assoc-cdr label labels
						 (let1 s (symbol->string label)
						   (cond [(eq? #\$ (string-ref s 0))
								  (if (= 1 (string-length s))
									  addr ; $
									  (+ addr (string->number (substring s 1 -1))))]
								 [else (print "unknown label " s) 0])))))

		(case operator
		  [(CALL GOTO)
		   `(,addr ,operator
				   ,(resolve-label (car operands))
				   )]
		  [(ADDWF ANDWF COMF DECF DECFSZ INCF INCFSZ IORWF MOVF RLF RRF SUBWF SWAPF XORWF) ; f d
		   `(,addr ,operator
				   ,(resolve-file-register (first operands))
				   ,(resolve-direction-bit (second operands))
				   )]
		  [(BCF BSF BTFSC BTFSS) ; f b
		   `(,addr ,operator
				   ,(resolve-file-register (first operands))
				   ,(resolve-bit (second operands))
				   )]
		  [(MOVWF CLRF
				  DEBUG:file) ; f
		   `(,addr ,operator
				   ,(resolve-file-register (first operands))
				   )]
		  [(ADDLW ANDLW IORLW MOVLW RETLW SUBLW XORLW) ; LW
		   (case (car operands)
			 [(HIGH High high HI Hi hi)
			  `(,addr ,operator ,(ash (resolve-label (cadr operands)) -8))]
			 [(LOW Low low LO Lo lo)
			  `(,addr ,operator ,(logand #xff (resolve-label (cadr operands))))]
			 [else
			  `(,addr ,operator ,@operands)])]
		  [else
		   `(,addr ,operator
				   ,@operands)])
		))

	(define (resolve-file-register f)
	  (if (number? f) f
		  (assoc-cdr f regs
					 (assoc-cdr f *file-registers* 0))))
  
	(define (resolve-direction-bit d)
	  (if (memq d '(f F 1)) 1 0))

	(define (resolve-bit b)
	  (if (number? b)
		  (if (<= 0 b 7) b 0)
		  (case b
			; STATUS <3>
			[(C) 0]
			[(DC) 1]
			[(Z) 2]
			[(^PD) 3]
			[(^TO) 4]
			[(RP0) 5]
			[(RP1) 6]
			[(IRP) 7]

			; ADCON0 <1f>
			[(GO) 2] ; ADCON0

			; INTCON <8b>
 			[(GIE) 7]

			; EECON1 <18c>
			[(EEPGD) 7]
			[(WRERR) 3]
			[(WREN) 2]
			[(WR) 1]
			[(RD) 0]

			(else 0) )))

	(if (null? insts-rest)
		(map resolve-name (reverse! insts-passed))
		(let ([inst (car insts-rest)]
			  [rest (cdr insts-rest)])
		  (cond [(symbol? inst) ; label
				 ; labels << (label . addr)
				 (next addr rest (cons (cons inst addr) labels) regs insts-passed)]
				[(not (pair? inst)) ; non-symbol atoms are ignored
				 ; ignore
				 (next addr rest labels regs insts-passed)]
				[(eq? '*rem (first inst)) ; remark
				 (next addr rest labels regs insts-passed)]
				[(eq? 'ORG (first inst)) ; (ORG <addr>)
				 ; addr = (second inst)
				 (next (second inst) rest labels regs insts-passed)]
				[(and (> (length inst) 1) (symbol? (second inst)) (eq? 'EQU (second inst))) ; (<reg> EQU <addr>)
										; (first inst) EQU (third inst)
				 (next addr rest labels (cons (cons (first inst) (third inst)) regs) insts-passed)]
				[else ; instruction
				 ; insts << (addr . inst)
				 (next (+ addr 1) rest labels regs (cons (cons addr inst) insts-passed))]
				)
		  ))))

;;
;;
;;
;(require "./intelhex")
(define (pic-asm code) (pass2 (pass1 code)))
;

;
; プリティプリンタ
;
(define (asm-pp asm)
  (cond [(not (pair? asm)) ; label
		 (print asm ":")]
		[(memq (first asm) '(*rem --)) ; remark
;		 (print "  // " (cdr asm))]
		 (display "  // ") (map (lambda (elt) (display elt) (display " ")) (cdr asm))
		 (newline)]
		[(= 1 (length asm)) ; オペランドのない命令
		 (print "\t" (first asm))]
		[(eq? 'EQU (second asm)) ; label EQU value
		 (format #t "~s\tEQU\t~xh\n" (first asm) (third asm))]
		[(memq (second asm) '(HIGH High high LOW Low low))
		 (print "\t" (first asm) "\t" (second asm) " " (third asm))]
		[else
		 (print "\t" (first asm) " \t"
				(string-join (map x->string (cdr asm)) ", "))]
		 ))

(define *asm-instructions*
  '(ADDWF ANDWF CLRF CLRW COMF DECF DECFSZ INCF INCFSZ
		  IORWF MOVF MOVWF NOP RLF RRF SUBWF SWAPF XORWF
		  BCF BSF BTFSC BTFSS
		  ADDLW ANDLW CALL CLRWDT GOTO
		  IORLW MOVLW RETFIE RETLW RETURN SLEEP SUBLW XORLW
		  ;; for debug
		  DEBUG:w DEBUG:file DEBUG:pc DEBUG:nextinst DEBUG:snapshot
		  DEBUG:scm:w DEBUG:scm:w-pp DEBUG:scm:stack DEBUG:scm:pairs DEBUG:scm:int16s
		  DEBUG:skip-if-emulator
		  ))
