(require "./macro-asm-plugin")

;;
;; [plug-in] basic
;;
(define (bool= a b)
  (or (and a b) (and (not a) (not b))))
(define (bool/= a b)
  (or (and a (not b)) (and (not a) b)))


(define (plug-in:basic)
  (define (remark-code . rem)
	`((*rem ,@rem)))
  (define (reg-alloc-code name)
	`((,name EQU ,(reg-alloc name))))
  (define (change-bank-code from to)
	(if (= from to)
		'() ; empty code
		(let1 cs '()
		  (when (bool/= (logbit? 1 from) (logbit? 1 to))
			(push! cs `(,(if (logbit? 1 to) 'BSF 'BCF) STATUS RP1)))
		  (when (bool/= (logbit? 0 from) (logbit? 0 to))
			(push! cs `(,(if (logbit? 0 to) 'BSF 'BCF) STATUS RP0)))
		  (if (null? cs)
				`((*rem page ,to))
				(append `((*rem change page from ,from to ,to))
						cs)))))

  (define (change-irp-code from to) ; from/to の指定はbank[0,1,2,3]
	(if (= (ash from -1) (ash to -1))
		'() ; empty code
		(let1 cs '()
		  (when (bool/= (logbit? 1 from) (logbit? 1 to))
			(push! cs `(,(if (logbit? 1 to) 'BSF 'BCF) STATUS IRP)))
		  (if (null? cs)
			  `((*rem irp ,to))
			  (append `((*rem change page from ,from to ,to))
					  cs)))))

										; レジスタに即値を代入
  (define (setf-code reg value)
	(define (bankless-setf-code reg value)
	  (if (= value 0)
		  `((CLRF ,reg))
		  `((MOVLW ,value)
			(MOVWF ,reg)) ))
	(let1 reg-addr (reg-resolve reg)
	  (let1 bank (ash reg-addr -7)
		(if (= bank 0) ;; (<= reg 0x7f)
			(bankless-setf-code reg value)
			(append (change-bank-code 0 bank) ;バンク切替え
					(bankless-setf-code reg value)
					(change-bank-code bank 0) ;バンク戻す
					)))))
  
  ;; GOTO
;  (define (goto-code to) `((GOTO ,to)))
  
  ;; if (--<reg> != 0) goto <to>
  (define (dec-jnz-code reg to)
	`((DECFSZ ,reg F)
	  (GOTO ,to)))
  
;  (define (call-code to) `((CALL ,to)))
;  (define (return-code) '((RETURN)))
  
  (make-plugin
   '(temp) ;; 共用
   '()
   '()
   ;; plugins
   (list '--
		 (lambda rem (apply remark-code rem)))
   (list '*rem
		 (lambda rem (apply remark-code rem)))
   (list 'remark
		 (lambda rem (apply remark-code rem)))
   (list 'alloc
		 (lambda (reg) (reg-alloc-code reg)))
   (list 'set
		 (lambda (reg value) (setf-code reg value)))
   
   (list 'change-bank
		 (lambda (from to) (change-bank-code from to)))
   (list 'change-irp
		 (lambda (from to) (change-irp-code from to)))
   
   (list 'label
		 (lambda (label) (list label)))
   (list 'dec-jnz
		 (lambda (reg to) (dec-jnz-code reg to)))
   
;   (list 'goto
;		 (lambda (to) (goto-code to)))
;   (list 'GOTO
;   (list 'call
;		 (lambda (to) (call-code to)))
;   (list 'ret (lambda () '((RETURN))))

   ;;
   ;; special instruction mnemonics
   ;;   cf. http://blog3.fc2.com/g/goda/file/picoldmnem.htm
   (list 'ADDCF  ; Add Carry to File
		 (lambda (f d) `((BTFSC STATUS C)
						 (INCF  ,f ,d))))
   (list 'ADDDCF ; Add Digit Carry to File
		 (lambda (f d) `((BTFSC STATUS DC)
						 (INCF  ,f ,d))))
   (list 'B ; Branch
		 (lambda (k) `((GOTO ,k))))
   (list 'BC ; Branch on Carry
		 (lambda (k) `((BTFSC STATUS C)
					   (GOTO ,k))))
   (list 'BDC ; Branch on Digit Carry
		 (lambda (k) `((BTFSC STATUS DC)
					   (GOTO ,k))))
   (list 'BNC ; Branch on No Carry
		 (lambda (k) `((BTFSS STATUS C)
					   (GOTO ,k))))
   (list 'BNDC ; Branch on No Digit Carry
		 (lambda (k) `((BTFSS STATUS DC)
					   (GOTO ,k))))
   (list 'BNZ ; Branch on No Zero
		 (lambda (k) `((BTFSS STATUS Z)
					   (GOTO ,k))))
   (list 'BZ ; Branch on Zero
		 (lambda (k) `((BTFSC STATUS Z)
					   (GOTO ,k))))
   (list 'CLRC ; Clear Carry
		 (lambda () '(BCF STATUS C)))
   (list 'CLRDC ; Clear Digit Carry
		 (lambda () '(BCF STATUS DC)))
   (list 'CLRZ ; Clear Zero
		 (lambda () '(BCF STATUS Z)))
   (list 'LCALL ; Long Call
		 (lambda (k)
		   (let ([bit14 (logbit? 14 k)]
				 [bit13 (logbit? 13 k)]
				 [bit12-0 (logand #x1fff k)])
			 `((,(if bit14 'BSF 'BCF) PCLATH 4)
			   (,(if bit13 'BSF 'BCF) PCLATH 3)
			   (CALL ,bit12-0)))))
   (list 'LGOTO ; Long GOTO
		 (lambda (k)
		   (let ([bit14 (logbit? 14 k)]
				 [bit13 (logbit? 13 k)]
				 [bit12-0 (logand #x1fff k)])
			 `((,(if bit14 'BSF 'BCF) PCLATH 4)
			   (,(if bit13 'BSF 'BCF) PCLATH 3)
			   (GOTO ,bit12-0)))))
   (list 'MOVFW ; Move File to W
		 (lambda (f) `((MOVF ,f W))))
   (list 'NEGF ; Negative File
		 (lambda (f d) `((COMF ,f F) ; changes F
						 (INCF ,f ,d))))
   (list 'SETC ; Set Carry
		 (lambda () '((BSF STATUS C))))
   (list 'SETDC ; Set Digit Carry
		 (lambda () '((BSF STATUS DC))))
   (list 'SETZ ; Set Zero
		 (lambda () '((BSF STATUS Z))))
   (list 'SKPC ; Skip on Carry
		 (lambda () '((BTFSS STATUS C))))
   (list 'SKPDC ; Skip on Digit Carry
		 (lambda () '((BTFSS STATUS DC))))
   (list 'SKPNC ; Skip on No Carry
		 (lambda () '((BTFSC STATUS C))))
   (list 'SKPNDC ; Skip on No Digit Carry
		 (lambda () '((BTFSC STATUS DC))))
   (list 'SKPNZ ; Skip on No Zero
		 (lambda () '((BTFSC STATUS Z))))
   (list 'SKPZ ; Skip on Zero
		 (lambda () '((BTFSS STATUS Z))))
   (list 'SUBCF ; Subtract Carry from File
		 (lambda (f d) `((BTFSC STATUS C)
						 (DECF ,f ,d))))
   (list 'SUBDCF ; Subtract Digit Carry from File
		 (lambda (f d) `((BTFSC STATUS DC)
						 (DECF ,f ,d))))
   (list 'TSTF ; Test File
		 (lambda (f) `((MOVF ,f F))))

   ;;
   ;; raw asm code
   ;;
   (list 'asm
		 (lambda asm (if (pair? (car asm))
						 asm
						 (list asm))))
   ))
