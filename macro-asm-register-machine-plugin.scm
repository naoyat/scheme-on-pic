(require "./macro-asm-plugin")

(define (tagged? obj tag)
  (and (pair? obj) (eq? tag (car obj))))

(define (tagged-body obj) (second obj))

(define register? (cut tagged? <> 'reg))
(define const? (cut tagged? <> 'const))
(define operation? (cut tagged? <> 'op))
(define label? (cut tagged? <> 'label))

(define (register-name obj)
  (cond [(register? obj) (tagged-body obj)]
		[(symbol? obj) obj]
		[(number? obj) obj]
		[else #f]))

(define (const-value obj)
  (cond [(const? obj) (tagged-body obj)]
		;;[(symbol? obj) obj]
		[(number? obj) obj]
		[else #f]))

(define (operation-name obj)
  (cond [(operation? obj) (tagged-body obj)]
		[(symbol? obj) obj]
		;;[(number? obj) obj]
		[else #f]))

(define (label-name obj)
  (cond [(label? obj) (tagged-body obj)]
		[(symbol? obj) obj]
		[(number? obj) obj]
		[else #f]))

;;
;; integer
;;
(define (make-scm-6bit-integer i)
  (logior #x01 (ash i 2)))

(define (make-scm-16bit-integer i)
  (let1 ui (logand #xffff i) ; これは i<0 でも [0,65535] 内で返ってくる
	(let ([upper (ash ui -8)]
		  [lower (logand ui #xff)])
	  `((-- ,(format "0x~x" ui))
		(MOVLW ,upper)
		(save-w)
		(MOVLW ,lower)
;		(save-w)
		(CALL int16)
		))))
;;
;;
;(define *undefined* (if #f 0))
;
(define (plug-in:register-machine)
  ;; command implementations
  (define (register-machine/test op args)
	(case op
	  [(=)
	   (let ([a1 (first args)]
			 [a2 (second args)])
		 (if (and (symbol? a1) (number? a2) (= a2 0))
			 `((-- (= ,a1 0))
			   (asm (CLRF  flag)
					(MOVF  ,a1 F) ; a1 -> W
					(BTFSC STATUS Z) ; skip if Z=0
					(INCF  flag F)) ; flag++ if Z=1, ie. a1=a2
			   (--))
			 `((*rem (= ,a1 ,a2))
			   (asm (CLRF flag)
					,(if (symbol? a1)
						 `(MOVF  ,a1 W) ; a1 -> W
						 `(MOVLW ,a1)) ; a1 -> W
					,(if (symbol? a2)
						 `(SUBWF ,a2 W) ; a2 - w -> W
						 `(SUBLW ,a2))  ; a2 - w -> w
					(BTFSC STATUS Z) ; skip if Z=0
					(INCF flag F)) ; flag++ if Z=1, ie. a1=a2
			   (--))
			 ))]
	  [(<)
	   (let ([a1 (first args)]
			 [a2 (second args)])
		 `((-- (< ,a1 ,a2))
		   ,(if (symbol? a1)
				(if (symbol? a2)
					;; (< reg reg)
					`(asm (CLRF  flag)
						  (MOVF  ,a2 W)
						  (SUBWF ,a1 W) ; a1 - a2 < 0ならBO=1; C=0
						  (BTFSS STATUS C) ; skip if C=1 (BO=0)
						  (INCF  flag F)) ; flag++ if C=0 (BO=1)
					;; reg const
					`(asm (CLRF  flag)
						  (MOVLW ,a2)
						  (SUBWF ,a1 W) ; a1 - a2 < 0ならBO=1; C=0
						  (BTFSS STATUS C) ; skip if C=1 (BO=0)
						  (INCF  flag F)) ; flag++ if C=0 (BO=1)
					)
				(if (symbol? a2)
					;; const reg
					`(asm (CLRF  flag)
						  (MOVF  ,a2 W)
						  (SUBLW ,a1) ; a1 - a2 < 0ならBO=1; C=0
						  (BTFSS STATUS C) ; skip if C=1 (BO=0)
						  (INCF  flag F)) ; flag++ if C=0 (BO=1)
					;; const const
					`(assign flag ,(if (< a1 a2) 1 0))
					))
		   (--))
		 )]
	  [else
	   (format #t "TEST ~a(unknown) ~a\n" op args)
	   '()]
	  ))
  (define (register-machine/apply op args)
	(case op
	  [(+)
	   (let ([a1 (first args)]
			 [a2 (second args)])
		 `((-- (+ ,a1 ,a2))
		   (asm ,(if (symbol? a2)
					 `(MOVF  ,a2 W)
					 `(MOVLW ,a2))
				,(if (symbol? a1)
					 `(ADDWF ,a1 W)
					 `(ADDLW ,a1))
				))
;		   (--)
		 )]
	  [(-)
	   (let ([a1 (first args)]
			 [a2 (second args)])
		 `((-- (- ,a1 ,a2))
		   (asm ,(if (symbol? a2)
					 `(MOVF  ,a2 W)
					 `(MOVLW ,a2))
				,(if (symbol? a1)
					 `(SUBWF ,a1 W)
					 `(SUBLW ,a1))
				))
		 )]
	  [else
	   (format #t "APPLY ~a(unknown) ~a\n" op args)
	   '()]
	  ))
  ;;(define (register-machine/assign reg args)
  ;;  (if 
  ;;  )
  (define (assign-reg reg reg2) ; (assign reg1 reg2)
;	(print "ASSIGN (reg " reg ") (reg " reg2 ")")
	`((asm (MOVF   ,reg2 W)
		   (MOVWF  ,reg))))
  (define (assign-const reg const) ; (assign reg const)
;	(print "ASSIGN (reg " reg ") (const " const ")")
	`((asm (MOVLW ,const)
		   (MOVWF ,reg))))
  (define (assign-op reg op args)
;	(print "ASSIGN (reg " reg ") (op " op ") " args)
	(append (register-machine/apply op args)
			`((asm MOVWF ,reg))))
  (define (assign-label reg label)
	`((-- (assign ,reg ,label))
	  ;; make-scm-16bit-integer
	  (MOVLW  HIGH ,label)
	  (save-w)
	  (MOVLW  LOW ,label)
;	  (save-w)
	  (CALL   int16)
	  (MOVWF  ,reg)
	  ))
  ;;(define (register-machine/perform op args)
  ;;  ([code'apply] op args)
  ;;  )
  ;;(define (register-machine/test op args)
  ;;  (append
  ;;   ([code'apply] op args)
  ;;   `((MOVLW 1) ;dummy
  ;;	 (MOVWF ,flag)
  ;;	 )))
  (define (register-machine/branch label)
	`((asm (MOVF   flag F)
		   (BTFSS  STATUS Z) ; skip if Z=1
		   (GOTO   ,label)) ; goto label if Z=0, ie. flag!=0
	  ))

  (define (register-machine/branch-unless label)
	`((asm (MOVF   flag F)
		   (BTFSC  STATUS Z) ; skip if Z=0
		   (GOTO   ,label)) ; goto label if Z=1, ie. flag==0
	  ))

  (make-plugin
   ;; registers to allocate
   '(flag
	 top-of-stack
	 int16-addr
	 pair-addr
	 fsr-keeper
	 w-keeper
;	 argl ; 引数リスト
	 )
   ;; init code
;   '((assign top-of-stack #x30))
;   '((assign top-of-stack #x38)
   '((assign top-of-stack #x37) ; 38hから始めたい
	 (assign pair-addr #x1F)    ; 20hから始めたい
	 (assign int16-addr #x4F))  ; 50hから
   ;; subroutines
   `(;; save-w
	 save-w
	 (asm    (MOVWF  w-keeper)                ; Wの値を保存しておく：temp = w
			 (MOVF   FSR W)
			 (MOVWF  fsr-keeper))
;;	 save-check
;;   (top-of-stack + 1) が [0x20 .. 0x6F] に入らなければ skip
	 (asm    (MOVLW  #x6F)                ; w = top-of-stack - 0x6F
			 (SUBWF  top-of-stack W)      ; top-of-stack >= 0x6F ならBORROWなし (C=1)
			 (BTFSC  STATUS C)            ; そうだったらsave-skipへ
			 (GOTO   save-skip)

			 (MOVLW  #x1F)                ; w = top-of-stack - 0x1F
			 (SUBWF  top-of-stack W)      ; top-of-stack >= 0x1F ならBORROWなし (C=1)
			 (BTFSS  STATUS C)            ; そうでなければsave-skipへ
			 (GOTO   save-skip))
;;	 save-do
	 (asm    (INCF   top-of-stack F)      ; FSR = ++top-of-stack
			 (MOVF   top-of-stack W)
			 (MOVWF  FSR)
			 (MOVF   w-keeper W)          ; [FSR] = w = temp (= initial w)
			 (MOVWF  INDF))
	 save-skip
	 (asm    ;(INCF   top-of-stack F)      ; top-of-stack ++ anyway
			 (MOVF   fsr-keeper W)        ; FSRの値を save-w 呼出し前の値に戻す
			 (MOVWF  FSR)
			 (MOVF   w-keeper W)          ; w = temp (= initial w)
			 (RETURN))
;	 save-skip2
;	 (display 15)
;	 (display 15)
;	 (GOTO   save-skip)

	 ;; restore-w
	 restore-w
	 (asm   ;(DECF   top-of-stack F)      ; top-of-stack --
			 (MOVF   FSR W)               ; fsr-keeper = (w =) FSR
			 (MOVWF  fsr-keeper))
;;	 restore-check
;;   if (top-of-stack < 0x1F || 0x6F <= top-of-stack) goto :save-skip
;;   top-of-stack が [0x20 .. 0x6F] に入らなければ skip
	 (asm    (MOVLW  #x70)                ; w = top-of-stack - 0x70
			 (SUBWF  top-of-stack W)      ; top-of-stack >= 0x70 ならBORROWなし (C=1)
			 (BTFSC  STATUS C)            ; そうだったらrestoreee-skipへ
			 (GOTO   restore-skip)
			 (MOVLW  #x20)                ; w = top-of-stack - 0x20
			 (SUBWF  top-of-stack W)      ; top-of-stack >= 0x1F ならBORROWなし (C=1)
			 (BTFSS  STATUS C)            ; そうでなければrestore-skipへ
			 (GOTO   restore-skip))
;;	 restore-do
	 (asm    (MOVF   top-of-stack W)      ; FSR = w = top-of-stack
			 (MOVWF  FSR)
			 (MOVF   INDF W)              ; temp = w = [FSR]
			 (MOVWF  w-keeper)
			 (MOVF   fsr-keeper W)        ; FSRの値を save-w 呼出し前の値に戻す
			 (MOVWF  FSR)
			 (MOVF   w-keeper W)
			 (DECF   top-of-stack F)      ; top-of-stack --
			 )
	 restore-skip
	 (asm    (RETURN))

	 ;;
	 ;; int16
	 ;;
	 int16
	 ;; ScmInt16 ([pop] + [pop]*256) を１つアロケートしてWに返す
	 ;; (0x100 + ofs, 0x80 + ofs), ofs = 0 - 79
	 ;; 79まで取れるけど63までしか使わない
	 ;; ... はやくGC作りたし
	 ;; (-- skip if ++int-addr >= 0x70) ; 50 - 6f
	 (INCF   int16-addr F)

	 ;; w=lower [upper]
	 (save-w)
	 ;; [lower upper]

	 ;;(when (<= #x70 int-addr) (GOTO ,fail-label))
	 (MOVF   int16-addr W)
	 (SUBLW  #x70)
	 (BTFSS  STATUS C) ; pair-addr >= 0x60のときborrow=1 (C=0)
	 (GOTO   int16-fail)

	 (MOVF   int16-addr W)
	 (MOVWF  FSR)

	 (-- set lower)
	 (restore-w)
	 (change-irp 0 2)
	 (MOVWF  INDF)
	 (change-irp 2 0)
	 
	 (-- set upper)
	 (BSF    FSR  7)
	 (restore-w)
;	 (change-irp 0 1)
	 (MOVWF  INDF)
;	 (change-irp 1 0)
	 
	 ;; w = #b010 | (int16-addr - 0x50) << 3 
	 (MOVF   int16-addr W)	; temp = w = pair-addr
	 (MOVWF  temp)

	 (MOVLW  #x50)			; w = 0x50
	 (SUBWF  temp F)	    ; temp -= 0x50
	 (BCF    STATUS C)		; STATUS[C] = 0
	 (RLF    temp F)	    ; w = temp << 3
	 (RLF    temp F)        ;
	 (RLF    temp W)
	 (ADDLW  #b010)
	 (RETURN)
	 ;; w = #<undef> if failed
   int16-fail
     (restore-w)
     (restore-w)
	 (RETLW #b10001110) ;;,scm-undefined


	 int16-lower
;	 (LED/display-w)
	 ;; checking if int16
	 (MOVWF  temp)
	 (BTFSC  temp 0)
	 (GOTO   int16-lower-skip)
	 (BTFSS  temp 1)
	 (GOTO   int16-lower-skip)
	 (BTFSC  temp 2)
	 (GOTO   int16-lower-skip)

	 ;; FSR = #x150 + (w - #b010) >> 3)
	 (MOVWF  FSR)		; FSR = w
	 (BCF    STATUS C)	; STATUS[C] = 0
	 (BCF    FSR  1)    ; FSR -= #b010
	 (RRF    FSR  F)	; FSR >>= 3
	 (RRF    FSR  F)
	 (RRF    FSR  F)
	 (MOVLW  #x50)		; w = 0x50
	 (ADDWF  FSR  F)	; FSR += w

;	 (LED/display-reg FSR)
	 ;; w = *FSR
	 (change-irp 0 2)
	 (MOVF   INDF W)	; w = [INDF]
	 (change-irp 2 0)
	 (RETURN)

	 int16-lower-skip
	 ;; *** ERROR: pair required, but got ????
	 (RETURN)


	 int16-upper
;	 (LED/display-w)
	 ;; checking if pair
	 (MOVWF  temp)
	 (BTFSC  temp 0)
	 (GOTO   int16-upper-skip)
	 (BTFSS  temp 1)
	 (GOTO   int16-upper-skip)
	 (BTFSC  temp 2)
	 (GOTO   int16-upper-skip)

	 ;; FSR = #x0D0 + (w - #b010) >> 3)
	 (MOVWF  FSR)		; FSR = w
	 (BCF    STATUS C)	; STATUS[C] = 0
	 (BCF    FSR  1)    ; FSR -= #b010
	 (RRF    FSR  F)	; FSR >>= 3
	 (RRF    FSR  F)
	 (RRF    FSR  F)
	 (MOVLW  #xD0)		; w = 0xD0
	 (ADDWF  FSR  F)	; FSR += w
;	 (LED/display-reg FSR)
	 ;; w = *FSR
;	 (change-irp 0 1)
	 (MOVF   INDF W)	; w = [INDF]
;	 (change-irp 1 0)
	 (RETURN)

	 int16-upper-skip
	 ;; *** ERROR: pair required, but got ????
	 (RETURN)
	 )

   ;; plugins

   ;; (assign <レジスタ名> (reg <レジスタ名>))
   ;; または (assign <レジスタ名> <レジスタ名>)
   ;; (assign <レジスタ名> (const <定数値>))
   ;; または (assign <レジスタ名> <定数値>)
   ;; (assign <レジスタ名> (op <演算子名>) 入力1 .. 入力1n)
   ;; または (assign <レジスタ名> <演算子名> 入力1 .. 入力1n)
   ;; または (assign <レジスタ名> (<演算子名> 入力1 .. 入力1n))
   ;; // (assign <レジスタ名> (label <ラベル名>)) -- 未実装
   (list 'assign
		 (lambda (reg . args)
		   (if (= 1 (length args))
			   (let1 arg (car args)
				 (cond [(register? arg) ; (assign REG (reg REG2))
						(assign-reg reg (register-name arg))]
					   [(const? arg) ; (assign REG (const CONST))
						(assign-const reg (const-value arg))]
					   [(operation? arg) ; (assign REG (op OPERATION) arg1 .. argn)
						(assign-op reg (operation-name arg) (map cadr (cdr args)))]
					   [(label? arg)
						(assign-label reg (label-name arg))]
					   [(pair? arg) ; (assign REG (OPERATION arg1 ... argn))
						(assign-op reg (car arg) (cdr arg))]
					   [(symbol? arg) ; (assign REG 'reg-name)
						(assign-reg reg arg)]
					   [(number? arg) ; (assign REG number)
						(assign-const reg arg)]
					   [else '()]
					   ))
			   (let ([op (car args)] [args (cdr args)])
				 (if (and (pair? op) (operation? op))
					 (assign-op reg (operation-name op) (map cadr args))
					 (assign-op reg op args)))
			   )))
   ;; (perform (op <演算子名>) arg1 .. argn)
   ;; または (perform <演算子名> arg1 .. argn)
   ;; または (perform (<演算子名> arg1 .. argn))
   (list 'perform
		 (lambda (op . args)
		   (if (pair? op)
			   (if (operation? op)
				   (register-machine/apply (operation-name op) args) ; (perform (op <演算子名>) ...)
				   (register-machine/apply (car op) (cdr op))) ; (perform (<演算子名> ...))
			   (register-machine/apply op args)))) ;  ; (perform <演算子名> ...)

   ;; (test (op <演算子名>) arg1 .. argn)
   ;; または (test <演算子名> arg1 .. argn)
   ;; または (test (<演算子名> arg1 .. argn))
   (list 'test
		 (lambda (op . args)
		   (if (pair? op)
			   (if (operation? op)
				   (register-machine/test (operation-name op) (map cadr args)) ; (test (op <演算子名>) ...)
				   (register-machine/test (car op) (cdr op))) ; (test (<演算子名> ...))
			   (register-machine/test op args)))) ;  ; (test <演算子名> ...)

   ;; (branch (label <ラベル名>))
   ;; または (branch <ラベル名>)
   (list 'branch
		 (lambda (arg) (register-machine/branch (label-name arg))))

   ;; (goto (label <ラベル名>))
   ;; または (goto <ラベル名>)
   (list 'goto
;		 (lambda (arg) `((GOTO ,(label-name arg)))))
		 (lambda (arg)
		   (cond [(pair? arg)
				  (case (car arg)
					[(reg) ; (register? arg)
					 (let1 reg (register-name arg)
					   `((MOVF   ,reg W)
						 (CALL   int16-upper)
						 (MOVWF  PCLATH)
						 (MOVF   ,reg W)
						 (CALL   int16-lower)
						 (MOVWF  PCL)
						 ))]
					[(label) ; (label? arg)
					 `((GOTO ,(cadr arg)))]
					[else '()]
					)]
				 [(symbol? arg)
				  `((GOTO ,arg))]
				 [(integer? arg)
				  `((GOTO ,arg))]
				 [else
				  '()] )))

   (list 'save-w
		 (lambda ()
		   '((CALL save-w))))
   (list 'save
		 (lambda (reg)
		   `((MOVF  ,reg W)
			 (CALL  save-w))))
   (list 'restore-w
		 (lambda ()
		   '((CALL restore-w))))
   (list 'restore
		 (lambda (reg)
		   `((CALL  restore-w)
			 (MOVWF ,reg))))

;   (list 'apply
;		 (lambda (op args) (register-machine/apply op args)))

   ;; int16
   (list 'int16-lower
		 ;; Wレジスタに入れたScmInt16のlowerをWに返す
		 (lambda () '((CALL int16-lower)) ))
   
   (list 'int16-upper
		 ;; Wレジスタに入れたScmPairのupperをWに返す
		 (lambda () '((CALL int16-upper)) ))

   ))
