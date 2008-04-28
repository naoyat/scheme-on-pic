(require "./macro-asm-plugin")
(require "./scmlib")

(use util.match)

;; for register machine emulation
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

;;;
;;; plug-in for Scheme
;;;
(define (plug-in:scheme)

  (make-plugin
   ;; registers to allocate
   scmlib-registers
   ;; init code
   scmlib-initcode
   ;; subroutines
   scmlib-subroutines

   ;;
   ;; plugins
   ;;
   (list 'mov
		 (lambda (dest src)
		   (define (w? reg) (memq reg '(w W)))
		   (define (reg<-reg dest-reg src-reg)
			 (cond [(w? src-reg) `((MOVWF ,dest-reg))]
				   [(w? dest-reg) `((MOVF ,src-reg W))]
				   [else `((MOVF ,src-reg W)
						   (MOVWF ,dest-reg))] ))
		   (define (reg<-lit dest-reg lit)
			 `((MOVLW ,lit)
			   ,@(if (w? dest-reg)
					 '()
					 `((MOVWF ,dest-reg)))))
		   (define (reg<-label dest-reg label)
			 `((-- (mov ,dest-reg ,label))
			   ;; make-scm-16bit-integer
			   (MOVLW  HIGH ,label)
			   (push)
			   (MOVLW  LOW ,label)
			   ;; (save-w)
			   (int16)
			   ,@(if (w? dest-reg)
					 '()
					 `((MOVWF ,dest-reg)))))
			 
		   (cond [(register? src) ; (mov REG (reg REG2))
				  (reg<-reg dest (register-name src))]
				 [(const? src)    ; (mov REG (const CONST))
				  (reg<-lit dest (const-value src))]
				 [(label? src)
				  (reg<-label dest (label-name src))]
				 [(symbol? src) ; (mov REG 'reg-name)
				  (reg<-reg dest src)]
;					  (reg<-reg dest src)]
				 [(number? src) ; (mov REG number)
				  (reg<-lit dest src)]
				 [else '()]
				 )))

   ;;
   ;; for register machine emulation
   ;;
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
						 (int16-upper)
						 (MOVWF  PCLATH)
						 (mov    w ,reg)
						 (int16-lower)
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

   (list 'ret (lambda ()
				`(;;(mov    val w) ; for return value
;;				  (DEBUG:file 99)
;;				  (DEBUG:scm:stack)
				  (mov    w continue)
				  (int16-upper)
				  (mov    PCLATH w)
				  (mov    w continue)
				  (int16-lower)
				  (mov    PCL w)
				  )))

   (list 'call (lambda ()
;;				 (let1 after-call (label-gen "after-call")
				 `(;; 引数はすべてスタック上にある
					 ;; proc [argn ... arg1]
;				   (DEBUG:file 98)
;				   (DEBUG:scm:stack)

					 ;; w = proc : (λ entrypoint . env)
				   (cdr)
				   (mov    t1 w) ; t1 = w = (cdr <proc>) ; '(entrypoint . env)
				   (cdr)
				   (mov    t2 w) ; t2 = w = (cddr <proc>) ; proc.env
				   (mov    w t1)
				   (car)
				   (mov    temp w) ; temp = w = (car t1) = entrypoint
					 
					 ;; proc.env (関数をバインドした時点のenv) を環境として使う。
					 ;; 呼び出し時のenvはスタックに保存
				   (push   env)    ; [env {argn .. arg1}]
				   (mov    env t2) ; env = w = t2 = proc.env
					 ;; [env {args}]

;					 (push   continue) ; [continue env {args}]
;					 (mov    continue (label ,after-call)) ; continue = <after-call>
;				   (DEBUG:scm:stack)
					 ;; procエントリポイントへ。
					 ;; continueは保存されていないよ
;					 (GOTO   label)
;				   (DEBUG:file continue-cont)
				   (-- (call w))
				   (mov    w temp) ; goto entrypoint
				   (int16-upper)
				   (mov    PCLATH w)
				   (mov    w temp)
				   (int16-lower)
				   (mov    PCL w)

;				   ,after-call
				   ;; []
				   ;; valに返り値が入ってるので適宜wに移すこと
;					 (mov    w val) ; 返り値を受け取る
				   )))


   ;; int16
   (list 'int16 (lambda () '((CALL int16)) ))
   (list 'int16p (lambda () '((CALL int16p))))
   (list 'int16-lower (lambda () '((CALL int16-lower)) )) ; Wレジスタに入れたScmInt16のlowerをWに返す
   (list 'int16-upper (lambda () '((CALL int16-upper)) )) ; Wレジスタに入れたScmPairのupperをWに返す

   ;;
   ;; VM
   ;;
   (list 'bf ;; branch if false
		 (lambda (label)
		   `((-- bf ":" ,label)
			 (XORLW  ,scm-false)
			 (BTFSC  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-false
			 )))

   (list 'bt ;; branch if true == unless #f
		 (lambda (label)
		   `((-- bt ":" ,label)
			 (XORLW  ,scm-false)
			 (BTFSS  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==0 ; ie. w != scm-false
			 )))
   
   (list 'bnull ;; branch if nil
		 (lambda (label)
		   `((-- bnull ":" ,label)
			 (XORLW  ,scm-nil)
			 (BTFSC  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-nil
			 )))
   
   (list 'bnnull ;; branch if not nil
		 (lambda (label)
		   `((-- bnnull ":" ,label)
			 (XORLW  ,scm-nil)
			 (BTFSS  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-nil
			 )))

   (list 'bundef ;; branch if undefined
		 (lambda (label)
		   `((-- bundef ":" ,label)
			 (XORLW  ,scm-undefined)
			 (BTFSC  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-false
			 )))

   (list 'bdef ;; branch unless undefined
		 (lambda (label)
		   `((-- bdef ":" ,label)
			 (XORLW  ,scm-undefined)
			 (BTFSS  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-false
			 )))

   (list 'nullp
		 (lambda () '((CALL  nullp))))
   (list 'pairp
		 (lambda () '((CALL  pairp))))
   (list 'lrefp
		 (lambda () '((CALL  lrefp))))
		 
;   (list 'vm-cons
;		 (lambda ()
;		   (pop ca_)
;		   (scm-cons ca_ w)
;   (define (scm-cons x y)
;	`((-- (cons ,x ,y))
;	  ,@(gauche->pic y)
;	  (eval)
;	  (push)
;	  ,@(gauche->pic x)
;	  (eval)
;	  (push)
;	  (CALL cons) ; w = 新しいpairオブジェクト
;	  ))

   ;; **
   (list 'lset (lambda (dep ofs)
				 (cond [(and (= dep 0) (<= ofs 3))
						`((CALL ,(string->symbol #`"lset,ofs")))]
					   [(and (<= dep 3) (<= ofs 3))
						`((CALL ,(string->symbol #`"lset,dep,ofs")))]
					   [else
						`((-- (lset ,dep ,ofs))
						  )]
					   )))
   (list 'lref (lambda (dep ofs)
				 (cond [(and (= dep 0) (<= ofs 3))
						`((CALL ,(string->symbol #`"lref,ofs")))]
					   [(and (<= dep 3) (<= ofs 3))
						`((CALL ,(string->symbol #`"lref,dep,ofs")))]
					   [else
						`((-- (lref ,dep ,ofs))
						  )]
					   )))

   (list 'lset0 (lambda () '((CALL lset0))))
   (list 'lset1 (lambda () '((CALL lset1))))
   (list 'lset2 (lambda () '((CALL lset2))))
   (list 'lset3 (lambda () '((CALL lset3))))

   (list 'lref0 (lambda () '((CALL lref0))))
   (list 'lref1 (lambda () '((CALL lref1))))
   (list 'lref2 (lambda () '((CALL lref2))))
   (list 'lref3 (lambda () '((CALL lref3))))
   (list 'lref10 (lambda () '((CALL lref10))))
   (list 'lref11 (lambda () '((CALL lref11))))
   (list 'lref12 (lambda () '((CALL lref12))))
;   (list 'lref13 (lambda () '((CALL lref13))))
   (list 'lref20 (lambda () '((CALL lref20))))
   (list 'lref21 (lambda () '((CALL lref21))))
   (list 'lref30 (lambda () '((CALL lref30))))

   (list 'constn (lambda () `((MOVLW 'scm-nil))))
   (list 'constf (lambda () `((MOVLW 'scm-false))))
   (list 'constu (lambda () `((MOVLW 'scm-undefined))))

   (list 'consti (lambda (n) (make-scm-16bit-integer n)))

   (list 'numaddi (lambda (n) `((push)
								,@(make-scm-16bit-integer n)
								(CALL int16-add))))
   (list 'numadd2 (lambda () '((CALL int16-add))))
   (list 'numincr (lambda () '((CALL int16-incr))))

   (list 'numsubi (lambda (n) `((push)
								,@(make-scm-16bit-integer n)
								(CALL int16-sub))))
   (list 'numsub2 (lambda () '((CALL int16-sub))))
   (list 'numdecr (lambda () '((CALL int16-decr))))

;   (list 'numeqi (lambda (n) `((push)
;								,@(make-scm-16bit-integer n)
;								(CALL int16-sub))))
   (list 'numeq2 (lambda () '((CALL int16-eq))))

   (list 'numzero (lambda () '((CALL int16-zerop))))
   (list 'numone (lambda () '((CALL int16-onep))))

   (list 'cons (lambda () '((CALL cons)) ))

   ; car/cdr
   (list 'car (lambda () '((CALL car)) ))
   (list 'cdr (lambda () '((CALL cdr)) ))
   (list 'caar (lambda () '((CALL car) (CALL car)) ))
   (list 'cadr (lambda () '((CALL cdr) (CALL car)) ))
   (list 'cdar (lambda () '((CALL car) (CALL cdr)) ))
   (list 'cddr (lambda () '((CALL cdr) (CALL cdr)) ))
   (list 'caaar (lambda () '((CALL car) (CALL car) (CALL car)) ))
   (list 'caadr (lambda () '((CALL cdr) (CALL car) (CALL car)) ))
   (list 'cadar (lambda () '((CALL car) (CALL cdr) (CALL car)) ))
   (list 'caddr (lambda () '((CALL cdr) (CALL cdr) (CALL car)) ))
   (list 'cdaar (lambda () '((CALL car) (CALL car) (CALL cdr)) ))
   (list 'cdadr (lambda () '((CALL cdr) (CALL car) (CALL cdr)) ))
   (list 'cddar (lambda () '((CALL car) (CALL cdr) (CALL cdr)) ))
   (list 'cdddr (lambda () '((CALL cdr) (CALL cdr) (CALL cdr)) ))
   (list 'caaaar (lambda () '((CALL car) (CALL car) (CALL car) (CALL car)) ))
   (list 'caaadr (lambda () '((CALL cdr) (CALL car) (CALL car) (CALL car)) ))
   (list 'caadar (lambda () '((CALL car) (CALL cdr) (CALL car) (CALL car)) ))
   (list 'caaddr (lambda () '((CALL cdr) (CALL cdr) (CALL car) (CALL car)) ))
   (list 'cadaar (lambda () '((CALL car) (CALL car) (CALL cdr) (CALL car)) ))
   (list 'cadadr (lambda () '((CALL cdr) (CALL car) (CALL cdr) (CALL car)) ))
   (list 'caddar (lambda () '((CALL car) (CALL cdr) (CALL cdr) (CALL car)) ))
   (list 'cadddr (lambda () '((CALL cdr) (CALL cdr) (CALL cdr) (CALL car)) ))
   (list 'cdaaar (lambda () '((CALL car) (CALL car) (CALL car) (CALL cdr)) ))
   (list 'cdaadr (lambda () '((CALL cdr) (CALL car) (CALL car) (CALL cdr)) ))
   (list 'cdadar (lambda () '((CALL car) (CALL cdr) (CALL car) (CALL cdr)) ))
   (list 'cdaddr (lambda () '((CALL cdr) (CALL cdr) (CALL car) (CALL cdr)) ))
   (list 'cddaar (lambda () '((CALL car) (CALL car) (CALL cdr) (CALL cdr)) ))
   (list 'cddadr (lambda () '((CALL cdr) (CALL car) (CALL cdr) (CALL cdr)) ))
   (list 'cdddar (lambda () '((CALL car) (CALL cdr) (CALL cdr) (CALL cdr)) ))
   (list 'cddddr (lambda () '((CALL cdr) (CALL cdr) (CALL cdr) (CALL cdr)) ))

   (list 'set-car (lambda () '((CALL set-car)) ))
   (list 'set-cdr (lambda () '((CALL set-cdr)) ))

   (list 'push ;; save-w,
		 (lambda args
		   (if (null? args)
			   ;; (push)
			   '((CALL  save-w))
			   ;; (push reg)
			   (let1 reg (car args)
				 (if (eq? 'w reg)
					 '((CALL  save-w))
					 `((-- push ,reg)
					   (MOVF  ,reg W)
					   (CALL  save-w))
					 )))))

   (list 'pop
		 (lambda args
		   (if (null? args)
			   '((CALL  restore-w))
			   (let1 reg (car args)
				 (if (eq? 'w reg)
					 '((CALL  restore-w))
					 `((-- push ,reg)
					   (CALL  restore-w)
					   (MOVWF ,reg))
					 )))))

   (list 'eval (lambda () '((CALL eval)) ))

;   (list 'print (lambda () '((CALL print)) )) ;; for debug

   (list 'display-int16 (lambda () '((CALL display-int16))))

   (list 'display
		 (lambda args
		   (if (null? args)
			   `((LED/display-w))
			   (let1 arg (car args)
				 (cond [(symbol? arg)
						`((LED/display-reg ,arg))]
					   [(number? arg)
						`((LED/display-literal ,arg))]
					   [else
						'()]
					   )))))
   ))
