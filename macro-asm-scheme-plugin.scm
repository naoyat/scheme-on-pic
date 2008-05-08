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
;		 (lambda (arg) `((GOTO ,(label-name arg)))))
  (define (goto arg)
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
		   '()] ))

  (define (jump) ;; <19>
	`((push)
	  (int16-upper)
	  (MOVWF  PCLATH)
	  (pop)
	  (int16-lower)
	  (MOVWF  PCL)))

  (define (ret) ;; <20>
	`(;;(mov    val w) ; for return value
;;				  (DEBUG:file 99)
;;				  (DEBUG:scm:stack)
	  (mov    w continue)
	  (int16-upper)
	  (mov    PCLATH w)
	  (mov    w continue)
	  (int16-lower)
	  (mov    PCL w)
	  ))

  (define (lset dep ofs) ;; <50>
	(cond [(and (= dep 0) (<= ofs 3))
		   `((CALL ,(string->symbol #`"lset,ofs")))]
		  [(and (<= dep 3) (<= ofs 3))
		   `((CALL ,(string->symbol #`"lset,dep,ofs")))]
		  [else
		   `((-- (lset ,dep ,ofs))
			 )]
		  ))

  (define (lref dep ofs) ;; <57>
	(cond [(and (= dep 0) (<= ofs 3))
		   `((CALL ,(string->symbol #`"lref,ofs")))]
		  [(and (<= dep 3) (<= ofs 3))
		   `((CALL ,(string->symbol #`"lref,dep,ofs")))]
		  [else
		   `((-- (lref ,dep ,ofs))
			 )]
		  ))

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

   ;; macro assembler
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

   ;; int16
   (list 'int16 (lambda () '((CALL int16)) ))
   (list 'int16p (lambda () '((CALL int16p))))
   (list 'int16-lower (lambda () '((CALL int16-lower)) )) ; Wレジスタに入れたScmInt16のlowerをWに返す
   (list 'int16-upper (lambda () '((CALL int16-upper)) )) ; Wレジスタに入れたScmPairのupperをWに返す

   ;;;;;;;;;;;;;;
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

   ;;
   ;; VM instructions
   ;;
   (list 'nop (lambda () '())) ;; 0
   ;;## CONST ;; 1
   (list 'consti (lambda (n) (make-scm-16bit-integer n))) ;; 2
   (list 'constn (lambda () `((MOVLW ,scm-nil)))) ;; 3
   (list 'constf (lambda () `((MOVLW ,scm-false)))) ;; 4
   (list 'constu (lambda () `((MOVLW ,scm-undefined)))) ;; 5
   ;;## CONST-PUSH ;; 6
   (list 'consti-push (lambda (n) `(,@(make-scm-16bit-integer n) (push)))) ;; 7
   (list 'constn-push (lambda () `((MOVLW ,scm-nil) (push)))) ;; 8
   (list 'constf-push (lambda () `((MOVLW ,scm-false) (push)))) ;; 9
   ;;## CONST-RET ;; 10
   (list 'constf-ret (lambda () `((MOVLW ,scm-false) ;; 11
								  ,@(ret))))
   (list 'constu-ret (lambda () `((MOVLW ,scm-undefined) ;; 12
								  ,@(ret))))
   (list 'push ;; 13 (save-w)
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

   (list 'pop ;; --
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

   ;;## PRE_CALL ;; 14
   ;;## PUSH_PRE_CALL ;; 15
   ;;## CHECK_STACK ;; 16
   (list 'call (lambda () ;; 17
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

   ;;## TAIL_CALL ;; 18
   (list 'jump (lambda () (jump))) ;; 19
   (list 'ret (lambda () (ret))) ;; 20
   ;;## DEFINE 21
   ;;## CLOSURE 22
   ;;## LOCAL_ENV 23
   ;;## PUSH_LOCAL_ENV 24
   ;;## LOCAL_ENV_CLOSURES 25
   ;;## POP_LOCAL_ENV 26
   ;;## LOCAL_ENV_JUMP 27
   ;;## LOCAL_ENV_CALL 28
   ;;## LOCAL_ENV_TAIL_CALL 29

   (list 'bf ;; 30 branch if false
		 (lambda (label)
		   `((-- bf ":" ,label)
			 (XORLW  ,scm-false)
			 (BTFSC  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-false
			 )))

   (list 'bt ;; 31 branch if true == unless #f
		 (lambda (label)
		   `((-- bt ":" ,label)
			 (XORLW  ,scm-false)
			 (BTFSS  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==0 ; ie. w != scm-false
			 )))

   ;;## BNEQ ;; 32
   ;;## BNEQV ;; 33
   
   (list 'bnull ;; - branch if nil
		 (lambda (label)
		   `((-- bnull ":" ,label)
			 (XORLW  ,scm-nil)
			 (BTFSC  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-nil
			 )))
   
   (list 'bnnull ;; 34 branch if not nil
		 (lambda (label)
		   `((-- bnnull ":" ,label)
			 (XORLW  ,scm-nil)
			 (BTFSS  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-nil
			 )))

   ;;## BNUMNE ;; 35
   ;;## BNLT ;; 36
   ;;## BNLE ;; 37
   ;;## BNGT ;; 38
   ;;## BNGE ;; 39
   ;;## BNUMNEI ;; 40
   ;;## BNEQC ;; 41
   ;;## BNEQVC ;; 42

   (list 'bundef ;; - branch if undefined
		 (lambda (label)
		   `((-- bundef ":" ,label)
			 (XORLW  ,scm-undefined)
			 (BTFSC  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-false
			 )))

   (list 'bdef ;; - branch unless undefined
		 (lambda (label)
		   `((-- bdef ":" ,label)
			 (XORLW  ,scm-undefined)
			 (BTFSS  STATUS Z)
			 (GOTO   ,label) ; goto :label if Z==1 ; ie. w == scm-false
			 )))

   ;;## RF ;; 43
   ;;## RT ;; 44
   ;;## RNEQ ;; 45
   ;;## RNEQV ;; 46
   ;;## RNNULL ;; 47
   ;;## RECEIVE ;; 48
   ;;## TAIL_RECEIVE ;; 49

   ;; lset
   (list 'lset (lambda (dep ofs) (lset dep ofs))) ;; 50
   (list 'lset0 (lambda () '((CALL lset0)))) ;; 51
   (list 'lset1 (lambda () '((CALL lset1)))) ;; 52
   (list 'lset2 (lambda () '((CALL lset2)))) ;; 53
   (list 'lset3 (lambda () '((CALL lset3)))) ;; 54
   (list 'lset4 (lambda () '((CALL lset4)))) ;; 55
   (list 'lset10 (lambda () '((CALL lset10))))
   (list 'lset11 (lambda () '((CALL lset11))))
   (list 'lset12 (lambda () '((CALL lset12))))
   (list 'lset20 (lambda () '((CALL lset20))))
   (list 'lset21 (lambda () '((CALL lset21))))
   (list 'lset30 (lambda () '((CALL lset30))))
   (list 'lset31 (lambda () '((CALL lset31))))
   ;### gset ;; 56
   (list 'lref (lambda (dep ofs) (lref dep ofs))) ;; 57
   (list 'lref0 (lambda () '((CALL lref0)))) ;; 58
   (list 'lref1 (lambda () '((CALL lref1)))) ;; 59
   (list 'lref2 (lambda () '((CALL lref2)))) ;; 60
   (list 'lref3 (lambda () '((CALL lref3)))) ;; 61
   (list 'lref10 (lambda () '((CALL lref10)))) ;; 62
   (list 'lref11 (lambda () '((CALL lref11)))) ;; 63
   (list 'lref12 (lambda () '((CALL lref12)))) ;; 64
   (list 'lref20 (lambda () '((CALL lref20)))) ;; 65
   (list 'lref21 (lambda () '((CALL lref21)))) ;; 66
   (list 'lref30 (lambda () '((CALL lref30)))) ;; 67

   (list 'lref4 (lambda () '((CALL lref4)))) ;; 68 <deprecated>
   (list 'lref13 (lambda () '((CALL lref13)))) ;; 69 <deprecated>
   (list 'lref14 (lambda () '((CALL lref14)))) ;; 70 <deprecated>
   ;; PUSH合成命令<*>はGauche VMとの互換性（というか比べやすさ）のために作ったもの。
   ;; 効率がよいとかいうことは特にない
   (list 'lref-push (lambda (dep ofs) `(,@(lref dep ofs) (push)))) ;; <*> 71
   (list 'lref0-push (lambda () '((CALL lref0) (push)))) ;; <*> 72
   (list 'lref1-push (lambda () '((CALL lref1) (push)))) ;; <*> 73
   (list 'lref2-push (lambda () '((CALL lref2) (push)))) ;; <*> 74
   (list 'lref3-push (lambda () '((CALL lref3) (push)))) ;; <*> 75
   (list 'lref10-push (lambda () '((CALL lref10) (push)))) ;; <*> 76
   (list 'lref11-push (lambda () '((CALL lref11) (push)))) ;; <*> 77
   (list 'lref12-push (lambda () '((CALL lref12) (push)))) ;; <*> 78
   (list 'lref20-push (lambda () '((CALL lref20) (push)))) ;; <*> 79
   (list 'lref21-push (lambda () '((CALL lref21) (push)))) ;; <*> 70
   (list 'lref30-push (lambda () '((CALL lref30) (push)))) ;; <*> 81
   (list 'lref4-push (lambda () '((CALL lref4) (push)))) ;; <*> 82 <deprecated>
   (list 'lref13-push (lambda () '((CALL lref13) (push)))) ;; <*> 83 <deprecated>
   (list 'lref14-push (lambda () '((CALL lref14) (push)))) ;; <*> 84 <deprecated>

   ;;## GREF 85
   ;;## GREF-PUSH 86
   ;;## GREF_CALL 87
   ;;## GREF_TAIL_CALL 88
   ;;## PUSH_GREF 89
   ;;## PUSH_GREF_CALL 90
   ;;## PUSH_GREF_TAIL_CALL 91
   ;;## LREF0-PUSH_GREF 92
   ;;## LREF0-PUSH_GREF_CALL 93
   ;;## LREF0-PUSH_GREF_TAIL_CALL 94

   ;;## PROMISE 95
   ;;## CONST_APPLY 96

   (list 'cons (lambda () '((CALL cons)) )) ;; 97
   (list 'cons-push (lambda () '((CALL cons) ;; <*> 98
								 (push))))
   ; car/cdr
   (list 'car (lambda () '((CALL car)) )) ;; 99
   (list 'car-push (lambda () '((CALL car) ;; <*> 100
								(push))))
   (list 'cdr (lambda () '((CALL cdr)) )) ;; 101
   (list 'cdr-push (lambda () '((CALL cdr) ;; <*> 102
								(push))))
   (list 'caar (lambda () '((CALL car) (CALL car)) )) ;; 103
   (list 'caar-push (lambda () '((CALL car) (CALL car) ;; <*> 104
								 (push))))
   (list 'cadr (lambda () '((CALL cdr) (CALL car)) )) ;; 105
   (list 'cadr-push (lambda () '((CALL cdr) (CALL car) ;; <*> 106
								 (push))))
   (list 'cdar (lambda () '((CALL car) (CALL cdr)) )) ;; 107
   (list 'cdar-push (lambda () '((CALL car) (CALL cdr) ;; <*> 108
								 (push))))
   (list 'cddr (lambda () '((CALL cdr) (CALL cdr)) )) ;; 109
   (list 'cddr-push (lambda () '((CALL cdr) (CALL cdr) ;; <*> 110
								 (push))))
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

   (list 'set-car (lambda () '((CALL set-car)) )) ;;
   (list 'set-cdr (lambda () '((CALL set-cdr)) )) ;;

   ;;## LIST 111
   ;;## LIST_STAR 112
   ;;## LENGTH 113
   ;;## MEMQ 114
   ;;## MEMV 115
   ;;## ASSQ 116
   ;;## ASSV 117
   ;;## EQ 118
   ;;## EQV 119
   ;;## APPEND 120
   ;;## NOT 121
   ;;## REVERSE 122
   ;;## APPLY 123
   ;;## TAIL_APPLY 124
   ;;## IS_A 125

   (list 'nullp (lambda () '((CALL nullp)))) ;; 126
   (list 'pairp (lambda () '((CALL pairp)))) ;; 127
   ;;## CHARP 128
   (list 'eofp (lambda () '((CALL eofp)))) ;; 129
   (list 'lrefp (lambda () '((CALL lrefp)))) ;; -
   ;; STRINGP 130
   ;; SYMBOLP 131
   ;; VECTORP 132
   ;; IDENTIFIERP 133
   ;; SETTER 134
   ;; VALUES 135
   ;; VEC 136
   ;; LIST2VEC 137
   ;; APP_VEC 138
   ;; VEC_LEN 139
   ;; VEC_REF 140
   ;; VEC_SET 141
   ;; VEC_REFI 142
   ;; VEC_SETI 143
   (list 'numeq2 (lambda () '((CALL int16-eq)))) ;; 144
   ;; NUMLT2 145
   ;; NUMLE2 146
   ;; NUMGT2 147
   ;; NUMGE2 148
   (list 'numadd2 (lambda () '((CALL int16-add)))) ;; 149
   (list 'numsub2 (lambda () '((CALL int16-sub)))) ;; 150
   ;; NUMMUL2 151
   ;; NUMDIV2 152
   ;; NEGATE 153
   ;; NUMIADD2 154
   ;; NUMISUB2 155
   ;; NUMIMUL2 156
   ;; NUMIDIV2 157
   (list 'numaddi (lambda (n) `((push) ;; 158
								,@(make-scm-16bit-integer n)
								(CALL int16-add))))
   (list 'numsubi (lambda (n) `((push) ;; 159
								,@(make-scm-16bit-integer n)
								(CALL int16-sub))))
;   (list 'numeqi (lambda (n) `((push)
;								,@(make-scm-16bit-integer n)
;								(CALL int16-sub))))

   (list 'numincr (lambda () '((CALL int16-incr)))) ;; -
   (list 'numdecr (lambda () '((CALL int16-decr)))) ;; -

   (list 'numzero (lambda () '((CALL int16-zerop)))) ;;-
   (list 'numone (lambda () '((CALL int16-onep)))) ;;-

   ;;
   ;; READ_CHAR 160
   ;; PEEK_CHAR 161
   ;; WRITE_CHAR 162
   ;; CURIN 163
   ;; CUROUT 164
   ;; CURERR 165
   ;; SLOT_REF 166
   ;; SLOT_SET 167
   ;; SLOT_REFC 168
   ;; SLOT_SETC 169
   ;; RECEIVE_ALL 170
   ;; TAIL_RECEIVE_ALL 171
   ;; VALUES_N 172
   ;; PUSH_HANDLERS 173
   ;; POP_HANDLERS 174

   ))
