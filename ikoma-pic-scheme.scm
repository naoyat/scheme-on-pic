;;;
;;; macro-asm-test
;;;
;;;   Copyright (c) 2008 naoya_t (naoya.t@aqua.plala.or.jp)
;;;
(define *disable-writing* #t)
(define *disable-writing* #f)

(use srfi-1)
(use util.match)

;(require "./ips-base")
(require "./scm-object")

(define (extract-defined-vars seq)
  (let loop ([rest seq] [vars '()])
	(if (null? rest) vars
		(loop (cdr rest)
			  (let1 exp (car rest)
				(if (and (pair? exp) (eq? 'define (car exp)))
					(cons (if (pair? (cadr exp)) (caadr exp) (cadr exp)) vars)
					vars)
				)))))

(define (pos-in-env var env)
  (let1 curr-frame (car env)
	(let1 _ (memq var (car curr-frame))
	  (if _ (- (length (car curr-frame)) (length _)) #f))))

(define (lookup var env)
  (let loop ([rest-env env] [dep 0])
	(if (null? rest-env)
		`((mov w ,scm-undefined))
		(let1 curr-frame (car rest-env)
		  (let1 _ (memq var (car curr-frame))
			(if _ `((-- ,var)
					,(scm-lref dep (- (length (car curr-frame)) (length _)))
										;			  (asm (DEBUG:scm:pairs))
					)
				(loop (cdr rest-env) (+ dep 1))
				))))))

(define (check-references-in-env seq env)
  (append-map (lambda (exp) (scm-eval exp env)) seq)
  )

(define (proc-lookup op env)
  (case op
	[(+) 'int16-add]
	[(-) 'int16-sub]
	[(=) 'int16-eq]
	
	[(<) 'cmp-lt]
	[(>) 'cmp-gt]
	[(<=) 'cmp-le]
	[(>=) 'cmp-ge]

	[(print) 'print]
;	[(display) 'display]
	
	[(car cdr cons length) op]
	[else #f]
	))

(define (scm-call-proc operator env)
  ;; ,@(scm-eval operator env))
  ;; '((numadd2))
  `((,(cadr (assoc operator
				   '((+ numadd2)
					 (- numsub2)
					 (< numlt2)
					 (> numgt2)
					 (<= numle2)
					 (>= numge2)
					 (= numeq2)))))))

(define (scm-apply operator operands env)
  (cond [(and (eq? '= operator) (= 2 (length operands)))
		 (case (second operands)
		   [(0) `(,@(scm-eval (car operands) env)
				  (numzero))]
		   [(1) `(,@(scm-eval (car operands) env)
				  (numone))]
		   [else `(,@(scm-eval (car operands) env)
				   (push)
				   ,@(scm-eval (cadr operands) env)
				   (numeq2))]
		   )]
		[(and (eq? '+ operator) (= 2 (length operands)))
		 (case (second operands)
		   [(1) `(,@(scm-eval (car operands) env)
				  (numincr)
				  )]
		   [(2) `(,@(scm-eval (car operands) env)
				  (numincr)
				  (numincr)
				  )]
		   [else `(,@(scm-eval (car operands) env)
				   (push)
				   ,@(scm-eval (cadr operands) env)
				   (numadd2)
				   )]
		   )]
		[(and (eq? '- operator) (= 2 (length operands)))
		 (case (second operands)
		   [(1) `(,@(scm-eval (car operands) env)
				  (numdecr)
				  )]
		   [(2) `(,@(scm-eval (car operands) env)
				  (numdecr)
				  (numdecr)
				  )]
		   [else `(,@(scm-eval (car operands) env)
				   (push)
				   ,@(scm-eval (cadr operands) env)
				   (numsub2)
				   )]
		   )]
		[(memq operator '(+ - < > <= >= =))
		 (let loop ([rest (cdr operands)]
					[code (scm-eval (car operands) env)])
		   (if (null? rest)
			   code
			   (loop (cdr rest)
					 `(,@code
					   (push)
					   ,@(scm-eval (car rest) env)
					   ,@(scm-call-proc operator env)
					   ))))]
		[(memq operator '(print display))
		 (let loop ([rest (cdr operands)]
					[code (scm-eval (car operands) env)])
		   (if (null? rest)
			   `(,@code
				 ,@(scm-eval operator env)
				 )
			   (loop (cdr rest)
					 `(,@code
					   (push)
					   ,@(scm-eval (car rest) env)
					   ))))]
;		[(eq? operator 'display) ...]
		[else
		 (let loop ([rest (cdr operands)]
					[code `(,@(scm-eval (car operands) env)
							(push))])
;		   (print operator " : DEFAULT")
		   (if (null? rest)
			   `(,@code
				 ;; 全ての引数をスタック上に
				 ,@(scm-eval operator env)
				 ;; wにはprocのアドレス
				 (call)
				 )
			
			   (loop (cdr rest)
					 `(,@code
					   ,@(scm-eval (car rest) env)
					   (push)
					   ))))]
		))

(define (scm-list l env)
  (if (null? l)
	  `(
;		(-- w = "()")
		(mov  w ,scm-nil)
		)
;	  `(,@(gauche->pic (car elts))
	  `(,@(scm-eval (car l) env)
;		(CALL   eval)
		(push) ;CALL   save-w)
		,@(scm-list (cdr l) env)
		(cons)
		)))

(define (scm-lset dep pos)
  (if (and (= 0 dep) (<= pos 3))
	  (list (string->symbol #`"lset,pos"))
	  (list 'lset dep pos)))

(define (scm-lref dep pos)
  (cond [(<= 0 (+ dep pos) 3)
		 (if (= 0 dep)
			  (list (string->symbol #`"lref,pos"))
			  (list (string->symbol #`"lref,dep,pos"))
			  )]
		[else (list 'lref dep pos)] ))

(define (scm-eval exp env)
  (cond [(boolean? exp)
		 (if exp
			 `((-- w = #t)
			   (mov  w ,scm-true))
			 `((-- w = #f)
			   (mov  w ,scm-false))
			 )]

		[(symbol? exp)
		 (let1 proc (proc-lookup exp env)
		   (if proc
			   `((CALL ,proc))
			   (lookup exp env)
			   ))]
;		[(number? exp) (make-scm-16bit-integer exp)]

		[(number? exp) `((consti ,exp))]

		[(pair? exp)
		 (case (first exp)
		   [(asm) (cdr exp)]
		   [(debug)
			(case (second exp)
			  [(:snapshot) '((DEBUG:snapshot))]
			  [(:w) '((DEBUG:w))]
			  [(:pc) '((DEBUG:pc))]
			  [(:scm)
			   (case (third exp)
				 [(:w) '((DEBUG:scm:w))]
				 [(:w-pp) '((DEBUG:scm:w-pp))]
				 [(:stack) '((DEBUG:scm:stack))]
				 [(:pairs) '((DEBUG:scm:pairs))]
				 [(:int16s) '((DEBUG:scm:int16s))]
				 [else '()] )]
			  [else '()] )]

		   [(define)
			(if (pair? (cadr exp))
				;; (define (f x) ...) = (define f (lambda (x) ...))
				`((-- ,(caadr exp) <- λ ,(cdadr exp) ...);,@(cddr exp))
				  ,@(scm-proc (cdadr exp) (cddr exp) env)
;				  ,@(scm-eval (list 'lambda (cdadr exp) (cddr exp)) env)
;				  (asm (DEBUG:scm:stack))
;				  (asm (DEBUG:snapshot))
				  ,(scm-lset 0 (pos-in-env (caadr exp) env))
;				  (asm (DEBUG:scm:w)
;					   (DEBUG:scm:pairs))
				  )
;				`((-- ,(caadr exp) <- <closure>)
;				  ,@(scm-make-proc (cdadr exp) (cddr exp) '())
;				  ,(scm-lset 0 (pos-in-env (caadr exp) env))
;				  )
				;; (define x e)
				`((-- ,(cadr exp) <- ,(third exp))
				  ,@(scm-eval (third exp) env)
				  ,(scm-lset 0 (pos-in-env (cadr exp) env))
				  )
				)]

		   [(if)
			(let ([cond (cadr exp)]
				  [then (caddr exp)]
				  [else (cadddr exp)])
			  (let ([else-label (label-gen "else")]
					[fi-label (label-gen "fi")])
				`(,@(scm-eval cond env)
				  (bf ,else-label)
;				if-then
				  ,@(scm-eval then env)
				  (GOTO ,fi-label)
				,else-label
				  ,@(scm-eval else env)
				,fi-label
				  )
				))]
		   [(let)
			(let ([vars (map car (cadr exp))]
				  [vals (map cadr (cadr exp))]
				  [body (cddr exp)])
			  (let1 env+ (cons (cons vars vals) env)
				`((-- ((λ ,vars ,@body) ,@vals))
				  (push  env)
				  ,@(scm-list vals env)
				  (push)
				  (mov   w env)
				  (cons)
				  (mov   env w)
										;
				  ,@(append-map (lambda (exp) (scm-eval exp env+)) body)
										;
				  (mov   t1 w) ;using t1 for temp
				  (pop   env)
				  (mov   w t1)
				  )))]

		   [(lambda)
			(scm-proc (cadr exp) (cddr exp) env)]

		   [else
			(let ([operator (car exp)]
				  [operands (cdr exp)])
			  (scm-apply operator operands env)
			   )]
		   )]
		[else
		 `((--<ELSE> ,exp)
		   )]
		))

(define (scm-proc vars body env)
  (let ([vals '()]
		[nvals (length vars)])
	(let1 env+ (cons (cons vars vals) env)
	  (let ([lambda-label (label-gen "lambda")]
			[skip-lambda-label (label-gen "lambda-end")])
		`((GOTO ,skip-lambda-label)
		  ,lambda-label
		  (-- (λ ,vars ...));,@body))
		  ;; [continue env n n-1 ... 2 1]
		  (pop   t1) ; t1 = continue
		  (pop   t2) ; t2 = env
					
		  (mov   w ,scm-nil) ; () [n ... 1]
		  ,@(make-list nvals '(cons))
		  (push) ; [(arg1 .. argn)]
		  (mov   w env)
		  (cons) ; ((arg1 .. argn) env)

		  (mov   temp w) ; temp = ((arg1 .. argn) env)

		  (push  t2)  ; t2=env
		  (push  t1)  ; t1=continue
		  ;; [continue env]
;		  (asm (DEBUG:scm:stack))
;		  (push  env)
		  (mov   env temp)

		  ,@(append-map (lambda (exp) (scm-eval exp env+)) body)

;		  (mov   temp w) ;using temp for temp
;					(mov   val w)
;					(pop   temp)
;					(pop   continue)
;		  (pop   env)
;		  (mov   w temp)
					;(RETURN)
		  (ret)
		  ,skip-lambda-label

		  ,@(make-scm-proc-object nvals lambda-label)
		  )
		))))

(define (scheme->macro-asm scm-code)
  (let* ([gvars (extract-defined-vars scm-code)]
		 [gvals (make-list (length gvars) scm-undefined)])
	(let1 env (cons (cons gvars gvals) '())
;	  (print "g: " gvars)

	  (append `((-- env = ,gvals)
				,@(scm-list gvals env)
				(push)
				(mov  w ,scm-nil)
				(cons)
				(mov env w))
			  (append-map (lambda (exp) (scm-eval exp env)) scm-code)
			  ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./macro-asm")
(require "./pic-asm")
(require "./pic-disasm")
;(require "./pic-device")

(require "./pic16f690-emulator")
(require "./intelhex-lib") ; save-obj-code
;(require "./pic16f690-repl")

(define (4bit val)
  (string-join (map (lambda (b) (if (logbit? b val) "●" "○"))
					'(3 2 1 0))
			   " "))

(define cnt 0)
(define sometimes #f)
;(define sometimes 1)
;(define sometimes 1)
(define verbose-mode #f)
;(define verbose-mode #t)
(define show-led #f)
;(define verbose-mode #t)

(define (take-before needle l)
  (let1 found (memq needle l)
	(if found
		(take l (- (length l) (length found)))
		l)))

(define (ips scm-code macro-asm? asm? show-util-code? disasm? . rest)
  (let1 macro-asm-code (scheme->macro-asm scm-code)

	(when macro-asm?
	  (map asm-pp macro-asm-code)
	  (print "-------"))

  ;; マクロアセンブラ
	(let1 macro-asm (make-macro-assembler (list plug-in:basic
												;; plug-in:register-machine
												plug-in:wait
												plug-in:LED
												plug-in:scheme
												plug-in:debug
												))
	;;コンパイル
;	(when (and regs (not (null? regs)))
;	  (set! src (append (append-map (lambda (reg) `((alloc ,reg))) regs) src)))

	  (let1 asm-code (macro-asm macro-asm-code)

		(when asm?
										;		(map print asm-code)
										;		(print "-------")
		  (let1 util-code (memq 'utility_subroutines asm-code)
			(map asm-pp 
				 (if show-util-code? asm-code
					 (take-before 'utility_subroutines asm-code)))
			(print "-------")))
		
		;;アセンブル
		(let1 obj-code (pic-asm asm-code)
										;(print obj-code)
										;(print "-------")
		  
		  ;;オブジェクトコードをdisasmして表示（デバッグ用）
		  (when disasm? 
			(map print (pic-disasm obj-code))
			(print "-------"))
		  
;		(when (and (not *disable-writing*) write?)
		  ;;		  ;;オブジェクトコードをPICに書き込んで実行（５秒）
		  ;;		  (pic-write obj-code)
		  (let1 hex-file "_ipstest.hex"
			(save-obj-code obj-code hex-file)
										;		  (sys-system "gosh pic16f690-main.scm _emutest.hex")
			
			(let1 pic (make-pic16f690)
			  ([pic'load-hex-file] hex-file)
			  ;; (print "load hex-file completed")
			  
			  [pic'on]
			  
;			  (set! verbose-mode #t)
			  (let loop ([last-portc -1])
				(when (or verbose-mode (and sometimes (zero? (remainder cnt sometimes))))
				  (if [pic'finished?]
					  (begin (when show-led (printf "> %s\n" (4bit last-portc)))
;							 (dump-snapshot [pic'snapshot])
							 
							 (printf "%08x: %s  ;; %s\n"
									 [pic'pc] (mnemonic-pp [pic'mnemonic])
									 (map (cut sprintf "%04x" <>) [pic'call-stack])
									 ))
					  (print "// waiting 1 cycle... //")
					  ))
				
				;;(profiler-reset)
				;;(profiler-start)
				[pic'step]
				;;(profiler-stop)
				;;(profiler-show)
				(inc! cnt)
				
				(let1 portc (logand #b1111 ([pic'f] 7))
				  (when show-led
					(unless (= portc last-portc)
					  (printf "> %s\r" (4bit portc))
					  (flush)
					  (inc! cnt)))
				  
				  (if verbose-mode
					  (guard (e (else
								 [pic'halt]
								 (printf ";; %08x: %s\n" [pic'pc] [pic'mnemonic])
								 (dump-snapshot [pic'snapshot] 'full-mode)
								 (print "ERROR: " (ref e 'message))
								 ))
						(unless [pic'halt?] (loop portc)))
					  (unless [pic'halt?] (loop portc))
					  )))
			  )))
	  
		))))