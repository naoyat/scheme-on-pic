;;
;; demo14
;;
(require "./demo-base")

;(define code '(
;			   (DEBUG:nextpc)
;			   (MOVLW #x11)
;			   (DEBUG:w)
;			   (save-w)
;			   (DEBUG:w)
;;			   (DEBUG:reg z)
;			   (DEBUG:w)
;			   (DEBUG:snapshot)
;			   (set! a (cons 1 2))
;			   (DEBUG:snapshot)
;			   ))


(define code '(
;			   (DEBUG:snapshot)
;			   (set! a (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 ))
			   (set! a (list 1 2 3 4 5 6 7))
			   (set! a (list 1 2 3))
			   (DEBUG:snapshot)
;			   (DEBUG:pc)
;			   (DEBUG:scm:stack)
;			   (DEBUG:scm:pairs)
;			   (DEBUG:scm:int16s)

			   (MOVF   a W)
			   (DEBUG:scm:w)
;			   (DEBUG:snapshot)
			   (CALL   len)
			   (DEBUG:w)

			   (set! env (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;			   (DEBUG:scm:pairs)
;			   (MOVLW #b01000111)
			   (set!  a (lref 0 1))
			   (set!  b (lref 1 2))
			   (set!  c (lref 2 0))
			   (MOVF  a W)
			   (eval)
			   (DEBUG:scm:w)

			   (MOVF  b W)
			   (eval)
			   (DEBUG:scm:w)

			   (MOVF  c W)
			   (eval)
			   (DEBUG:scm:w)

;			   (set! d (+ 10 20 30 40 50))
;			   (MOVF  d W)
;			   (DEBUG:scm:w) ; 150 ok.

;			   (set! (let ((x 1) (y 2) (z 3))
										;					   (+ 1 2 3)))
			   (let ((x 1) (y 2) (z 3))
				 (set! d (+ x y z 1))
				 )
;				 (+ x y z 1))
			   (MOVF  d W)
			   (DEBUG:scm:w)

;			   (consti 1000)
;			   (numaddi 2000)
;			   (numaddi 3000)
;			   (numaddi 4000)
;			   (MOVF  a W)
;			   (eval)
;			   (DEBUG:scm:w)
;			   (MOVF env w)
;			   (caddr)
;			   (cadr)
;			   (DEBUG:scm:w)

;			   (DEBUG:scm:pairs)
;			   (lref12)
;			   (DEBUG:scm:w)
;			   (DEBUG:snapshot)

			   (GOTO   $)

			 len
;			   (DEBUG:scm:w)
;			   (DEBUG:file top-of-stack)
			   (INCF   top-of-stack F) ; [>??]
;			   (DEBUG:file top-of-stack) ; now 38h
;			   (DEBUG:scm:stack)
			   (push) ; [>w ??]
			   (nullp)

			   (bt     len-zero)
			   (pop) ; w< [??]
;			   (DEBUG:scm:w)
			   
			   (push) ; [>w ??]
			   (pairp)
			   (bf     len-error)
			   
			   ;; [top-of-stack-1] = 0
			   (MOVF   top-of-stack W)
			   (MOVWF  FSR)
			   (DECF   FSR F)
			   (CLRF   INDF) ; [w 0]
;			   (DEBUG:scm:stack)

			 len-loop
			   (DEBUG:pc)
			   (DEBUG:scm:stack)
			   (DEBUG:scm:pairs)
			   (DEBUG:scm:int16s)
			   ; [w 0]

			   ;; [top-of-stack-1]++
			   (MOVF   top-of-stack W)
			   (MOVWF  FSR)
			   (DECF   FSR F)
			   (INCF   INDF F) ; [w (len+1)

			   (pop) ; w< [(len+1)]
			   (cdr)
			   (push) ; [>w ??]
			   (nullp)
			   (bf     len-loop)

			 len-end
			   (pop)
			   (pop)
			   (RETURN)
			 
			 len-zero
			   (pop)
			   (pop)
			   (RETLW  0)

			 len-error
			   (pop)
			   (pop)
			   (RETLW  #x8e); scm-undefined)
			   ))

;(define *disable-writing* #t)
(macro-asm-test '(a b c d) code
				#t #t #f #f #t #t )
				;#f #f )


