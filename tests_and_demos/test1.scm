(require "./demo-base")


(define code '(
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
			   (CALL   length)
			   (DEBUG:w)

			   (set! env (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;			   (DEBUG:scm:pairs)
;			   (MOVLW #b01000111)
			   (set!  a (lref 0 1))
			   (set!  b (lref 1 2))
			   (set!  c (lref 2 0))
			   (MOVF  a W)
			   (eval)
;			   (DEBUG:scm:w) ;2

			   (MOVF  b W)
			   (eval)
;			   (DEBUG:scm:w) ;6

			   (MOVF  c W)
			   (eval)
;			   (DEBUG:scm:w) ;7

;			   (set! d (+ 10 20 30 40 50))
;			   (MOVF  d W)
;			   (DEBUG:scm:w) ; 150 ok.

;			   (set! (let ((x 1) (y 2) (z 3))
										;					   (+ 1 2 3)))
			   (let ((x 11) (y 22) (z 33))
				 (DEBUG:scm:pairs)
				 (+ x 2);y z 1 2)
				 )
;				 (+ x y z 1))
;			   (MOVF  d W)
			   (DEBUG:scm:w)
			   (DEBUG:scm:pairs)
			   (DEBUG:snapshot)

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
			   ))

;(define *disable-writing* #t)
(macro-asm-test '(a b c d) code
				#t #f)
				;#f #f )


