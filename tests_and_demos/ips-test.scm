(require "./ikoma-pic-scheme")

(use srfi-1)

(define program
  '(
;	(asm (MOVLW 1)
;		 (GOTO end))
	(define a 7)
	(print a) ;7
	(define b 8)
	(print b) ;8

;;	(define c (+ 9 a b 10))
;;	(print c)
	(print (+ 9 a b 10)) ;34

	(let ((d 1)
		  (e 10)
		  (f 100))
;	  (define c (+ 1 2 3))
;	  (print c)
;	  (print (+ 1 2 3))
;	  (print (+ d e f))
	  (print (+ b f 1)) ;109
	  )
;	(debug :scm :pairs)
;	(asm (DEBUG:scm:pairs))
;	(debug :snapshot)
;	(asm (lref 0 0))
;	(debug :scm :w)

;	(define c (+ a b))
	(define f (lambda (x y) (+ x y 1)))
	(print f) ;lambda --> いまは16bit intなのでアドレスを（数値）で表示する

	(print (f 5 15)) ; expects 111
;	(debug :scm :int16s)
;	(debug :snapshot)
;	(define x (if (< a b) #t #f))
;	(define (g x) (+ x 2))

;	(debug :snapshot)
;	(asm (DEBUG:snapshot))
;	(display c)
;	(asm end)
	))

(ips program #t #t #f #f)
;(ips program #t #t #t #t)

