(require "./ikoma-pic-scheme")

(use srfi-1)

(define (fib n)
  (print "n = " n)
  (if (= n 0) 1
	  (if (= n 1) 1
		  (+ (fib (- n 1))
			 (fib (- n 2))))))
;(print (fib 3)) ; = fib2 + fib1 = (fib1 + fib0) + fib1 = 1 + 1 + 1 = 3

(define program
  '(
	(define (fib n)
;	  (debug :snapshot)
;	  (debug :scm :pairs)
;	  (debug :scm :stack)
;	  (debug :scm :int16s)
	  (asm (lref0)
		   (print)
		   (GOTO $)
		   )
;	  (asm (lref0)
;		   (DEBUG:scm:w))
;	  (debug :scm :pairs)
;	  (debug :scm :int16s)
;	  (debug :pc)
	  (if (= n 0) 0 ; n
		  (if (= n 1) 1 ; n
			  (+ (fib (- n 1))
				 (fib (- n 2))))))

;	(debug :scm :pairs)
; 	(print (fib 0)) ; 0
;	(print (fib 1)) ; 1
; 	(print (fib 2)) ; 1
;	(print (fib 3)) ; 2
; 	(print (fib 4)) ; 3
	(print (fib 10)) ; 2
	))

(ips program #t #t #f #f)
;(ips program #t #t #t #t)
