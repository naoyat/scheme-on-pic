(define (fib n)
;	  (debug :snapshot)
;	  (debug :scm :pairs)
;	  (debug :scm :stack)
;	  (debug :scm :int16s)
  (asm (lref0)
	   (DEBUG:scm:w))
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
(print (fib 3)) ; 2
;	(print (fib 4)) ; 3
