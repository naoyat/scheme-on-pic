(require "./ikoma-pic-scheme")

(use srfi-1)

(define program
  '(
	(define a 7)
;	(print a) ;7
	(define b 8)
;	(print b) ;8

;	(print (- 100 10 1)) ; 89 ; 1 - 10 - 100
;	(print (if (= 3 2) 10 20))
;	(print (if (= b 7) 77
;			   (if (= b 8) 88 99)))

	(print (+ 9 a b 10)) ;34

	(let ((d 1)
		  (e 10)
		  (f 100))
	  (print (+ b f 1)) ; expects 109 (= 8 + 100 + 1)
	  )

	(define f (lambda (x y) (+ x y a 1)))
;	(print f) ;lambda --> いまは16bit intなのでアドレスを（数値）で表示する

	(print (f 5 15)) ; expects 28 (= 5 + 15 + 7 + 1)
	))

;(ips program #t #t #f #f)
(ips program #t #t #t #t)

