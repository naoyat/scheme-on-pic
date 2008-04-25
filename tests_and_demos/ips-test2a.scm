(require "./ikoma-pic-scheme")

(use srfi-1)

(define program
  '(
	(define (f x) (lambda (y) (+ x y)))

	(define g (f 1))
	
	(print (g 10))
	(print (g 100))
	))

;(ips program #t #t #f #f)
(ips program #t #t #t #t)

