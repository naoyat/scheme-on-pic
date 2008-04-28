;;; higepon #2
(define cont #f)
(define (a)
  (+ 1
	 (+ 2 3)
	 (call/cc (lambda (c) (set! cont c) 4))
	 (+ 5 6)))

(print (a))

(print (cont 5))
