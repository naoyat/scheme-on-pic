;; higepon #3
(define cont #f)
(define (a)
  (if (call/cc (lambda (c) (set! cont c) #t))
	  (print 1)
	  (print 0))
  (print 9))

(a)
(cont #f)
