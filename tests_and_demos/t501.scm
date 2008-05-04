;; higepon #3
(define cont #f)
(define a (lambda ()
			(print 1000)
			(print (call/cc (lambda (c) (set! cont c) 0)))))

(a)
(a)
(cont 10)
(cont 20)


