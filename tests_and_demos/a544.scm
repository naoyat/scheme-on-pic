(disasm (lambda ()
;; higepon #3
(define cont #f)
(define a (lambda ()
			(if (call/cc (lambda (c) (set! cont c) #f))
				(print 1)
				(print 0))
			(print 9)))

(a)
(cont #t)
))
