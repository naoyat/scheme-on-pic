(define (f)
  (call/cc (lambda (escape)
			 (begin (print 1)
					(print 2)
					(escape 10) ; call/cc の外に大域脱出！
					(print 3)))))

;; (define (f) <call/cc..>)

(print (f)) ; 1, 2, 10
