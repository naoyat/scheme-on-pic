(define x #f)

(begin
  (print 0)
  (print 1)
  (call/cc (lambda (z) (set! x z)))
  (print 2)
  (print 3))
										;;	(print x)
(x #t)
