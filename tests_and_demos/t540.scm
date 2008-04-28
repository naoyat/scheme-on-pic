;; higepon #1
(define cont #f)

(define (a)
  (print 1)
  ((lambda ()
	 (print 2)
	 (call/cc (lambda (c) (set! cont c)))
	 (print 3)))
  (print 4))

(a)

;(print)
(cont 0)

;(define (a)
;  #undef
;  ((lambda ()
;	 #undef
;	 <call/cc..> <-- here
;	 (print 3)))
;  (print 4))
