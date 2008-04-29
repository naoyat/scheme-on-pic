(disasm (lambda ()
;;; higepon #2
(define cont #f)
(define a (lambda ()
			(+ 1
			   (+ 2 3)
			   (call/cc (lambda (c) (set! cont c) 4))
			   (+ 5 6))))

(print (a))
;(print (cont 5))
(cont 5)
))
