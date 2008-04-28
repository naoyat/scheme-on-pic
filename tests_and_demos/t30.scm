(define (f x) (lambda (y) (+ x y)))

(define g (f 1))

(print (g 10))
(print (g 100))
