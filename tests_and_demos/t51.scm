(define cont #f)

(print (+ 5 (call/cc (lambda (c) (begin (set! cont c) 1))))) ;; expects 6 (ok)

(cont 11) ; 16
(cont 21) ; 26
