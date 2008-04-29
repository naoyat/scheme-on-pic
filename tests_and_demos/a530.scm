(disasm (lambda ()
(define cont #f)

(print (+ 5 (call/cc (lambda (c) (begin (set! cont c) 1))))) ;; expects 6 (ok)

;;cont = (+ 5 <call/cc..>) ではなくて
;;cont = (print (+ 5 <call/cc..>)) で動いてるっぽい

(cont 11) ; 16
(cont 21) ; 26
))
