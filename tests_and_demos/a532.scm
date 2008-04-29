(disasm (lambda ()
(define cont #f)

(print (+ 6 (+ 5 (call/cc (lambda (c) (begin (set! cont c) 1))) 10) 12)) ;; expects 34

;;cont = (+ 5 <call/cc..>) ではなくて
;;cont = (print (+ 5 <call/cc..>)) で動いてるっぽい

(cont 11) ; 44 = 6 + (5 + * + 10) + 12 = 33 + 11 = 44
(cont 21) ; 54 = 6 + (5 + * + 10) + 12 = 33 + 21 = 54
))
