(print (+ 20 30 (call/cc (lambda (cont) (+ 5 10)))))
; expects 65
