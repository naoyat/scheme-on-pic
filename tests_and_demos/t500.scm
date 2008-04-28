(print (+ 20 30 (call/cc (lambda (cont) (+ 5 10))))) ; expects 65

;; (+ 20 30 <call/cc...>) ?
;; (print (+ 20 30 <call/cc...>)) ? たぶんこっちで動いてる

