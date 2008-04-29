(disasm (lambda ()
(print (+ 3 (call/cc (lambda (c) (+ 3 (c 4)))))) ; 7 ; why doesn't stop...
))

;(+ 3 <call/cc..>)
;(print (+ 3 <call/cc..>))
;
; 3 << <call/cc> << +
; << print
; この辺りで落ちないと
; 
