;(print f) ;142=8Eh=undefined

(define f (lambda (x y) (+ x y 1)))
;(print 1)
(print f) ;lambda --> いまは16bit intなのでアドレスを（数値）で表示する

;	(f 5 15) ; expects 21
(print (f 5 15)) ; expects 21
;	(print 0)
;	(debug :scm :int16s)
;	(debug :snapshot)
;	(define x (if (< a b) #t #f))
;	(define (g x) (+ x 2))

;	(asm (DEBUG:scm:int16s))
;	(GOTO $)
;	(print 1)
