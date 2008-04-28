(define (identity x) x)

(let ((yin ((lambda (foo) (print 0) foo) ;\n
			(call/cc identity))))
  (let ((yang ((lambda (foo) (print 1) foo) ;*
			   (call/cc identity))))
	(yin yang)))

;(let ((yin ((lambda (foo) (print 0) foo) <call/cc..>))) ;;?
;  (let ((yang ((lambda (foo) (print 1) foo) <call/cc..>))) ;;?
;	(yin yang)))
