;;
;; GEN#5
;;
(require "./demo-base")

(define code '(
			   (alloc x)
			   (alloc y)
			   (alloc z)

			   (assign z (cons 1 2))
			   (assign x (car z))
			   (assign y (cdr z))

			   (LED/display-reg x)
			   (LED/display-reg y)
			   ))

(macro-asm-test '() code #t #t #t 60)
