;;
;; demo05 fibonacci
;;
(require "./demo-base")

(define code '(
			   (assign n 3)

			   (assign continue (label fib-done))
			 fib-loop
			   ;(if (< n 2) (goto immediate-answer))
			   (test < n 2)
			   (branch immediate-answer)

			   (save continue)
			   (assign continue (label afterfib-n-1))
			   (save n)
			   (assign n - n 1)
			   (goto fib-loop)

			 afterfib-n-1
			   (restore n)
			   (restore continue)

			   (assign n - n 2)
			   (save continue)
			   (assign continue (label afterfib-n-2))
			   (save val)
			   (goto fib-loop)

			 afterfib-n-2
			   (assign n val)
			   (restore val)
			   (restore continue)
			   (assign val + val n)
			   (goto (reg continue))
			   
			 immediate-answer
			   (assign val n)
			   (goto (reg continue))
			 fib-done

			   (LED/display-reg val)
			   (LED/display-reg val)
			   ))

(macro-asm-test '(n); val continue)
				  code #t #t); #t 5)
