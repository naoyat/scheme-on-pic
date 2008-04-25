;;
;; demo11 scm-object
;;
(require "./demo-base")

(define code-fib305 '(
					  (assign n 13)

					  (assign continue (label fib-done))
					  (assign label1 (label afterfib-n-1))
					  (assign label2 (label afterfib-n-2))
					fib-loop
					  ;;(display 8)
					  (test (op <) (reg n) (const 2))
					  (branch (label immediate-answer))
					  ;;
					  (save continue)
;					  (assign continue (label afterfib-n-1))
					  (assign continue label1)
					  (save n)
					  (assign n (op -) (reg n) (const 1))
;					  (DECF n F)
					  (goto (label fib-loop))

					afterfib-n-1
					  ;;(display 9)
					  (restore n)
					  ;;(restore continue)
					  ;;
					  (assign n (op -) (reg n) (const 2))
;					  (DECF n F)
;					  (DECF n F)
					  ;;(save continue)
;					  (assign continue (label afterfib-n-2))
					  (assign continue label2)
					  (save val)
					  (goto (label fib-loop))
					afterfib-n-2
					  ;;(display 10)
					  (assign n (reg val))
					  (restore val)
					  (restore continue)
					  (assign val (op +) (reg val) (reg n))
					  (goto (reg continue))
					immediate-answer
					  ;;(display 11)
					  (assign val (reg n))
					  (goto (reg continue))
					fib-done

					  (display val)
					  (display val)
					  (display val)
					  (end)
					))



(define code-fib '(
					  (assign label0 (label fib-done))
					  (assign label1 (label afterfib-n-1))
					  (assign label2 (label afterfib-n-2))

					  (assign n 0)
					test-loop
					  (test (op <) (const 13) (reg n))
					  (branch (label test-done))

					  (assign continue label0) ; fib-done
					  (save n)
					  (goto (label fib-loop))
				    fib-done
				      (display val)
					  (restore n)
					  (assign n (op +) (reg n) (onst 1))
					  (goto (label test-loop))

					fib-loop
					  ;;(display 8)
					  (test (op <) (reg n) (const 2))
					  (branch (label immediate-answer))
					  ;;
					  (save continue)
;					  (assign continue (label afterfib-n-1))
					  (assign continue label1)
					  (save n)
					  (assign n (op -) (reg n) (const 1))
;					  (DECF n F)
					  (goto (label fib-loop))

					afterfib-n-1
					  ;;(display 9)
					  (restore n)
					  ;;(restore continue)
					  ;;
					  (assign n (op -) (reg n) (const 2))
;					  (DECF n F)
;					  (DECF n F)
					  ;;(save continue)
;					  (assign continue (label afterfib-n-2))
					  (assign continue label2)
					  (save val)
					  (goto (label fib-loop))
					afterfib-n-2
					  ;;(display 10)
					  (assign n (reg val))
					  (restore val)
					  (restore continue)
					  (assign val (op +) (reg val) (reg n))
					  (goto (reg continue))
					immediate-answer
					  ;;(display 11)
					  (assign val (reg n))
					  (goto (reg continue))
;					fib-done
					
				    test-done
				      (end)
					))

										;(define *disable-writing* #t)
(macro-asm-test ;'(n val continue   label0 label1 label2)
				'(n label0 label1 label2)
				code-fib #t #t #t 30)
;				code-fib305 #t #t #t 10)

;; 1 1 2 3 5 8 13 21
;; 34 55 89 144 233ぐらいまで
;; 01 01 02 03 05 08 0D 15
;; 22 37 59 90 E9