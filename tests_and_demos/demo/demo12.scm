;;
;; demo11 scm-object
;;
(require "./demo-base")

(define code021 '(
				  (assign continue (label nine))

;				  (save (label one))
				  (goto (reg continue))

;				  (label one)
				one
				  (LED/display-literal 1)

;				  (label seven)
				seven
				  (LED/display-literal 7)
				  (GOTO $)

;				  (label nine)
				nine
				  (LED/display-literal 9)
				  (GOTO $)
				  ))

(define code299 '(
				  (assign a 15)
				  (assign b 10)
				test-b
;				  (test (op =) (reg b) (const 0))
				  (test (= b 0))
				  (branch (label gcd-done))
				  (assign t (reg a))
				rem-loop
;				  (test (op <) (reg t) (reg b))
				  (test (< t b))
				  (branch (label rem-done))
;				  (assign t (op -) (reg t) (reg b))
				  (assign t (- t b))
				  (goto (label rem-loop))
				rem-done
				  (assign a (reg b))
				  (assign b (reg t))
				  (goto (label test-b))
				gcd-done
				  (LED/display-reg a) ; 10
				  (LED/display-reg b) ; 5
				  ))

(define code299g '(
				  (assign a 15)
				  (assign b 10)
				test-b
				  (test (op =) (reg b) (const 0))
;				  (test (= b 0))
				  (branch (label gcd-done))
				  (assign t (reg a))
				rem-loop
				  (test (op <) (reg t) (reg b))
;				  (test (< t b))
				  (branch (label rem-done))
				  (assign t (op -) (reg t) (reg b))
;				  (assign t (- t b))
				  (goto (label rem-loop))
				rem-done
				  (assign a (reg b))
				  (assign b (reg t))
				  (goto (label test-b))
				gcd-done
;				  (LED/display-reg a) ; 10
				  (LED/display-reg b) ; 5
				  ))

(define code301b '(
				   (GOTO test)

				 gcd
				   (test (= b 0))
				   (branch (label gcd-done))
				   (assign t (rem a b))
				   (assign a (reg b))
				   (assign b (reg t))
				   (goto (label gcd))
				 gcd-done
				   (goto (reg continue))

				 test
				   (assign continue (label after-gcd-1))
				   (goto (label gcd))
				 after-gcd-1
				   (assign continue (label after-gcd-2))
				   (goto (label gcd))
				 after-gcd-2

				   (GOTO $)
				   ))

(define code030 '(
				  (assign continue (label after-1))
;				  (LED/display-literal 1)
				  (display 1)
				  (goto (label test))
;				  (GOTO test)
				after-1
				  (assign continue (label after-2))
;				  (LED/display-literal 2)
				  (display 2)
				  (goto (label test))
;				  (GOTO test)
				after-2
;				  (LED/display-literal 3)
				  (display 3)
;				  (GOTO $)
				  (end)

				test
;				  (LED/display-literal 9)
				  (display 9)
				  (goto (reg continue))
				  ))

(define *disable-writing* #t)
(macro-asm-test '(a b t u v x y z continue)
;				  code299 #t #t #t 5)
				  code299g #t #t #t 5)
;				  code030 #t #t #t 5)
