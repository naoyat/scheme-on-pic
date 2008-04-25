;;
;; demo10 save/restore
;;
(require "./demo-base")

(define code '(
			   (assign x 1)
			   (assign y 2)
			   (save x)
			   (save y)
			   (assign x 3)
			   (assign x 4)
			   (restore y) ; x: 4 -> 2
			   (restore x) ;// x: 4 -> 1
			   (LED/display-reg x)
			   (LED/display-reg x)
			   (LED/display-reg x)
			   ))

(macro-asm-test '(x y)
;				  code #t #t #f 0)
				  code #t #t); #t 4)
