;;
;; demo02 - GCD (最大公約数) を計算し、結果をLEDで2進数表示する
;;
(require "./demo-base")

(define code '(
			   ;;(alloc a)
			   ;;(alloc b)
			   ;;(alloc t)
			   
			   (assign a 24)       ; 24と15の最大公約数は？
			   (assign b 16)

			   ; gcd
			 test-b
			   (test = b 0)
			   (branch gcd-done)   ; goto gcd-done if b == 0
			   (assign t a)        ; t = a
			 rem-loop
			   (test < t b)
			   (branch rem-done)   ; goto rem-done if t < b
			   (assign t - t b)    ; t = t - b
			   (goto rem-loop)
			 rem-done

			   (assign a b)        ; a = b
			   (assign b t)        ; b = t
			   (goto test-b)
			 gcd-done

			   (LED/display-reg a)
			   (LED/display-reg a)
			   (LED/display-reg a)
			   ))

(macro-asm-test '(a b t) code #t #t #t 5)
