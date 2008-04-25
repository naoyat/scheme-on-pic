;;
;; demo10a save/restore
;;
(require "./demo-base")

(define code '((assign x 1) ; x=1
               (assign y 2) ; y=2
                   ; x=1, y=2, [...]

               (LED/display-reg x) ;; 1を表示
               (LED/display-reg y) ;; 2を表示

               (save x)     ; 1 >> [...]
               (save y)     ; 2 >> [1 ...]
                   ; x=1, y=2, [2 1 ...]

               (LED/display-reg x) ;; 1を表示
               (LED/display-reg y) ;; 2を表示

               (assign x 3) ; x=3
               (assign y 4) ; y=4
                   ; x=3, y=4, [2 1 ...]

               (LED/display-reg x) ;; 3を表示
               (LED/display-reg y) ;; 4を表示

               ;; わざとクロスしてrestore
               (restore x)  ; x << [2 1 ...]
               (restore y)  ; y << [1 ...]
                   ; x=2, y=1, [...]

               (LED/display-reg x) ;; 2を表示
               (LED/display-reg y) ;; 1を表示
			   ))

(macro-asm-test '(x y)
;				  code #t #t #f 0)
				  code #t #t #t 4)
