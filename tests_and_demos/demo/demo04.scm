;;
;; demo04 - ３軸加速度センサの出力をRA4から入力し、A/D変換し、加速度に応じたLEDを表示。
;;
(require "./demo-base")

(define code '(
			 loop
			   (a2d RA4) ; AN2; 使えるのは RA1(=AN1) RA2(=AN2) RA4(=AN3) の３つ
			   ;; RA5 RA4 RA3 RC5 RC4 RC3 RA0 RA1 RA2 RC0 RC1 RC2 +5V GND
			   ;;  -  <3>  -   -   -  <7> <0> <1> <2> <4> <5> <6>
			   ;;         SW1         LED RP1          LED LED LED

;			   (test < 213 a2d-upper)
			   (test < 161 a2d-upper)
			   (branch +2g)
			   
;			   (test < 161 a2d-upper)
			   (test < 145 a2d-upper)
			   (branch +1g)

;			   (test < 61 a2d-upper)
			   (test < 129 a2d-upper)
			   (branch 0g)
			   
;			   (test < 61 a2d-upper)
			   (test < 113 a2d-upper)
			   (branch -1g)

			 -2g
			   (LED #b0011 0.1)
			   (goto loop)
			 -1g
			   (LED #b0010 0.1)
			   (goto loop)
			 0g
			   (LED #b0000 0.1)
			   (GOTO loop)
			 +1g
			   (LED #b0100 0.1)
			   (goto loop)
			 +2g
			   (LED #b1100 0.1)
			   (goto loop)
			   ))

(macro-asm-test '() code #t #t #t 60)
