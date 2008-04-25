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

			   ;; まだ単なる制御構造
			   (cond
				[(< 161 a2d-upper) (LED #b0011 0.1)] ; +2g
				[(< 145 a2d-upper) (LED #b0010 0.1)] ; +1g
				[(< 129 a2d-upper) (LED #b0000 0.1)] ; +0g
				[(< 113 a2d-upper) (LED #b0100 0.1)] ; -1g
				[else              (LED #b1100 0.1)]); -2g
			   (goto loop)

			   ;; その１歩手前
;			   (if (< 161 a2d-upper) :goto +2g)
;			   (if (< 145 a2d-upper) :goto +1g)
;			   (if (< 129 a2d-upper) :goto 0g)
;			   (if (< 113 a2d-upper) :goto -1g)
;
;			 -2g
;			   (LED #b0011 0.1)
;			   (goto loop)
;			 -1g
;			   (LED #b0010 0.1)
;			   (goto loop)
;			 0g
;			   (LED #b0000 0.1)
;			   (GOTO loop)
;			 +1g
;			   (LED #b0100 0.1)
;			   (goto loop)
;			 +2g
;			   (LED #b1100 0.1)
;			   (goto loop)
;			   ))
			   ))

;(macro-asm-test code #t #t #t 60)
(macro-asm-test '() code #t #t #f 0)
