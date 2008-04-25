;;
;; demo03 - RA0への電圧入力をA/Dで変換しLED表示。
;;          ※Starter Kit ボードの配線上、可変抵抗RP1との合算値になる
;;
(require "./demo-base")

(define code '(
			 loop
			   (a2d)
			   ;; デフォルトは RA0 (=AN0) ; = #7
			   ;; これは可変抵抗RP1との合算なので、可変抵抗で適宜調整
			   (ADDLW  #b11100111) ; W -= 11000b
			   (MOVWF  display)
			   (MOVF   display W)
			   (MOVWF  PORTC)
			   (GOTO   loop)
			   ))

(macro-asm-test '() code #t #t); #t 60)
