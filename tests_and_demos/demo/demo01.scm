;;
;; demo01 - LEDを光らせる
;;
(require "./demo-base")

(define code '(
			 loop
			   (LED #b0000 0.1) ; LEDをパターン0000で0.1秒間点灯する
			   (LED #b0001 0.1)
			   (LED #b0011 0.1)
			   (LED #b0010 0.1)
			   (LED #b0110 0.1)
			   (LED #b0100 0.1)
			   (LED #b1100 0.1)
			   (LED #b1000 0.1)
			   (LED #b0000 0.1)
			   (LED #b1000 0.1)
			   (LED #b1100 0.1)
			   (LED #b0100 0.1)
			   (LED #b0110 0.1)
			   (LED #b0010 0.1)
			   (LED #b0011 0.1)
			   (LED #b0001 0.1)
			   (LED #b0000 0.1)

			   (GOTO loop)
			   ))

(macro-asm-test '() code #t #t #t 10)
