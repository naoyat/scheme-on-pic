;;
;; demo14
;;
(require "./demo-base")

(define code '(
			   (MOVLW #x13)
			   (MOVWF FSR)
			   (MOVLW #x99)
			   (COMF FSR 1)
			   (display) ; w = 99 ? 13 ? --> 99

			   (MOVLW #x13)
			   (MOVWF FSR)
			   (MOVLW #xAA)
			   (DECF FSR 1)
			   (display) ; w = AA ? 13 ? 12 ? --> AA

			   (MOVLW #x20)
			   (MOVWF FSR)
			   (MOVLW #xBB)
			   (RLF FSR 1)
			   (display) ; w = BB ? 20 ? 40 ? --> BB

			   (MOVLW #x20)
			   (MOVWF FSR)
			   (MOVLW #xCC)
			   (RRF FSR 1)
			   (display) ; w = CC ? 20 ? 10 ? --> CC

			   (MOVLW #xA5)
			   (MOVWF FSR)
			   (MOVLW #xDD)
			   (SWAPF FSR 1)
			   (display) ; w = DD ? A5 ? 5A ? --> DD

			   (MOVLW #x01)
			   (SUBLW #x10)
			   (MOVF STATUS W)
			   (display) ; -> 19 (00011001) (Cのみ立つ)
			   ))

(define code2 '(
;			   (display 10)
;			   (wait 2.0)
			   (wait 2.0)
;			   (display 10)
			   ))

;(define *disable-writing* #t)
(macro-asm-test '(x) ;nx u v x y z)
				code2 #t #t #t 10)
