;;
;; demo14
;;
(require "./demo-base")

(define code '(
;			   (set! u 1)
;			   (display u)

;			   (set! v 2)
;			   (display v)

			   (set! x 0);(cons u v))
;			   (display x)
; 1010(i1) 8E 8E 8E
; 8 4 4 4E 4E
; 8 (4), 4 (0), 0 (4E),  4E <0>  2 2 2
;			   (set! y (list 1 2 3 4 5 6 7))
			   (set! y (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
;			   (set! y (list 1 2))
;			   (display 15)
;			   (display y)
;			   (display 15)
			   (set! z (length y))
;			   (display 9)
			   (display z)
			   (DEBUG:reg z)
			   (display z)
			   (DEBUG:w)
			   (display z)
			   (DEBUG:stat)
			   (display z)
;			   (display z)
;			   (display z)
;			   (display z)
;			   (display 9)
			   ))

(define code2 '(
				(set! x 15)
				(display x) ;- 0010
				(set! y (int16-upper x))
				(display y) ;- 0000
				(set! z (int16-lower x))
				(display z) ;- 1111
;				(assign x 15)

;				(MOVLW 3)
;				(display)
;				(display 1)
;				(display x)

;				(display)
;				(display 2)
;				(display x)

;				(display)
;				(display 4)
;				(display x)

;				(display)
;				(CALL  display-8bit-value)
;				(CALL  display-8bit-value)
;				(CALL  display-8bit-value)
				))

;(define *disable-writing* #t)
(macro-asm-test '(n u v x y z)
				code #t #t #t 10)

;4E(nil) 12(i2) 1A(i3) 22(i4) 0C(cons3) 8E(undef)
;4E 12 1A 22 0C #F# 80  80  80  80  80  80  80...
;rE 12 1A 22 0C E 8E
;  22 0C(cons3), A3 0E(false), 8E 
;   1A 22(i4) 0C(cons3) 0C A3 0E 8E
;10100011 
;
;4E
;
;
; 4E(nil) 12(i2) 1A(i3) 22(i4)
; 0C(cons3) 0C A3 0E 8E
; C C C(cons3) 2E FE FE  E 8F  0
; C  C  FE 8F 0
;C C 8 4  4E 91
; C C 8 4  4E 91
; 2A = 28/4 = 14/2 = 10
