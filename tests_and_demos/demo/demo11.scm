;;
;; demo11 scm-object
;;
(require "./demo-base")

(define code000 '(
				  (assign x 4)
				  (save x)
				  (restore z)

;				  (assign PORTC z)
				  (-- --- result ---)
				  (LED/display-reg z)
				  (LED/display-reg z)
				  (LED/display-reg z)
				  ;; ok
				  ))

(define code001 '(
				  (assign x 2)
				  (assign y 3)
				  (assign z (if #t x y))
				  ;; now z must be x (= 2)

				  ;; 000010 01
				  (LED/display-reg x) ; 0000 1001
				  ;; ok
				  ))

(define code002 '(
				  (assign x 2)
				  (assign y 3)
				  (assign z (if #t x y))
				  ;; now z must be x (= 3)

				  ;; 000011 01
				  (LED/display-reg x) ; 0000 1101
				  ;; ok
				  ))

(define code003 '(
				  (assign x #f)
				  (assign y #t)
				  (assign z (if #t x y))
				  ;; now z must be x (= #f)

				  ;; 000 01110
				  (LED/display-reg z) ; 0000 1110
				  ;; ok
				  ))

(define code004 '(
				  (assign x #f)
				  (assign y #t)
				  (assign z (if #f x y))
				  ;; now z must be y (= #t)

				  ;; 001 01110
				  (LED/display-reg z) ; 0010 1110
				  ;; ok
				  ))

(define code005 '(
				  (assign x (if #f 1 3))
				  ;; now x must be 3

				  ;; 000011 01
				  (LED/display-reg x) ; 0000 1101
				  ;; ok
				  ))

(define code006 '(
				  (assign x (if #f 0))
				  ;; now x must be #<undef>
				  
				  ;; 100 01110
				  (LED/display-reg x) ; 1000 1110
				  ;; ok
				  ))

(define code007 '(
				  (LED/display-literal 5)
				  (LED/display-literal 6)
				  (LED/display-literal 7)

				  ;; ok
				  ))

(define code008 '(
				  (assign x 0)

				  (MOVLW #x50)
				  (MOVWF FSR)

				  (MOVLW 6)
				  (MOVWF INDF)

				  (LED/display-reg FSR) ; expects 50

				  (LED/display-reg INDF) ; expects 6
				  
				  (MOVF FSR W)
				  (LED/display-w) ; expects 50

				  (MOVF INDF W)
				  (LED/display-w) ; expects 6

				  ;; ok
				  ))

(define code009 '(
				  (MOVLW 5)
				  (LED/display-w) ; expects 5
				  (save-w)
				  (LED/display-w) ; expects 5

				  (MOVLW 6)
				  (LED/display-w) ; expects 6
				  (restore-w)
				  (LED/display-w) ; expects 5

				  ;; ok
				  ))

(define code010 '(
				  (MOVLW #x1F)
				  (CALL  testsub)
				  (MOVLW #x20)
				  (CALL  testsub)
				  (MOVLW #x6F)
				  (CALL  testsub)
				  (MOVLW #x70)
				  (CALL  testsub)
				  (GOTO  $)

				  (label  testsub)
				  (MOVWF  v)

				  (MOVLW  #x70)                 ; w = top-of-stack - 0x70
				  (SUBWF  v W)      ; top-of-stack >= 0x70 ならBORROWなし (C=1)
				  (BTFSC  STATUS C)            ; C=1 の場合(w<0)にsave-skipへ
				  (GOTO   test-fail)

				  (MOVLW  #x20)                 ; w = top-of-stack - 0x20
				  (SUBWF  v W)      ; top-of-stack >= 0x20 ならBORROWなし (C=1)
				  (BTFSS  STATUS C)            ; C=0 の場合(w<0)にsave-skipへ
				  (GOTO   test-fail)

				  (LED/display-literal #b1001)
				  (RETURN)

				  (label test-fail)
				  (LED/display-literal #b0110)
				  (RETURN)

				  ;; ok
				  ))

(define code011 '(
				  (assign v (cons 3 4))
				  (assign x (cons 1 2))
				  (assign y (car x))
				  (assign z (cdr x))

				  (-- --- result ---)
				  (LED/display-reg x) ; 000001 00 ; ---- -*--

				  (LED/display-reg y) ; 000001 01 ; ---- -*-*
;
				  (LED/display-reg z) ; 000010 01 ; ---- *--*
;0000 01 00 ; ok
;1101 11 00 ; ng
;0111 11 11 ; ng
				  ))

(define code012 '(
				  (assign x 100)
;				  (LED/display-reg x) ; 00000 010 ; ---- --*-
				  (assign y 1000)
;				  (LED/display-reg y) ; 00001 010 ; ---- *-*-
				  (assign z 10000)
;				  (LED/display-reg z) ; 00010 010 ; ---* --*-

				  (-- --- result ---)
				  (assign u (int16-upper x))
				  (LED/display-reg u) ; 0  = ---- ----
				  (assign v (int16-lower x))
				  (LED/display-reg v) ; 64 = -**- -*--

				  (assign u (int16-upper y))
				  (LED/display-reg u) ; 3  = ---- --**
				  (assign v (int16-lower y))
				  (LED/display-reg v) ; e8 = ***- *---

				  (assign u (int16-upper z))
				  (LED/display-reg u) ; 27 = --*- -***
				  (assign v (int16-lower z))
				  (LED/display-reg v) ; 10 = ---* ----

				  ;ok
				  ))

(define code020 '(
				  (assign x #x33)

;			  (LED/display-literal 4)

				  (goto (reg x))

				  (label one)
				  (LED/display-literal 1)
				  (GOTO $)

				  (asm (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP))
;				  (asm (ORG 100))

				  (label nine)
				  (LED/display-literal 9)
				  (GOTO $)
				  ))

;;(assign *disable-writing* #t)
(macro-asm-test '(u v x y z)
				  code020 #t #t #t 5)
; 1111 0011
; 1111 1001