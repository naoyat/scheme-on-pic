(require "./scm-object")

(define scmlib-registers
  '(;flag
	top-of-stack
	int16-addr
	pair-addr
	fsr-keeper
	w-keeper
;;	arg ; 単一引数
;;	argl ; 引数リスト
	env ; 環境
	continue
	val
	t1 ; dep, hi
	t2 ; pos, lo
	))

(define scmlib-initcode
  '((mov    top-of-stack #x37) ; 38hから始めたい
	(mov    pair-addr    #x1F) ; 20hから始めたい
	(mov    int16-addr   #x4F) ; 50hから
	))

(define scmlib-subroutines
  `(
	;;
	;; save-w  // w-keeper, fsr-keeper, top-of-stack
	;;         // FSRは保持される
	;;
   save-w
	 (mov    w-keeper w)            ; Wの値を保存しておく; temp = w
	 (mov    fsr-keeper FSR)
;;	 save-check
;;   (top-of-stack + 1) が [0x20 .. 0x6F] に入らなければ skip
	 (mov    w #x6F)              ; w = top-of-stack - 0x6F
	 (SUBWF  top-of-stack w)      ; top-of-stack >= 0x6F ならBORROWなし (C=1)
	 (BTFSC  STATUS C)            ; そうだったらsave-skipへ
	 (GOTO   save-skip)

	 (mov    w #x1f)              ; w = top-of-stack - 0x1F
	 (SUBWF  top-of-stack W)      ; top-of-stack >= 0x1F ならBORROWなし (C=1)
	 (BTFSS  STATUS C)            ; そうでなければsave-skipへ
	 (GOTO   save-skip)
;;	 save-do
	 (INCF   top-of-stack F)      ; FSR = ++top-of-stack
	 (mov    FSR top-of-stack)
	 (mov    INDF w-keeper)       ; [FSR] = w = temp (= initial w)
   save-skip
   ;;(INCF   top-of-stack F)      ; top-of-stack ++ anyway
     (mov    FSR fsr-keeper)      ; FSRの値を save-w 呼出し前の値に戻す
	 (mov    w w-keeper)          ; w = temp (= initial w)
	 (RETURN)
;	 save-skip2
;	 (display 15)
;	 (display 15)
;	 (GOTO   save-skip)
	 
	 ;;
	 ;; restore-w  // fsr-keeper, top-of-stack, w-keeper
	 ;;            // FSRは保持される
	 ;;
   restore-w
	 ;;(DECF   top-of-stack F)      ; top-of-stack --
	 (mov    fsr-keeper FSR)      ; fsr-keeper = (w =) FSR
;;	 restore-check
;;   if (top-of-stack < 0x1F || 0x6F <= top-of-stack) goto :save-skip
;;   top-of-stack が [0x20 .. 0x6F] に入らなければ skip
	 (mov    w #x70)              ; w = top-of-stack - 0x70
	 (SUBWF  top-of-stack W)      ; top-of-stack >= 0x70 ならBORROWなし (C=1)
	 (BTFSC  STATUS C)            ; そうだったらrestoreee-skipへ
	 (GOTO   restore-skip)
	 (mov    w #x20)                ; w = top-of-stack - 0x20
	 (SUBWF  top-of-stack W)      ; top-of-stack >= 0x1F ならBORROWなし (C=1)
	 (BTFSS  STATUS C)            ; そうでなければrestore-skipへ
	 (GOTO   restore-skip)
;;	 restore-do
	 (mov    FSR top-of-stack)    ; FSR = w = top-of-stack
	 (mov    w-keeper INDF)       ; temp = w = [FSR]
	 (mov    FSR fsr-keeper)      ; FSRの値を save-w 呼出し前の値に戻す
	 (mov    w w-keeper)
	 (DECF   top-of-stack F)      ; top-of-stack --
   restore-skip
     (RETURN)


	 ;;
	 ;; int16   // temp, int16-addr
	 ;;
   int16
	 ;; ScmInt16 ([pop] + [pop]*256) を１つアロケートしてWに返す
	 ;; (0x100 + ofs, 0x80 + ofs), ofs = 0 - 79
	 ;; 79まで取れるけど63までしか使わない
	 ;; ... はやくGC作りたし
	 ;; (-- skip if ++int-addr >= 0x70) ; 50 - 6f
	 (INCF   int16-addr F)

	 ;; w=lower [upper]
	 (push)
	 ;; [lower upper]

	 ;;(when (<= #x70 int-addr) (GOTO ,fail-label))
	 (MOVF   int16-addr w)
	 (SUBLW  #x70)
	 (BTFSS  STATUS C) ; pair-addr >= 0x60のときborrow=1 (C=0)
	 (GOTO   int16-fail)

	 (mov    FSR int16-addr)

	 (-- set lower)
	 (pop)
	 (change-irp 0 2)
	 (mov    INDF w)
	 (change-irp 2 0)
	 
	 (-- set upper)
	 (BSF    FSR  7)
	 (pop)
;	 (change-irp 0 1)
	 (mov    INDF w)
;	 (change-irp 1 0)
	 
	 ;; w = #b010 | (int16-addr - 0x50) << 3 
	 (mov    FSR int16-addr) ; FSR (as temp) = w = int16-addr

	 (mov    w #x50)		; w = 0x50
	 (SUBWF  FSR F) 	    ; temp -= 0x50
	 (BCF    STATUS C)		; STATUS[C] = 0
	 (RLF    FSR F) 	    ; w = temp << 3
	 (RLF    FSR F)         ;
	 (RLF    FSR W)
	 (ADDLW  #b010)
	 (RETURN)
	 ;; w = #<undef> if failed
   int16-fail
	 (pop)
	 (pop)
	 (RETLW #b10001110) ;;,scm-undefined

	 ;;
	 ;; int16p
	 ;;
   int16p
	 (ANDLW  #b00000111)
	 (XORLW  #b00000010)
	 (BTFSC  STATUS Z)
	 (RETLW  ,scm-true) ; Z=1 (int16)
	 (RETLW  ,scm-false) ; Z=0

	 ;;
	 ;; int16-lower  // FSR
	 ;;
   int16-lower
;	 (LED/display-w)
	 ;; checking if int16
	 (mov    FSR w)  ;; FSR (as temp)
	 (BTFSC  FSR 0)
	 (GOTO   int16-lower-skip)
	 (BTFSS  FSR 1)
	 (GOTO   int16-lower-skip)
	 (BTFSC  FSR 2)
	 (GOTO   int16-lower-skip)

	 ;; FSR (as FSR) = #x150 + (w - #b010) >> 3)
	 (mov    FSR w)		; FSR = w
	 (BCF    STATUS C)	; STATUS[C] = 0
	 (BCF    FSR 1)     ; FSR -= #b010
	 (RRF    FSR F) 	; FSR >>= 3
	 (RRF    FSR F)
	 (RRF    FSR F)
	 (mov    w #x50)	; w = 0x50
	 (ADDWF  FSR F)	; FSR += w
;	 (LED/display-reg FSR)
	 ;; w = *FSR
	 (change-irp 0 2)
	 (mov    w INDF)	; w = [INDF]
	 (change-irp 2 0)
	 (RETURN)

	int16-lower-skip
	 ;; *** ERROR: pair required, but got ????
	 (RETURN)

	 ;;
	 ;; int16-upper  // FSR
	 ;;
   int16-upper
;	 (LED/display-w)
	 ;; checking if pair
	 (mov    FSR w)  ; FSR (as temp)
	 (BTFSC  FSR 0)
	 (GOTO   int16-upper-skip)
	 (BTFSS  FSR 1)
	 (GOTO   int16-upper-skip)
	 (BTFSC  FSR 2)
	 (GOTO   int16-upper-skip)

	 ;; FSR (as FSR) = #x0D0 + (w - #b010) >> 3)
	 (mov    FSR w)		; FSR = w
	 (BCF    STATUS C)	; STATUS[C] = 0
	 (BCF    FSR 1)     ; FSR -= #b010
	 (RRF    FSR F) 	; FSR >>= 3
	 (RRF    FSR F)
	 (RRF    FSR F)
	 (mov    W #xd0)	; w = 0xD0
	 (ADDWF  FSR F)	; FSR += w
;	 (LED/display-reg FSR)
	 ;; w = *FSR
;	 (change-irp 0 1)
	 (mov    w INDF)	; w = [INDF]
;	 (change-irp 1 0)
	 (RETURN)

	int16-upper-skip
	 ;; *** ERROR: pair required, but got ????
	 (RETURN)

	 ;;
	 ;; int16-add  // t1, t2
	 ;;
   int16-add
	 (push) ; [x y]
;	 (DEBUG:scm:stack)
	 (int16-lower) ; w = x.lo
	 (mov    t2 w) ; t2 = w = x.lo
	 (pop) ; x [y]

	 (push) ; [x y]
	 (int16-upper) ; w = x.hi
	 (mov    t1 w) ; t1 = w = x.hi
	 (pop) ;x [y]

	 (pop) ; y []

	 (push) ; [y]
	 (int16-lower) ; w = y.lo
	 (ADDWF  t2 F) ; t2 += w
	 (BTFSS  STATUS C)
	 (GOTO   int16-add-nocarry)

	 (INCF   t1 F) ; t1 += carry

    int16-add-nocarry
	 (pop) ; y []
	 (push) ; [y]
	 (int16-upper) ; w = y.hi
	 (ADDWF  t1 F) ; t1 += w
	 (pop) ; []

	 (push   t1) ; [t1]
	 (mov    w t2)
	 (CALL   int16)
	 (RETURN)

	 ;;
	 ;; int16-sub  // t1, t2
	 ;;
   int16-sub
   ;;x [y]
     (mov t1 w) ; t1 = x
	 (pop) ; []
	 (mov t2 w) ; t2 = y
	 (mov w t1)
	 (push) ; [x]
	 (mov w t2)
	 (push) ; [y x] ;; 求めたいのは y - x

;	 (push) ; [x y] ;; 求めたいのは y - x
	 (int16-lower) ; w = x.lo
	 (mov    t2 w) ; t2 = w = y.lo
	 (pop) ; x [y]

	 (push) ; [x y]
	 (int16-upper) ; w = x.hi
	 (mov    t1 w) ; t1 = w = y.hi
	 (pop) ;x [y]

	 (pop) ; y []

	 (push) ; [y]
	 (int16-lower) ; w = x.lo
	 (SUBWF  t2 F) ; t2 -= w
	 (BTFSC  STATUS C)
	 (GOTO   int16-sub-noborrow)

	 (DECF   t1 F) ; t1 -= borrow

    int16-sub-noborrow
	 (pop) ; y []
	 (push) ; [y]
	 (int16-upper) ; w = x.hi
	 (SUBWF  t1 F) ; t1 -= w
	 (pop) ; []

	 (push   t1) ; [t1]
	 (mov    w t2)
	 (CALL   int16)
	 (RETURN)

	 ;;
	 ;; int16-incr  // t1, t2
	 ;;
   int16-incr
     ;;x
	 (push)
	 (int16-lower)
	 (mov    t2 w) ; t2 = w = x.lo
	 (pop) ; x
	 (push)
	 (int16-upper)
	 (mov    t1 w) ; t1 = w = x.hi
	 (pop)
	 ;; t2--
	 (INCF   t2 F)
	 (BTFSC  STATUS C) ; skip if unset (ie. carry=0)
	 (INCF   t1 F) ; t1 += carry
	 (push   t1)
	 (mov    w t2)
	 (CALL   int16)
	 (RETURN)

	 ;;
	 ;; int16-decr  // t1, t2
	 ;;
   int16-decr
     ;;x
	 (push)
	 (int16-lower)
	 (mov    t2 w) ; t2 = w = x.lo
	 (pop) ; x
	 (push)
	 (int16-upper)
	 (mov    t1 w) ; t1 = w = x.hi
	 (pop)
	 ;; t2--
	 (DECF   t2 F)
	 (BTFSS  STATUS C) ; skip if set (ie. borrow=0)
	 (DECF   t1 F) ; t1 -= borrow

	 (push   t1)
	 (mov    w t2)
	 (CALL   int16)
	 (RETURN)

	 ;;
	 ;; int16-eq
	 ;;
   int16-eq
	 (push) ; [x y]
	 (int16-lower) ; w = x.lo
	 (mov    t2 w) ; t2 = w = x.lo
	 (pop) ; x [y]

	 (push) ; [x y]
	 (int16-upper) ; w = x.hi
	 (mov    t1 w) ; t1 = w = x.hi
	 (pop) ;x [y]

	 (pop) ; y []

	 (push) ; [y]
	 (int16-lower) ; w = y.lo
	 (XORWF  t2 W)
	 (BTFSS  STATUS Z)
	 (GOTO   int16-eq-false)

	 (pop) ; y []
	 (push) ; [y]
	 (int16-upper) ; w = y.hi
	 (XORWF  t1 W)
	 (BTFSS  STATUS Z)
	 (GOTO   int16-eq-false)

	 (pop)
	 (RETLW  ,scm-true)
   int16-eq-false
	 (pop)
	 (RETLW  ,scm-false)

	 ;;
	 ;; int16-zerop
	 ;;
   int16-zerop
	 (push) ; [x]
	 (int16-lower) ; w = x.lo
	 (XORLW  0)
	 (BTFSS  STATUS Z)
	 (GOTO   int16-zerop-false)
	 (pop) ; []
	 (push) ; [x]
	 (int16-upper) ; w = x.hi
	 (XORLW  0)
	 (BTFSS  STATUS Z)
	 (GOTO   int16-zerop-false)
	 (pop)
	 (RETLW  ,scm-true)
   int16-zerop-false
	 (pop)
	 (RETLW  ,scm-false)

	 ;;
	 ;; int16-onep
	 ;;
   int16-onep
	 (push) ; [x]
	 (int16-lower) ; w = x.lo
	 (XORLW  1)
	 (BTFSS  STATUS Z)
	 (GOTO   int16-onep-false)
	 (pop) ; []
	 (push) ; [x]
	 (int16-upper) ; w = x.hi
	 (XORLW  0)
	 (BTFSS  STATUS Z)
	 (GOTO   int16-onep-false)
	 (pop)
	 (RETLW  ,scm-true)
   int16-onep-false
	 (pop)
	 (RETLW  ,scm-false)

	 ;;
	 ;; cons  // pair-addr, FSR (as temp)
	 ;;
   cons
	 ;; ScmPair ([pop],w) を１つアロケートしてWに返す
	 ;; (0x100 + ofs, 0x80 + ofs), ofs = 0 - 79
	 ;; 79まで取れるけど63までしか使わない
	 ;; ... はやくGC作りたし
	 ;; (-- skip if ++pair-addr >= 0x60) ; 20 - 4f

	 (INCF   pair-addr F)

	 ;; w=cdr [car]
	 (push) ;; Wに入っていたのはCDR
	 ;; [cdr car]

	 ;;(when (<= #x50 pair-addr) (GOTO ,fail-label))
	 (mov    w pair-addr)
	 (SUBLW  #x50)
	 (BTFSS  STATUS C) ; pair-addr >= 0x50のときborrow=1 (C=0)
	 (GOTO   cons-fail)

	 ; FSR = (w =) pair-addr
	 (mov    FSR pair-addr)

	 ; [0x100 + FSR] = (w =) pop
	 (-- set cdr)
	 (pop)
	 (change-irp 0 2)
	 (mov    INDF w)
	 (change-irp 2 0)
	 
	 ; [0x80 + FSR] = (w =) pop
	 (-- set car)
	 (BSF    FSR  7)
	 (pop)
;	 (BCF    FSR  7)
;	 (change-irp 0 1)
	 (MOVWF  INDF)
;	 (change-irp 1 0)
	 
	 ;; w = (pair-addr - 0x20) << 2
	 (mov    FSR pair-addr) ; FSR (as temp) = w = pair-addr

	 (mov    w #x20)		; w = 0x20
	 (SUBWF  FSR F)	    ; temp -= 0x20
	 (BCF    STATUS C)		; STATUS[C] = 0
	 (RLF    FSR F)	    ; w = temp << 2
	 (RLF    FSR W)
	 (RETURN)
	 ;; w = #<undef> if failed
    cons-fail
	 (pop)
	 (pop)
	 (RETLW  ,scm-undefined)

	 ;;
	 ;; pairp
	 ;;
   pairp
	 (ANDLW  #b00000011)
	 (BTFSC  STATUS Z)
	 (RETLW  ,scm-true) ; Z=1 (pair)
	 (RETLW  ,scm-false) ; Z=0

	 ;;
	 ;; nullp
	 ;;
   nullp
	 (XORLW  ,scm-nil)
	 (BTFSC  STATUS Z)
	 (RETLW  ,scm-true) ; Z=1 (w = scm-nil)
	 (RETLW  ,scm-false) ; Z=0

	 ;;
	 ;; car  // FSR
	 ;;
   car
	 ;; checking if pair
	 (mov    FSR w)      ; using FSR for temp
	 (BTFSC  FSR 0)
	 (GOTO   car-skip)
	 (BTFSC  FSR 1)
	 (GOTO   car-skip)

	 ;; FSR = #xA0 + (w >> 2)
	 (mov    FSR w)		; FSR = w
	 (BCF    STATUS C)	; STATUS[C] = 0
	 (RRF    FSR F) 	; FSR >>= 2
	 (RRF    FSR F)
	 (mov    w #xa0)	; w = 0xA0
	 (ADDWF  FSR F) 	; FSR += w

	 ;; w = *FSR
;	 (BSF    FSR  7)
;	 (change-irp 0 1)
	 (mov    w INDF)	; w = [INDF]
;	 (change-irp 1 0)
	 (RETURN)

    car-skip
	 ;; *** ERROR: pair required, but got ????
	 (RETURN)

	 ;;
	 ;; cdr  // FSR
	 ;;
   cdr
	 ;; checking if pair
	 (mov    FSR w)    ; using FSR for temp
	 (BTFSC  FSR 0)
	 (GOTO   cdr-skip)
	 (BTFSC  FSR 1)
	 (GOTO   cdr-skip)

	 ;; FSR = #x120 + (w >> 2)
	 (mov    FSR w)		; FSR = w
	 (BCF    STATUS C)	; STATUS[C] = 0
	 (RRF    FSR F) 	; FSR >>= 2
	 (RRF    FSR F)
	 (mov    w #x20)	; w = 0x20
	 (ADDWF  FSR  F)	; FSR += w
	 ;; w = *FSR
	 (change-irp 0 2)
	 (mov    w INDF)	; w = [INDF]
	 (change-irp 2 0)
	 (RETURN)

    cdr-skip
	 ;; *** ERROR: pair required, but got ????
	 (RETURN)

	 ;;
	 ;; set-car  // FSR
	 ;;
   set-car
	 ;; checking if pair
;     (DEBUG:scm:pairs)
	 (mov    FSR w)      ; using FSR for temp
;     (DEBUG:file FSR)
	 (BTFSC  FSR 0)
	 (GOTO   set-car-skip)
	 (BTFSC  FSR 1)
	 (GOTO   set-car-skip)

	 ;; FSR = #xA0 + (w >> 2)
	 (mov    FSR w)		; FSR = w
	 (BCF    STATUS C)	; STATUS[C] = 0
	 (RRF    FSR F) 	; FSR >>= 2
	 (RRF    FSR F)
	 (mov    w #xa0)	; w = 0xA0
	 (ADDWF  FSR F) 	; FSR += w

	 ;; w = *FSR
;	 (BSF    FSR  7)
;	 (change-irp 0 1)
	 (pop)
	 (mov    INDF w)    ; [INDF] = w = (pop)
;	 (mov    w INDF)	; w = [INDF]
;	 (change-irp 1 0)
	 (RETURN)

    set-car-skip
	 ;; *** ERROR: pair required, but got ????
	 (RETURN)


	 ;;
	 ;; set-cdr  // FSR
	 ;;
   set-cdr
	 ;; checking if pair
	 (mov    FSR w)    ; using FSR for temp
	 (BTFSC  FSR 0)
	 (GOTO   set-cdr-skip)
	 (BTFSC  FSR 1)
	 (GOTO   set-cdr-skip)

	 ;; FSR = #x120 + (w >> 2)
	 (mov    FSR w)		; FSR = w
	 (BCF    STATUS C)	; STATUS[C] = 0
	 (RRF    FSR F) 	; FSR >>= 2
	 (RRF    FSR F)
	 (mov    w #x20)	; w = 0x20
	 (ADDWF  FSR  F)	; FSR += w
	 ;; w = *FSR
	 (change-irp 0 2)
	 (pop)
	 (mov     INDF w)   ; [INDF] = w = (pop)
	 ;(mov    w INDF)	; w = [INDF]
	 (change-irp 2 0)
	 (RETURN)

    set-cdr-skip
	 ;; *** ERROR: pair required, but got ????
	 (RETURN)

	 ;;
	 ;; length
	 ;;
   length
	 (INCF   top-of-stack F) ; [>??]
	 (push) ; [>w ??]
	 (nullp)

	 (bt     length-zero)
	 (pop) ; w< [??]
	 
	 (push) ; [>w ??]
	 (pairp)
	 (bf     length-error)
			   
	 ;; [top-of-stack-1] = 0
	 (mov    FSR top-of-stack)
	 (DECF   FSR F)
	 (CLRF   INDF) ; [w 0]

	 length-loop
	 ;; [top-of-stack-1]++
	 (mov    FSR top-of-stack)
	 (DECF   FSR F)
	 (INCF   INDF F) ; [w (len+1)

	 (pop) ; w< [(len+1)]
	 (cdr)
	 (push) ; [>w ??]
	 (nullp)
	 (bf     length-loop)

	 length-end
	 (pop)
	 (pop)
	 (RETURN)
			 
	 length-zero
	 (pop)
	 (pop)
	 (RETLW  0)

	 length-error
	 (pop)
	 (pop)
	 (RETLW  ,scm-undefined)


	 ;;
	 ;; lrefp
	 ;;
   lrefp
	 (ANDLW  #b00000011)
	 (XORLW  #b00000011)
	 (BTFSC  STATUS Z)
	 (RETLW  ,scm-true) ; Z=1 (lref)
	 (RETLW  ,scm-false) ; Z=0

	 ;;
	 ;; lsetXX
	 ;;
   lset0  ;(caar env) = w
;     (asm (DEBUG:snapshot))
     (push)
	 (mov    w env)
	 (car)
	 (set-car)
	 (RETURN)
   lset1  ;(cadar env) = w
     (push)
	 (mov    w env)
	 (cdar)
	 (set-car)
	 (RETURN)
   lset2  ;(caddar env) = w
     (push)
	 (mov    w env)
	 (cddar)
	 (set-car)
	 (RETURN)
   lset3  ;(cadddar env) = w
     (push)
	 (mov    w env)
	 (cdddar)
	 (set-car)
	 (RETURN)
   lset10  ; (caadr env) = w
     (push)
	 (mov    w env)
	 (cadr)
	 (set-car)
	 (RETURN)
   lset11  ; (cadadr env) = w
     (push)
	 (mov    w env)
	 (cdadr)
	 (set-car)
	 (RETURN)
   lset12  ; (caddadr env) = w
     (push)
	 (mov    w env)
	 (cddadr)
	 (set-car)
	 (RETURN)
   lset20  ; (caaddr env) = w
     (push)
	 (mov    w env)
	 (caddr)
	 (set-car)
	 (RETURN)
   lset21  ; (cadaddr env) = w
     (push)
	 (mov    w env)
	 (cdaddr)
	 (set-car)
	 (RETURN)
   lset30  ; (caadddr env) = w
     (push)
	 (mov    w env)
	 (cadddr)
	 (set-car)
	 (RETURN)

	 ;;
	 ;; lrefXX
	 ;;
   lref0  ; (caar env)
	 (mov    w env)
	 (car)
	 (car)
	 (RETURN)
   lref1  ; (cadar env)
	 (mov    w env)
	 (car)
	 (cadr)
	 (RETURN)
   lref2  ; (caddar env)
	 (mov    w env)
	 (car)
	 (caddr)
	 (RETURN)
   lref3  ; (cadddar env)
	 (mov    w env)
	 (car)
	 (cadddr)
	 (RETURN)
   lref10  ; (caadr env)
	 (mov    w env)
	 (cadr)
	 (car)
	 (RETURN)
   lref11  ; (cadadr env)
	 (mov    w env)
	 (cadr)
	 (cadr)
	 (RETURN)
   lref12  ; (caddadr env)
	 (mov    w env)
	 (cadr)
	 (caddr)
	 (RETURN)
;   lref13  ; (cadddadr env)
;	 (mov    w env)
;	 (cadr)
;	 (cadddr)
;	 (RETURN)
   lref20  ; (caaddr env)
	 (mov    w env)
	 (caddr)
	 (car)
	 (RETURN)
   lref21  ; (cadaddr env)
	 (mov    w env)
	 (caddr)
	 (cadr)
	 (RETURN)
;   lref22  ; (caddaddr env)
;	 (mov    w env)
;	 (caddr)
;	 (caddr)
;	 (RETURN)
;   lref23  ; (cadddaddr env)
;	 (mov    w env)
;	 (caddr)
;	 (cadddr)
;	 (RETURN)
   lref30  ; (caadddr env)
	 (mov    w env)
	 (cadddr)
	 (car)
	 (RETURN)
;   lref31  ; (cadadddr env)
;	 (mov    w env)
;	 (cadddr)
;	 (cadr)
;	 (RETURN)
;   lrefXX-fail
;	 (pop)
;	 (RETLW  ,scm-undefined)

	 ;;
	 ;; eval  // t1 t2 temp env
	 ;;
   eval
     (push)
	 ; miscp
	 (ANDLW  #b00011111)
	 (XORLW     #b01110)
	 (BTFSC  STATUS Z)
	 (GOTO   eval-misc) ; Z=1
	 (pop)

	 (push)
	 (pairp)
	 (bt     eval-pair)
	 (pop)

	 (push)
	 (int16p)
	 (bt     eval-int16)
	 (pop)

	 (push)
	 (lrefp)
	 (bt     eval-lref)

	 (pop)
	 (RETURN)

   eval-misc
   eval-int16
     ;; -#f, #t, '(), #<eof-object>, #<undefined>, #<unbound>
     ;; -16ビット整数
     ;; →そのまま返す
	 (pop)
	 (RETURN)

   eval-pair
     ;; pair → applyして返すべき
	 (pop)
	 (RETURN)

	 ;; // t1 t2 [env]
   eval-lref
     ;; リファレンス: 現在のenvレジスタから参照
	 (pop)

	 (mov     FSR w)  	 ; FSR (as temp) = dddppp11
	 (RRF     FSR F)	 ;          temp = -dddppp1
	 (RRF     FSR W)	 ;             w = 1-dddppp
	 (ANDLW   #b0111)	 ;             w = 00000ppp
	 (mov     t2 w)	     ;       t2<pos> = 00000ppp
	 (SWAPF   FSR W)	 ;             w = ppp1-ddd
	 (ANDLW   #b0111)	 ;             w = 00000ddd
	 (mov     t1 w)      ;       t1<dep> = 00000ddd

	 (push    env)

     ;;(MOVF env W)
;	 (MOVF    env W)
;	 (MOVWF   temp)
    depth-loop
     ;; if (dep == 0) break
     (MOVF    t1 F)
	 (BTFSC   STATUS Z)
	 (GOTO    depth-loop-end) ; Z=1, ie.dep=0
	 ;; temp = (cdr temp)
	 (mov     w env)
	 (cdr)
	 (mov     env w)
	 ;; dep--
	 (DECF    t1 F)
	 (GOTO    depth-loop)
    depth-loop-end

     ;; env = (car env)
	 (mov     w env)
	 (car)
	 (mov     env w)
    pos-loop
     ;; if (pos == 0) break
     (MOVF    t2 F)
	 (BTFSC   STATUS Z)
	 (GOTO    pos-loop-end)
	 ;; env = (cdr env)
	 (mov     w env)
	 (cdr)
	 (mov     env w)
	 ;; pos--
	 (DECF    t2 F)
	 (GOTO    pos-loop)
    pos-loop-end
	 ;; w = (car temp)
     (mov     w env)
	 (car)
;	 (MOVWF   temp)
	 
	 (mov     t1 w)  ; using t1 for temp
	 (pop     env)
	 (mov     w t1)

;	 (MOVLW   ,scm-undefined)
	 (RETURN)

	 ;;
	 ;; print (w)
	 ;;
   print
;     (DEBUG:pc)
;     (DEBUG:scm:stack)
;    (push)
	 (DEBUG:scm:w-pp) ;; for debug
;     (DEBUG:scm:stack)
;	 (DEBUG:if-debug)
	 (RETURN)
;	 (GOTO print-in-led)
    print-in-led
	 (int16-upper)
	 (CALL   display-8bit-value)
     (pop)
	 (int16-lower)
	 (CALL   display-8bit-value)
	 (mov    w t1)
	 (RETURN)

	 ))
