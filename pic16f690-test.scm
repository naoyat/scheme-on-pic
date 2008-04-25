(require "./pic16f690-emulator")

(use gauche.test)
(test-start "pic16f690")

(test-section "=== PICインスタンス生成 ===")
(define pic (make-pic16f690))
(test* "pic object" #f (not pic))

(test* "power" #f [pic'on?])
[pic'on]
(test* "power on" #t [pic'on?])
[pic'off]
(test* "power off" #f [pic'on?])

(test* "sleep? initial stat" #f [pic'sleep?])
[pic'go-to-sleep]
(test* "sleep? after 'sleep" #t [pic'sleep?])
[pic'wake-up]
(test* "sleep? after 'wake-up" #f [pic'sleep?])

(test* "halt? initial stat" #f [pic'halt?])
[pic'halt]
(test* "halt? after 'halt" #t [pic'halt?])
[pic'unhalt]
(test* "halt? after 'unhalt" #f [pic'halt?])

(test* "pc" 0 [pic'pc])

(test-section "=== プログラムをロード ===")
([pic'load-program] 0 #u16(0 0 0))
;(test* "pc doesn't move after 'load-program" 0 [pic'pc])
([pic'pc-set!] 10)
(test* "pc after pc-set!" 10 [pic'pc])
[pic'reset]
(test* "pc after reset" 0 [pic'pc])
[pic'step]
(test* "pc after 1 step" 1 [pic'pc])
[pic'step]
(test* "pc after 2 step" 2 [pic'pc])
[pic'step]
(test* "pc after 3 step" 3 [pic'pc])

(test* "halt? after program" #t [pic'halt])
[pic'step]
(test* "pc after 4 step (no program!)" 3 [pic'pc])
[pic'unhalt]
[pic'step]
(test* "pc after 4 step (stepping after unhalt)" 3 [pic'pc])
;;
;;

(define INDF 0)
(define PCL 2)
(define STATUS 3)
(define FSR 4)
(define PCLATH #x0a)
(define INTCON #x0b)

(define C 0)
(define DC 1)
(define Z 2)

;;;;;;;;;;;;;;;;
(test-section "=== レジスタ、フラグ ===")
(test* "initial w" 0 [pic'w])
([pic'w-set!] 1)
(test* "w" 1 [pic'w])

(test* "initial f[32]" 0 ([pic'f] 32))
([pic'f-set!] 32 7)
(test* "f[32]" 7 ([pic'f] 32))

;; FSR / INDF
([pic'f-set!] FSR 32)
(test* "FSR=32; INDF" 7 ([pic'f] 0))

([pic'f-set!] FSR 33)
([pic'f-set!] INDF 100)
(test* "FSR=33; INDF=100; reg<33>==100" 100 ([pic'f] 33))

;; C flag
;;  = status<0>
[pic'c-set!]
(test* "Carry=on" #t [pic'c])
(test* "STATUS<0> = 1" #t (logbit? 0 [pic'status]))
[pic'c-unset!]
(test* "Carry=off" #f [pic'c])
(test* "STATUS<0> = 0" #f (logbit? 0 [pic'status]))

;; DC flag
;;  = status<1>
[pic'dc-set!]
(test* "DC=on" #t [pic'dc])
(test* "STATUS<1> = 1" #t (logbit? 1 [pic'status]))
[pic'dc-unset!]
(test* "DC=off" #f [pic'dc])
(test* "STATUS<1> = 0" #f (logbit? 1 [pic'status]))

;; Zero flag
;;  = status<2>
[pic'z-set!]
(test* "Zero=on" #t [pic'z])
(test* "STATUS<2> = 1" #t (logbit? 2 [pic'status]))
[pic'z-unset!]
(test* "Zero=off" #f [pic'z])
(test* "STATUS<2> = 0" #f (logbit? 2 [pic'status]))

([pic'f-set!] STATUS 0)
(test* "Z=off DC=off C=off" #f (or [pic'z] [pic'dc] [pic'c]))
([pic'f-set!] STATUS 1)
(test* "Z=off DC=off C=on" #f (or [pic'z] [pic'dc] (not [pic'c])))
([pic'f-set!] STATUS 2)
(test* "Z=off DC=on C=off" #f (or [pic'z] (not [pic'dc]) [pic'c]))
([pic'f-set!] STATUS 4)
(test* "Z=on DC=off C=off" #f (or (not [pic'z]) [pic'dc] [pic'c]))

([pic'pc-set!] #xfff)
([pic'write-inst-at-pc] #x1234)
(test* "instruction at #xfff" #x1234 [pic'inst])
([pic'pc-set!] #x1fff)
(test* "instruction at #x1fff" #x1234 [pic'inst])

;(test* "pc after writing inst" 0 [pic'pc])
;(test* "mnemonic at pc" '(ADDLW 255) [pic'mnemonic])

;;;;;;;;;;;;
(test-section "=== インタプリタ機能 ===")

([pic'pc-set!] 0)
(test* "mnemonic at addr 0" '(NOP) [pic'mnemonic])

([pic'write-inst-at-pc] #x3fff)
(test* "pc after writing inst" 0 [pic'pc])
(test* "mnemonic at pc" '(ADDLW 255) [pic'mnemonic])

([pic'write-inst-at-pc] 0)
(test* "pc after writing inst" 0 [pic'pc])
(test* "mnemonic at pc" '(NOP) [pic'mnemonic])
;;;;

([pic'w-set!] 0)
(test* "w (cleared)" 0 [pic'w])
;([pic'interpret] #x0aa0) ; '(INCF <reg32>)
([pic'interpret] #x3f05) ; (ADDLW 5)
(test* "pc after 'interpret" 1 [pic'pc])
([pic'pc-set!] 0)
(test* "has the program-mem been overwritten?" '(NOP) [pic'mnemonic])
(test* "w after (ADDLW 5)" 5 [pic'w])

([pic'addlw] 3)
(test* "pc after 'addlw" 8 [pic'w])

;; prog #2
;([pic'load-program]
; 0
; #u16(#x1683 #x1007 #x1283 #x1407 #x0ba0 #x2804 #x0ba1 #x2804 #x1007 #x0ba0 #x2809 #x0ba1 #x2809 #x2803))

;(test-section "loading hex-file")


;;
;; 全インストラクションテスト
;;
(test-section "=== 全インストラクションテスト ===")
;;   PIC16F690のデータシート、及び「はじめてのPICアセンブラ入門」巻末の命令表に基づいて作成。
;;  「はじめての--」の命令表に何カ所か間違いを発見...

(define cycles #f)

(define (count-cycles second-proc)
  (if second-proc
	  2 ;(second-proc)
	  1))

(define (test-inst caption
				   w-before fsr-before status-before pc-before
				   test-proc
				   w-after  fsr-after  status-after  pc-after
				   cycles-after)
  (print caption)
  (and pc-before ([pic'pc-set!] pc-before))
  (and w-before ([pic'w-set!] w-before))
  (and fsr-before ([pic'f-set!] FSR fsr-before))
  (and status-before ([pic'status-set!] status-before))
  (let1 cycles (count-cycles (test-proc))
	(when w-after
	  (test* (format "w: 0x~x" w-after)
			 w-after [pic'w]))
	(when fsr-after
	  (test* (format "FSR: 0x~x" fsr-after)
			 fsr-after ([pic'f] FSR)))
	(when status-after
	  (test* (format "STATUS: ~s ~s ~s"
					 (if (logbit? 2 status-after) 'Z '-)
					 (if (logbit? 1 status-after) 'DC '-)
					 (if (logbit? 0 status-after) 'C '-))
			 status-after [pic'status]))
	(when cycles-after
	  (test* (format "cycle: ~d" cycles-after)
			 cycles-after cycles))
	(when pc-after
	  (test* (if (= pc-before 0)
				 (format "pc: +~d" pc-after)
				 (format "pc: ~d" pc-after))
			 pc-after [pic'pc]))
	))

;; ADDLW
(test-section "[ADDLW]")
(test-inst "例1) w = 0x10; addlw 0x15 // wレジスタ値0x10に0x15を加える"
		   #x10 #f #b000 0
		   (lambda () ([pic'addlw] #x15))
		   #x25 #f #b000 1
		   1)
(test-inst "例2) w = 0x08; addlw 0x08 // DCビットがセットする加算"
		   #x08 #f #b000 0
		   (lambda () ([pic'addlw] #x08))
		   #x10 #f #b010 1
		   1)
(test-inst "例3) w = 0xFF; addlw 0x01 // マイナス１と１を加算する"
		   #xFF #f #b000 0
		   (lambda () ([pic'addlw] #x01))
		   #x0 #f #b111 1
		   1)
(test-inst "例4) w = 0xF0; addlw 0x10 // DCビットがセットしない桁上がり"
		   #xF0 #f #b000 0
		   (lambda () ([pic'addlw] #x10))
		   #x0 #f #b101 1
		   1)

;; ADDWF
(test-section "[ADDWF]")
(test-inst "例1) FSR = 0xC2; w = 0x17; addwf FSR,0 // wレジスタ値0x17に0xC2を加える。結果はd=0なのでwレジスタへ"
		   #x17 #xC2 #b000 0
		   (lambda () ([pic'addwf] FSR 0))
		   #xD9 #xC2 #b000 1
		   1)
(test-inst "例2) FSR = 0xC2; w = 0x17; addwf FSR,1 // wレジスタ値0x17に0xC2を加える。結果はd=1なのでファイルレジスタ（この場合はFSR）へ"
		   #x17 #xC2 #b000 0
		   (lambda () ([pic'addwf] FSR 1))
		   #x17 #xD9 #b000 1
		   1)
(test-inst "例3) FSR = 0x08 ; w = 0x08; addwf FSR,0 // DCビットがセットする加算"
		   #x08 #x08 #b000 0
		   (lambda () ([pic'addwf] FSR 0))
		   #x10 #x08 #b010 1
		   1)
(test-inst "例4) FSR = 0xFF ; w = 0x01; addwf FSR,0 // マイナス１と１を加算する"
		   #x01 #xFF #b000 0
		   (lambda () ([pic'addwf] FSR 0))
		   #x0 #xFF #b111 1
		   1)

;; ANDLW
(test-section "[ANDLW]")
(test-inst "例1) w = 0xA3; andlw 0x5F // wレジスタ値0xA3に0x5FをANDする"
		   #xA3 #f #b000 0
		   (lambda () ([pic'andlw] #x5F))
		   #x03 #f #b000 1
		   1)
(test-inst "例2) w = 0xA5; andlw 0x5A // wレジスタ値0xA5に0x5AをANDする（結果がゼロ）"
		   #xA5 #f #b000 0
		   (lambda () ([pic'andlw] #x5A))
		   #x0 #f #b100 1
		   1)

;; ANDWF
(test-section "[ANDWF]")
(test-inst "例1) FSR = 0xC2; w = 0x17; andwf FSR,0 // wレジスタ値0x17に0xC2をANDする<1>"
		   #x17 #xC2 #b000 0
		   (lambda () ([pic'andwf] FSR 0))
		   #x02 #xC2 #b000 1
		   1)
(test-inst "例2) FSR = 0xC2; w = 0x17; andwf FSR,1 // wレジスタ値0x17に0xC2をANDする<2>"
		   #x17 #xC2 #b000 0
		   (lambda () ([pic'andwf] FSR 1))
		   #x17 #x02 #b000 1
		   1)
(test-inst "例3) FSR = 0xFE ; w = 0x01; andwf FSR,0 // 結果がゼロ"
		   #x01 #xFE #b000 0
		   (lambda () ([pic'andwf] FSR 0))
		   #x0 #xFE #b100 1
		   1)

;; BCF
(test-section "[BCF]")
(test-inst "例) FSR = 0xFF ; bcf FSR,4 // 4ビット目をリセット"
		   #f #xff #b000 0
		   (lambda () ([pic'bcf] FSR 4))
		   #f #xef #b000 1
		   1)

;; BSF
(test-section "[BSF]")
(test-inst "例) FSR = 0x0 ; bsf FSR,4 // 4ビット目をセット"
		   #f #x0 #b000 0
		   (lambda () ([pic'bsf] FSR 4))
		   #f #x10 #b000 1
		   1)

;; BTFSC
(test-section "[BTFSC]")
(test-inst "例1) STATUS C=1 ; btfsc STATUS,C ; pc == 1 // 条件不成立ジャンプ"
		   #f #f #b001 0
		   (lambda () ([pic'btfsc] STATUS C))
		   #f #f #b001 1
		   1)
(test-inst "例2) STATUS C=0 ; btfsc STATUS,C ; pc == 2 // 条件成立ジャンプ"
		   #f #f #b000 0
		   (lambda () ([pic'btfsc] STATUS C))
		   #f #f #b000 2
		   2)
(test-inst "例3) FSR = 0x05 ; btfsc FSR,2 ; pc == 1 // FSRの2ビット目がセットならジャンプ"
		   #f #x05 #f 0
		   (lambda () ([pic'btfsc] FSR 2))
		   #f #f #f 1
		   1)

;; BTFSS
(test-section "[BTFSS]")
(test-inst "例1) STATUS C=1 ; btfss STATUS,C ; pc == 2 // 条件成立ジャンプ"
		   #f #f #b001 0
		   (lambda () ([pic'btfss] STATUS C))
		   #f #f #b001 2
		   2)
(test-inst "例2) STATUS C=0 ; btfss STATUS,C ; pc == 1 // 条件不成立ジャンプ"
		   #f #f #b000 0
		   (lambda () ([pic'btfss] STATUS C))
		   #f #f #b000 1
		   1)
(test-inst "例3) FSR = 0x05 ; btfss FSR,2 ; pc == 2 // FSRの2ビット目がセットならジャンプ"
		   #f #x05 #f 0
		   (lambda () ([pic'btfss] FSR 2))
		   #f #f #f 2
		   2)

;; CALL
(test-section "[CALL]")
([pic'f-set!] PCLATH 0)
(test-inst "1)"
		   #f #f #f 0
		   (lambda () ([pic'call] #x100))
		   #f #f #f #x100
		   2)
(test-inst "2)"
		   #f #f #f 0
		   (lambda () ([pic'call] #x7ff))
		   #f #f #f #x7ff
		   2)
(test* "CALLスタックの先頭に0x01が入っているか？" #x01 (car [pic'call-stack]))
(test-inst "3) PCLATH = 0 ; call 0x0800 ; pc == 0x0 // 11ビットマスクが働くか"
		   #f #f #f 0
		   (lambda () ([pic'call] #x801))
		   #f #f #f #x1
		   2)

([pic'f-set!] PCLATH #b01000)
(test* "CALLスタックの先頭に0x01が入っているか？" #x01 (car [pic'call-stack]))
(test-inst "4) PCLATH = #b01000 ; call 0x07ff ; pc == 0xfff // 11ビットマスクが働くか"
		   #f #f #f 0
		   (lambda () ([pic'call] #x7ff))
		   #f #f #f #x0fff
		   2)
(test* "CALLスタックの先頭に0x01が入っているか？" #x01 (car [pic'call-stack]))
(test-inst "5) PCLATH = #b01000 ; call 0x0801 ; pc == 0x801 // 11ビットマスクが働くか"
		   #f #f #f 0
		   (lambda () ([pic'call] #x801))
		   #f #f #f #x0801
		   2)

([pic'f-set!] PCLATH #b00011000)
(test-inst "6) PCLATH = #b11000 ; call 0x07ff ; pc == 0x1fff"
		   #f #f #f 0
		   (lambda () ([pic'call] #x7ff))
		   #f #f #f #x1fff
		   2)
(test-inst "7) PCLATH = #b11000 ; call 0x0801 ; pc == 0x1801"
		   #f #f #f 0
		   (lambda () ([pic'call] #x801))
		   #f #f #f #x1801
		   2)

([pic'f-set!] PCLATH #b11111111)
(test-inst "8) PCLATH = #b11111111 ; call 0x07ff ; pc == 0x1fff"
		   #f #f #f 0
		   (lambda () ([pic'call] #x7ff))
		   #f #f #f #x1fff
		   2)
([pic'f-set!] PCLATH 0)

;; CLRF
(test-section "[CLRF]")
(test-inst "例) w = 0xFF; FSR = 0x13; clrf FSR // ファイル・レジスタ（ここではFSR）をクリア"
		   #xff #xfe 0 0
		   (lambda () ([pic'clrf] FSR))
		   #xff #x0 #b100 1
		   1)

;; CLRW
(test-section "[CLRW]")
(test-inst "例) w = 0xFF; FSR = 0x13; clrw // wレジスタをクリア"
		   #xff #xfe 0 0
		   (lambda () ([pic'clrw]))
		   #x0 #xfe #b100 1
		   1)

;; CLRWDT
(test-section "[CLRWDT]")
(print "WDTをクリア")
([pic'clrwdt])
(test* "/**/ WDT == 0" #t #f)
(test* "/**/ WDT prescaler == 0" #t #f)
(test* "^TO (STATUS<4>) = 1" #t (logbit? 4 [pic'status]))
(test* "^PD (STATUS<3>) = 1" #t (logbit? 3 [pic'status]))
;(set! cycles (count-cycles ([pic'clrwdt])))
;(test* "     cycle: 1" 1 cycles)

;; COMF
(test-section "[COMF]") ; invert(f) → (destination)
(test-inst "例1) FSR = 0x13; comf FSR,0"
		   #f #x13 0 0
		   (lambda () ([pic'comf] FSR 0))
		   #xEC #x13 #b000 1
		   1)
(test-inst "例2) FSR = 0x13; comf FSR,1"
		   #f #x13 0 0
		   (lambda () ([pic'comf] FSR 1))
;;;;       #x13 #xEC #b000 1
;;;;        ↑ 実機で確認したけど FSR, 1 の場合はWには影響しない。
;;;;           おそらく MOVLW 0x13; MOVWF FSR; COMF FSR 1 とか試してるんだろうな。
;;;;           COMFの前にせめて MOVLW 0x99 とか、関係ない値を入れてくれたら気がつくはず
		   #f #xEC #b000 1
		   1)
(test-inst "例3) FSR = 0x13; comf FSR,0"
		   #f #xFF 0 0
		   (lambda () ([pic'comf] FSR 0))
		   #x00 #xFF #b100 1
		   1)

;; DECF
(test-section "[DECF]") ; (f) - 1 → (destination)
(test-inst "例1) FSR = 0x13; decf FSR,0"
		   #f #x13 0 0
		   (lambda () ([pic'decf] FSR 0))
		   #x12 #x13 0 1
		   1)
(test-inst "例2) FSR = 0x13; decf FSR,1"
		   #f #x13 0 0
		   (lambda () ([pic'decf] FSR 1))
;;;;	   #x13 #x12 0 1
;;;;        ↑ 実機で確認したけど FSR, 1 の場合はWには影響しない。
		   #f #x12 0 1
		   1)
(test-inst "例3) FSR = 0x01; decf FSR,0 // 結果がゼロ"
		   #f #x01 0 0
		   (lambda () ([pic'decf] FSR 0))
		   #x00 #x01 #b100 1
		   1)
;;// w, fsr, status, pc  さいごにcycles

;; DECFSZ
(test-section "[DECFSZ]") ; (f) - 1 → (destination), skip if result = 0
(test-inst "例1) FSR = 0x01; decfsz FSR,0 // １引いてゼロならジャンプ<1>"
		   #f #x01 #f 0
		   (lambda () ([pic'decfsz] FSR 0))
		   #x00 #x01 #f 2
		   2)

(test-inst "例2) FSR = 0x01; decfsz FSR,1 // １引いてゼロならジャンプ<2>"
		   #f #x01 #f 0
		   (lambda () ([pic'decfsz] FSR 1))
		   #f #x00 #f 2
		   2)

(test-inst "例3) FSR = 0x05; decfsz FSR,0 // 不成立ジャンプ"
		   #f #x05 #f 0
		   (lambda () ([pic'decfsz] FSR 0))
		   #x04 #x05 #f 1
		   1)

;; GOTO
(test-section "[GOTO]")

([pic'f-set!] PCLATH 0)
(test-inst "1) goto 100"
		   #f #f #f 0
		   (lambda () ([pic'goto] 100))
		   #f #f #f 100
		   2)
(test-inst "2) goto #x07ff"
		   #f #f #f 0
		   (lambda () ([pic'goto] #x7ff))
		   #f #f #f #x7ff
		   2)
(test-inst "3) goto #x0801"
		   #f #f #f 0
		   (lambda () ([pic'goto] #x801))
		   #f #f #f #x1
		   2)

([pic'f-set!] PCLATH #b01000)
(test-inst "4) PCLATH = #b01000 ; goto #x07ff"
		   #f #f #f 0
		   (lambda () ([pic'goto] #x7ff))
		   #f #f #f #x0fff
		   2)
(test-inst "5) PCLATH = #b01000 ; goto #x0801"
		   #f #f #f 0
		   (lambda () ([pic'goto] #x801))
		   #f #f #f #x0801
		   2)

([pic'f-set!] PCLATH #b11000)
(test-inst "6) PCLATH = #b11000 ; goto #x07ff"
		   #f #f #f 0
		   (lambda () ([pic'goto] #x7ff))
		   #f #f #f #x1fff
		   2)
(test-inst "7) PCLATH = #b11000 ; goto #x0801"
		   #f #f #f 0
		   (lambda () ([pic'goto] #x801))
		   #f #f #f #x1801
		   2)

([pic'f-set!] PCLATH #b11111111)
(test-inst "8) PCLATH = #b11111111 ; goto #x07ff"
		   #f #f #f 0
		   (lambda () ([pic'goto] #x7ff))
		   #f #f #f #x1fff
		   2)

([pic'f-set!] PCLATH 0)

;; INCF
(test-section "[INCF]") ; (f) + 1 → (destination)
(test-inst "例1) FSR = 0x13; incf FSR,0"
		   #f #x13 0 0
		   (lambda () ([pic'incf] FSR 0))
		   #x14 #x13 0 1
		   1)
(test-inst "例2) FSR = 0x13; incf FSR,1"
		   #f #x13 0 0
		   (lambda () ([pic'incf] FSR 1))
;		   #x13 #x14 0 1 -- w=0x13になるのって誤植？本当？実機でテストしないと。
		   #f #x14 0 1
		   1)
(test-inst "例3) FSR = 0xFF; incf FSR,0 // 結果がゼロ"
		   #f #xFF 0 0
		   (lambda () ([pic'incf] FSR 0))
		   #x00 #xFF #b100 1
		   1)
;;// w, fsr, status, pc  さいごにcycles

;; INCFSZ
(test-section "[INCFSZ]") ; (f) + 1 → (destination), skip if result = 0
(test-inst "例1) FSR = 0xFF; incfsz FSR,0 // １足してゼロならジャンプ<1>"
		   #f #xFF #f 0
		   (lambda () ([pic'incfsz] FSR 0))
		   #x00 #xFF #f 2
		   2)

(test-inst "例2) FSR = 0xFF; incfsz FSR,1 // １足してゼロならジャンプ<2>"
		   #f #xFF #f 0
		   (lambda () ([pic'incfsz] FSR 1))
		   #f #x00 #f 2
		   2)

(test-inst "例3) FSR = 0x05; incfsz FSR,0 // 不成立ジャンプ"
		   #f #x05 #f 0
		   (lambda () ([pic'incfsz] FSR 0))
		   #x06 #x05 #f 1
		   1)

;; IORLW
(test-section "[IORLW]")
(test-inst "例1) w = 0x9A; iorlw 0x35 // wレジスタ値0x9Aに0x35をORする"
		   #x9A #f #b000 0
		   (lambda () ([pic'iorlw] #x35))
		   #xBF #f #b000 1
		   1)
(test-inst "例2) w = 0x00; iorlw 0x00 // wレジスタ値0x00に0x00をORする"
		   #x00 #f #b000 0
		   (lambda () ([pic'iorlw] #x00))
		   #x00 #f #b100 1
		   1)

;; IORWF
(test-section "[IORWF]")
(test-inst "例1) FSR = 0x91; w = 0x13; iorwf FSR,0 // wレジスタ値0x13に0x91をORする<1>"
		   #x91 #x13 #b000 0
		   (lambda () ([pic'iorwf] FSR 0))
		   #x93 #x13 #b000 1
		   1)
(test-inst "例2) FSR = 0x91; w = 0x13; iorwf FSR,1 // wレジスタ値0x13に0x91をORする<2>"
		   #x91 #x13 #b000 0
		   (lambda () ([pic'iorwf] FSR 1))
		   #x91 #x93 #b000 1
		   1)
(test-inst "例3) FSR = 0x00 ; w = 0x00; iorwf FSR,0 // wレジスタ値0x00に0x00をORする"
		   #x0 #x0 #b000 0
		   (lambda () ([pic'iorwf] FSR 0))
		   #x0 #x0 #b100 1
		   1)

;; MOVF
(test-section "[MOVF]")
(test-inst "例1) FSR = 0xA5 ; movf FSR,0 // ファイル・レジスタの内容を移動"
		   #f #xA5 #b000 0
		   (lambda () ([pic'movf] FSR 0))
		   #xA5 #xA5 #b000 1
		   1)
(test-inst "例2) FSR = 0xA5 ; movf FSR,1 // ファイル・レジスタの内容を移動"
		   #f #xA5 #b000 0
		   (lambda () ([pic'movf] FSR 1))
		   #f #xA5 #b000 1
		   1)
(test-inst "例3) FSR = 0x00 ; movf FSR,1 // ファイル・レジスタの内容をセット（ファイル・レジスタの内容がゼロかどうかテストするのに使える）"
		   #f #x0 #b000 0
		   (lambda () ([pic'movf] FSR 0))
		   #f #x00 #b100 1
		   1)

;; MOVLW
(test-section "[MOVLW]")
(test-inst "例) w = 0xFF ; movlw 0x5A // wレジスタに0x5Aをセット"
		   #xFF #f #b000 0
		   (lambda () ([pic'movlw] #x5A))
		   #x5A #f #b000 1
		   1)

;; MOVWF
(test-section "[MOVWF]")
(test-inst "例) FSR = 0xA5; w = 0xFF ; movwf FSR // wレジスタの内容をファイル・レジスタにセット"
		   #xB5 #xA5 #b000 0
		   (lambda () ([pic'movwf] FSR))
		   #xB5 #xB5 #b000 1
		   1)

;; NOP
(test-section "[NOP]") ; No operation
(test-inst "例)"
		   #x12 #x34 #b0 0
		   (lambda () ([pic'nop]))
		   #x12 #x34 #b0 1
		   1)

;; RETFIE
(test-section "[RETFIE]")
(let1 tos [pic'tos]
  ([pic'retfie])
  (test* "PC == TOS" tos [pic'pc])
  (test* "GIE == 1" 1 ([pic'f-bit] INTCON 7))
  ;; 割り込みをかけて、W/STATUS等を元通りに戻せるかとかテストしたい
  )
;(set! cycles (count-cycles ([pic'clrwdt])))
;(test* "     cycle: 2" 2 cycles)

;; PC = TOS
;; GIE = 1
;(test-inst "例)"
;		   #x12 #x34 #b0 0
;		   (lambda () ([pic'retfie]))
;		   #x12 #x34 #b0 1
;		   1)

;; RETLW
(test-section "[RETLW]")
([pic'pc-set!] 20)
([pic'call] 1000)
(test-inst "例)"
		   #x0 #f #b0 1001
		   (lambda () ([pic'retlw] 123))
		   123 #f #b0 21
		   2)

;; RETURN
(test-section "[RETURN]")
([pic'pc-set!] 20)
([pic'call] 1000)
(test-inst "例)"
		   #x0 #f #b0 1001
		   (lambda () ([pic'return]))
		   #x0 #f #b0 21
		   2)

;; RLF
(test-section "[RLF]")
(test-inst "例1) ファイル・レジスタの内容0x20を左シフト<1>"
		   #f #x20 #b0 0
		   (lambda () ([pic'rlf] FSR 0))
		   #x40 #x20 #b0 1
		   1)
(test-inst "例2) ファイル・レジスタの内容0x20を左シフト<2>"
		   #f #x20 #b0 0
		   (lambda () ([pic'rlf] FSR 1))
;;;;	   #x20 #x40 #b0 1
;;;;        ↑ 実機で確認したけど FSR, 1 の場合はWには影響しない。
		   #f #x40 #b0 1
		   1)
(test-inst "例3) ビット飛び出し"
		   #f #x80 #b0 0
		   (lambda () ([pic'rlf] FSR 0))
		   #x00 #x80 #b001 1
		   1)
(test-inst "例4) ビット挿入"
		   #f #x20 #b001 0
		   (lambda () ([pic'rlf] FSR 0))
		   #x41 #x20 #b000 1
		   1)
(test-inst "例5) ビット飛び出しを挿入"
		   #f #x88 #b001 0
		   (lambda () ([pic'rlf] FSR 0))
		   #x11 #x88 #b001 1
		   1)

;; RRF
(test-section "[RRF]")
(test-inst "例1) ファイル・レジスタの内容0x20を右シフト<1>"
		   #f #x20 #b0 0
		   (lambda () ([pic'rrf] FSR 0))
		   #x10 #x20 #b0 1
		   1)
(test-inst "例2) ファイル・レジスタの内容0x20を右シフト<2>"
		   #f #x20 #b0 0
		   (lambda () ([pic'rrf] FSR 1))
;;;;	   #x20 #x10 #b0 1
;;;;        ↑ 実機で確認したけど FSR, 1 の場合はWには影響しない。
		   #f #x10 #b0 1
		   1)
(test-inst "例3) ビット飛び出し"
		   #f #x01 #b0 0
		   (lambda () ([pic'rrf] FSR 0))
		   #x00 #x01 #b001 1
		   1)
(test-inst "例4) ビット挿入"
		   #f #x20 #b001 0
		   (lambda () ([pic'rrf] FSR 0))
		   #x90 #x20 #b000 1
		   1)
(test-inst "例5) ビット飛び出しを挿入"
		   #f #x11 #b001 0
		   (lambda () ([pic'rrf] FSR 0))
		   #x88 #x11 #b001 1
		   1)

;; SLEEP
(test-section "[SLEEP]")
([pic'sleep])
(test* "/**/ WDT == 0" #t #f)
(test* "/**/ WDT prescaler == 0" #t #f)
(test* "^TO (STATUS<4>) = 1" #t (logbit? 4 [pic'status]))
(test* "^PD (STATUS<3>) = 0" #f (logbit? 3 [pic'status]))

(test* "sleep?" #t [pic'sleep?])
;(set! cycles (count-cycles ([pic'clrwdt])))
;(test* "     cycle: 1" 1 cycles)

;; SUBLW
(test-section "[SUBLW]") ; k - (w) = (w)
(test-inst "例1) w = 0x05; sublw 0x07 // 定数 7 からwレジスタ値 5 を引く"
		   #x05 #f #b000 0
		   (lambda () ([pic'sublw] #x07))
		   #x02 #f #b011 1
		   1)
(test-inst "例2) w = 0x01; sublw 0x10 // 0x10から１を引く"
		   #x01 #f #b000 0
		   (lambda () ([pic'sublw] #x10))
;;;;	   #x0F #f #b011 1
;;;;        ↑ DCはボローが出るから立たないのでは？
;;;;        ↑ 実機で確認したら、やっぱりcarryしか立ってない。
		   #x0F #f #b001 1
		   1)
(test-inst "例3) w = 0x01; sublw 0x01 // 結果がゼロ"
		   #x01 #f #b000 0
		   (lambda () ([pic'sublw] #x01))
		   #x00 #f #b111 1
		   1)
(test-inst "例4) w = 0x07; sublw 0x05 // 結果がマイナス"
		   #x07 #f #b000 0
		   (lambda () ([pic'sublw] #x05))
		   #xFE #f #b000 1
		   1)
(test-inst "例5) w = 0x10; sublw 0x01 // 結果がマイナス、DCセット"
		   #x10 #f #b000 0
		   (lambda () ([pic'sublw] #x01))
		   #xF1 #f #b010 1
		   1)

;; SUBWF
(test-section "[SUBWF]") ; (f) - (w) → (destination)
(test-inst "例1) FSR = 0x07 ; w = 0x05; subwf FSR,0 // ファイルレジスタの 7 からwレジスタの 5 を引く<1>"
		   #x05 #x07 #b000 0
		   (lambda () ([pic'subwf] FSR 0))
		   #x02 #x07 #b011 1
		   1)
(test-inst "例2) FSR = 0x07 ; w = 0x05; subwf FSR,1 // ファイルレジスタの 7 からwレジスタの 5 を引く<2>"
		   #x05 #x07 #b000 0
		   (lambda () ([pic'subwf] FSR 1))
		   #x05 #x02 #b011 1
		   1)
(test-inst "例3) FSR = 0x01 ; w = 0x10; subwf FSR,0 // 結果がマイナス、DCセット"
		   #x10 #x01 #b000 0
		   (lambda () ([pic'subwf] FSR 0))
		   #xF1 #x01 #b010 1
		   1)
(test-inst "例4) FSR = 0x01 ; w = 0x01; subwf FSR,0 // 結果がゼロ"
		   #x01 #x01 #b000 0
		   (lambda () ([pic'subwf] FSR 0))
		   #x00 #x01 #b111 1
		   1)
(test-inst "例5) FSR = 0x05; w = 0x07; subwf FSR,0 // 結果がマイナス"
		   #x07 #x05 #b000 0
		   (lambda () ([pic'subwf] FSR 0))
		   #xFE #x05 #b000 1
		   1)

;; SWAPF
(test-section "[SWAPF]")
(test-inst "例1) FSR = 0xA5; swapf FSR,0 // ファイル・レジスタの内容の上下を入れ替える"
		   #f #xA5 #b000 0
		   (lambda () ([pic'swapf] FSR 0))
		   #x5A #xA5 #b000 1
		   1)
(test-inst "例2) FSR = 0xA5; swapf FSR,1 // ファイル・レジスタの内容の上下を入れ替える"
		   #f #xA5 #b000 0
		   (lambda () ([pic'swapf] FSR 1))
;;;;	   #xA5 #x5A #b000 1
;;;;        ↑ 実機で確認したけど FSR, 1 の場合はWには影響しない。
		   #f #x5A #b000 1
		   1)

;; XORLW
(test-section "[XORLW]")
(test-inst "例1) w = 0xB5; xorlw 0xAF // wレジスタ値0xB5に0xAFをXORする"
		   #xB5 #f #b000 0
		   (lambda () ([pic'xorlw] #xAF))
		   #x1A #f #b000 1
		   1)
(test-inst "例2) w = 0x00; xorlw 0x00 // wレジスタ値0x00に0x00をXORする"
		   #x00 #f #b000 0
		   (lambda () ([pic'xorlw] #x00))
		   #x00 #f #b100 1
		   1)
(test-inst "例3) w = 0xA5; xorlw 0xA5 // 同じ値をXORする"
		   #xA5 #f #b000 0
		   (lambda () ([pic'xorlw] #xA5))
		   #x00 #f #b100 1
		   1)

;; XORWF
(test-section "[XORWF]")
(test-inst "例1) FSR = 0xAF ; w = 0xB5; xorwf FSR,0 // wレジスタ値0xB5にファイル・レジスタの0xAFをXORする<1>"
		   #xB5 #xAF #b000 0
		   (lambda () ([pic'xorwf] FSR 0))
		   #x1A #xAF #b000 1
		   1)
(test-inst "例2) FSR = 0xAF ; w = 0xB5; xorwf FSR,0 // wレジスタ値0xB5にファイル・レジスタの0xAFをXORする<2>"
		   #xB5 #xAF #b000 0
		   (lambda () ([pic'xorwf] FSR 1))
		   #xB5 #x1A #b000 1
		   1)
(test-inst "例3) FSR = 0x00 ; w = 0x00; xorwf FSR,0 // wレジスタ値0x00に0x00をXORする"
		   #x0 #x0 #b000 0
		   (lambda () ([pic'xorwf] FSR 0))
		   #x0 #x0 #b100 1
		   1)
(test-inst "例4) FSR = 0xA5 ; w = 0xA5; xorwf FSR,0 // 同じ値をXORする"
		   #xA5 #xA5 #b000 0
		   (lambda () ([pic'xorwf] FSR 0))
		   #x0 #xA5 #b100 1
		   1)

(test-end)
;(dump-snapshot [pic'snapshot] 'full)
