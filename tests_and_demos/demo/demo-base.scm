;;;
;;; macro-asm-test
;;;
;;;   Copyright (c) 2008 naoya_t (naoya.t@aqua.plala.or.jp)
;;;
(define *disable-writing* #t)
(define *disable-writing* #f)

(use srfi-1)
(use util.match)

(require "./macro-asm")
(require "./pic-asm")
(require "./pic-disasm")
;(require "./pic-device")

(require "./pic16f690-emulator")
(require "./intelhex-lib") ; save-obj-code
;(require "./pic16f690-repl")

(define (4bit val)
  (string-append (if (logbit? 3 val) "● " "○ ")
				 (if (logbit? 2 val) "● " "○ ")
				 (if (logbit? 1 val) "● " "○ ")
				 (if (logbit? 0 val) "●" "○")))
;  (string-join (map (lambda (b)
;					  (if (logbit? b val) "●" "○"))
;					'(3 2 1 0))
;			   " "))

(define cnt 0)
(define sometimes #f)
;(define sometimes 1)
;(define sometimes 1)
(define verbose-mode #f)
(define show-led #f)
;(define verbose-mode #t)


;(define (macro-asm-test regs src asm? disasm? write? runsec)
(define (macro-asm-test regs src asm? disasm? . rest)
  (print "-------")
  ;; マクロアセンブラ
  (let1 macro-asm (make-macro-assembler (list plug-in:basic
											  plug-in:register-machine
											  plug-in:wait
											  plug-in:LED
											  plug-in:scheme
											  plug-in:debug
											  ))
	;;コンパイル
	(when (and regs (not (null? regs)))
	  (set! src (append (append-map (lambda (reg) `((alloc ,reg))) regs) src)))

	(let1 asm-code (macro-asm src)

	  (when asm?
;		(map print asm-code)
;		(print "-------")
		(map asm-pp asm-code)
		(print "-------"))
	  
	  ;;アセンブル
	  (let1 obj-code (pic-asm asm-code)
		;(print obj-code)
		;(print "-------")
		
		;;オブジェクトコードをdisasmして表示（デバッグ用）
		(when disasm? 
		  (map print (pic-disasm obj-code))
		  (print "-------"))
		
;		(when (and (not *disable-writing*) write?)
;;		  ;;オブジェクトコードをPICに書き込んで実行（５秒）
;;		  (pic-write obj-code)
		(let1 hex-file "_emutest.hex"
		  (save-obj-code obj-code hex-file)
;		  (sys-system "gosh pic16f690-main.scm _emutest.hex")

		  (let1 pic (make-pic16f690)
			([pic'load-hex-file] hex-file)
			(print "load hex-file completed")

			[pic'on]

			(let loop ([last-portc -1])
			  (when (or verbose-mode (and sometimes (zero? (remainder cnt sometimes))))
				(if [pic'finished?]
					(begin (when show-led (printf "> %s\n" (4bit last-portc)))
						   (dump-snapshot [pic'snapshot])
						   
						   (printf "%08x: %s  ;; %s\n"
								   [pic'pc] (mnemonic-pp [pic'mnemonic])
								   (map (cut sprintf "%04x" <>) [pic'call-stack])
								   ))
					(print "// waiting 1 cycle... //")
					))

			  ;;(profiler-reset)
			  ;;(profiler-start)
			  [pic'step]
			  ;;(profiler-stop)
			  ;;(profiler-show)
			  (inc! cnt)

			  (let1 portc (logand #b1111 ([pic'f] 7))
				(when show-led
				  (unless (= portc last-portc)
					(printf "> %s\r" (4bit portc))
					(flush)
					(inc! cnt)))
				
				(if verbose-mode
				  (guard (e (else
							 [pic'halt]
							 (printf ";; %08x: %s\n" [pic'pc] [pic'mnemonic])
							 (dump-snapshot [pic'snapshot] 'full-mode)
							 (print "ERROR: " (ref e 'message))
							 ))
					(unless [pic'halt?] (loop portc)))
				  (unless [pic'halt?] (loop portc))
				  )))
			)))
	  
	  
	  )))
		  
;;		  ;;指定した秒数だけ給電
;;		  (when (number? runsec)
;;			(pic-on)
;;			(sys-sleep runsec)
;;			(pic-off))
;;		  ;;delete hex-file here if you wish
;;		  )))))
