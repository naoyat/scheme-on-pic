;;
;; PIC16F690 Emulator
;;
;;  2008/4/17 by naoya_t
;;
(use gauche.uvector)
(use util.match)
(use srfi-1)

(use nt.printf)
(require "./intelhex-lib")

(use gauche.threads)

(define (clone-vec vec)
  (cond
   [(u8vector? vec) (u8vector-copy vec)]
   [(u16vector? vec) (u16vector-copy vec)]
   [(u32vector? vec) (u32vector-copy vec)]
   [(vector? vec) (vector-copy vec)]
   [else vec]))

(define-macro (XX-for bits fn8 fn16 fn32 fnfail)
  `(cond [(<= 1 bits 8) ,fn8]
		 [(<= 9 bits 16) ,fn16]
		 [(<= 17 bits 32) ,fn32]
		 [else ,fnfail]))

(define-macro (make-uXXvector-for bits)
  `(XX-for ,bits make-u8vector make-u16vector make-u32vector make-vector))
(define-macro (uXXvector-ref-for bits)
  `(XX-for ,bits u8vector-ref u16vector-ref u32vector-ref vector-ref))
(define-macro (uXXvector-set!-for bits)
  `(XX-for ,bits u8vector-set! u16vector-set! u32vector-set! vector-set!))
(define-macro (max-value-for bits)
  `(- (ash 1 ,bits) 1))

; (define (make-range lower-limit upper-limit)
;   (let1 length (+ (- upper-limit lower-limit) 1)
;	 (define (offset val)
;	   (if (<= lower-limit val upper-limit)
;		   (- val lower-limit)
;		   #f))
;	 (lambda (m)
;	   (case m
;		 [(lower-limit) lower-limit]
;		 [(upper-limit) upper-limit]
;		 [(length) length]
;		 [(offset) offset]
;		 [(iota) (iota length lower-limit)]
;		 [(desc) (sprintf "[%x .. %x]" lower-limit upper-limit)]
;		 ))))

; (define (make-memory bits range . args)
;   (when (number? range)
;	 (set! range (make-range 0 (- range 1))))
;   (let ([implemented-memory-size
;		  (if (and (<= 1 (length args)) (first args))
;			  (first args) [range'length])]
;		 [mask
;		  (if (and (<= 2 (length args)) (second args))
;			  (map (cut - <> [range'lower-limit]) (second args)) #f)]
;		 [null-val
;		  (if (and (<= 3 (length args)) (third args))
;			  (third args) 0)]
;		 )
; ;;	(printf "%s, %d, %s\n" [range'desc] implemented-memory-size mask)
;	 (let1 memory ((make-uXXvector-for bits) implemented-memory-size null-val)
;	   (define (map-address-into-implemented-memory addr)
;		 (let1 ofs ([range'offset] addr)
;		   (if ofs
;			   (let1 mapped (remainder ofs implemented-memory-size)
;				 (if (if mask (memq mapped mask) #t) ;; maskがある場合はaddrがその中に含まれることが要件。
;					 mapped
;					 #f))
;			   #f)))
;	   (define peek
;		 (if (= bits 8)
;			 (lambda (addr) (u8vector-ref memory addr))
;			 (lambda (addr) (u16vector-ref memory addr)) ))
;	   (define poke
;		 (if (= bits 8)
;			 (lambda (addr val) (u8vector-set! memory addr val))
;			 (lambda (addr val) (u16vector-set! memory addr val)) ))
;
;	   (lambda (m)
;		 (case m
;		   [(vec) memory]
;		   [(peek) peek]
;		   [(poke) poke] ; available only when writing program
;		   [(range-iota) (or mask (iota implemented-memory-size [range'lower-limit]))]
;		   )) )))

(define *program-memory-size* #x1000)

(define (make-pic16f690)
  ;; STATUS f-reg
  (define INDF 0)
  ;; (define TMR0 1)
  (define PCL 2)
  (define STATUS 3)
  (define FSR 4)
  (define PCLATH #x0a)
  (define INTCON #x0b)

  (define EEDAT #x10c)
  (define EEADR #x10d)
  (define EEDATH #x10e)
  (define EEADRH #x10f)
  (define EECON1 #x18c)
  (define EECON2 #x18d)

  (define IRP 7)
  (define RP1 6)
  (define RP0 5)
  (define ^TO 4)
  (define ^PD 3)
  (define Z 2)
  (define DC 1)
  (define C 0)

  (let ([w-reg 0]
		[file-regs (make-u8vector #x200)] ;;(make-memory 8 #x200)]
		[program-mem (make-u16vector #x1000)] ;;(make-memory 14 (make-range 0 #x1fff) *program-memory-size* #f #x3fff)]
		[precompiled-inst-proc (make-vector #x1000)]
		[precompiled-inst-mnemonic (make-vector #x1000)]
		[pc 0]
		[call-stack (make-u16vector 8 0)]
		[call-stack-head 0]
		[config-mem (make-u16vector #x80)] ;; (make-memory 14 (make-range #x2000 #x3fff) #x80 ; configuration memory
;								  '(#x2000 #x2001 #x2002 #x2003 #x2007 #x2008))] ;
		[eeprom (make-u8vector 256)] ;(make-memory 8 (make-range 0 #xff) #x100)] ; Data EEPROM. r/w.
		[second-proc #f] ; 2クロック命令で使う
		[power-stat #f]
		[sleep-stat #f]
		[halt-stat #f]
		[interrupt-enabled #f]
		)

	(define (tos) (u16vector-ref call-stack 7)) ; top of stack

	(define (push-call-stack addr)
	  (u16vector-set! call-stack call-stack-head addr)
	  (inc! call-stack-head)
	  (when (= call-stack-head 8)
		(set! call-stack-head 0)))

	(define (pop-call-stack)
	  (set! call-stack-head
			(if (= call-stack-head 0) 7 (- call-stack-head 1)))
	  (u16vector-ref call-stack call-stack-head))
	
	;; リセット信号
	(define (por-reset) ;; Power-on Reset
	  (u8vector-copy! file-regs 0
					  #u8(0 0 0 #x18 0 0 0 0 0 0 0 0 0 0 0 0
							0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
	  (u8vector-copy! file-regs #x80
					  #u8(0 #xff 0 #x18 0 #x3f #xf0 #xff 0 0 0 0 0 0 #x10 #x60
							0 0 #xff #xff 0 #x37 0 #x08 #x02 0 0 #x40 0 0 0 0))
	  (u8vector-copy! file-regs #x10c
					  #u8(0 0 0 0
							0 0 0 0 0 #xf0 0 0 0 0 0 #x02 0 0 #xff #x0f))
	  (u8vector-copy! file-regs #x18c #u8(0 0))
	  (u8vector-copy! file-regs #x19d #u8(1 0))
	  (set! pc 0)
	  (set! w-reg 0)
	  )
	(define (other-reset) ;; BOR (Brown-out Reset)とか
	  (por-reset) ;ほんとは違う
	  )

	(define (do-call subr-addr)
	  ;;	  (printf "do CALL : subr=%x, next=%x => stack[%d]\n"
 ;			  subr-addr (+ pc 1) call-stack-head)
	  (push-call-stack (+ pc 1))
;	   (set! pc subr-addr))
	  (pc= subr-addr))

	(define (do-return)
;	   (set! pc (pop-call-stack))
	  (pc= (pop-call-stack)))
 ;	  (printf "do RETURN : next=%x => stack[%d]\n"
 ;			  pc call-stack-head)

	(define (pc++)
;	   (set! pc (logand #x1fff (+ pc 1)))
	  (inc! pc)
	  (when (<= #x2000 pc) (set! pc (logand #x1fff pc)))
;	   (u8vector-set! file-regs PCLATH (ash pc -8))
	  (u8vector-set! file-regs PCL (logand #x00ff pc))
	   )
										;	   (u8vector-set! file-regs PCL pc))
	(define (pc+=2)
;	   (set! pc (logand #x1fff (+ pc 2)))
	  (set! pc (+ pc 2))
	  (when (<= #x2000 pc) (set! pc (logand #x1fff pc)))
;	   (u8vector-set! file-regs PCLATH (ash pc -8))
	  (u8vector-set! file-regs PCL (logand #x00ff pc))
	  )
;	   (u8vector-set! file-regs PCL pc))
	(define (pc= addr)
										;	   (set! pc (logand #x1fff addr))
	  (set! pc addr)
;	   (when (<= #x2000 pc) (set! pc (logand #x1fff pc)))
;	   (u8vector-set! file-regs PCLATH (ash pc -8))
	  (u8vector-set! file-regs PCL (logand #x00ff pc))
	  )
;	   (u8vector-set! file-regs PCL pc))

	 ;; instructions
	(define (status-get) (u8vector-ref file-regs STATUS))
	(define (status-set! val) (u8vector-set! file-regs STATUS val))

;	 (define (rp) (logand 3 (ash (status-get) -5))) ; Page (bank)
	(define (rp) (logand 3 (ash (u8vector-ref file-regs STATUS) -5))) ; Page (bank)
;	 (define (irp) (ash (status-get) -7)) ; IRP
	(define (irp) (ash (u8vector-ref file-regs STATUS) -7)) ; IRP

;	 (define (w-get) w-reg)
	(define (w-set! val) (set! w-reg (logand #xff val)))

;	 (define ash+7 #(0 128 256 384))
;	 (define ash+8 #(0 256 512 768))

	(define (actual-addr masked)
	  (let1 at (+ (* (rp) 128) masked)
;	   (let1 at (logior (* (rp) 128) masked)
;	   (let1 at (logior (ref ash+7 (rp)) masked)
;	   (let1 at (logior (ref #(0 128 256 384) (rp)) masked)
;	   (let1 at (logior (ash (rp) 7) masked)
;	   (let1 at masked
		(cond
		 [(= masked 0) ; INDF : FSRの内容
;		   (logior (ash (irp) 8)
;		   (logior (ref ash+8 (irp))
;		   (logior (* (irp) 256)
		  (+ (* (irp) 256)
			 (u8vector-ref file-regs FSR))]
		 [(memq at '(8 9 #x1b
					   #x88 #x89 #x91 #x9c #x9d
					   #x108 #x109 #x110 #x111 #x112 #x113 #x114 #x117 #x11c #x11d
					   #x188 #x189 #x18e #x18f #x190 #x191 #x192 #x193 #x194 #x195
					   #x196 #x197 #x198 #x199 #x19a #x19b #x19c #x19f))
		  #f] ; 実装されていない
		 [(<= #x1a0 at #x1ef) #f]
		 [(and (< #x80 at)
			   (or (<= #x70 masked)
				   (memq masked '(2 3 4 #xa #xb))))
		  masked]
		 [(and (<= #x100 at) (<= masked #x0b))
		  (logand #xff at)]
		 [else at])))
	
	(define (file-get f)
	  (let1 addr (actual-addr f)
		(if addr
			(u8vector-ref file-regs addr)
			0)))
	(define (file-set! f val)
	  (let1 addr (actual-addr f)
		(when addr
;		   (printf "file[%x] = %b\n" addr val)
		  (u8vector-set! file-regs addr val)
		  (cond [(= addr PCL)
;			 (printf "(PCLATH, PCL) <= %02x %02x\n"
;					 (u8vector-ref file-regs PCLATH) val)
;			 (let1 new-pc (logior (ash (u8vector-ref file-regs PCLATH) 8)
;			 (let1 new-pc (logior (* (u8vector-ref file-regs PCLATH) 256)
				 (let1 new-pc (+ (* (u8vector-ref file-regs PCLATH) 256)
								 (u8vector-ref file-regs PCL))
				   ;;			   (pc= new-pc)
				   (pc= (- new-pc 1))
				   )]
				[(= addr INTCON)
				 (let1 GIE (logbit? 7 val)
				   (set! interrupt-enabled GIE)
				   (printf "<Interrupt:%s>\n" (if GIE "Enabled" "Disabled"))
				   )]
				[(= addr EECON1)
				 (eeprom-control val)]
										;				 [else]
				)
				 
										;			   (set! pc new-pc))
										;			 (printf "  pc = %04x\n" pc)
		  )))

	(define (eeprom-get addr)
	  (if (<= 0 addr #xff)
		  (u8vector-ref eeprom addr)
		  0))
	(define (eeprom-set! addr val)
	  (if (and (<= 0 addr #xff) (<= 0 val #xff))
		  (u8vector-set! eeprom addr val)
		  #f))
	
	(define (eeprom-control eecon1)
	  (let ([EEPGD (logbit? 7 eecon1)]
			[WRERR (logbit? 3 eecon1)]
			[WREN (logbit? 2 eecon1)]
			[WR (logbit? 1 eecon1)]
			[RD (logbit? 0 eecon1)] )
										;		 (printf "%s - - - %s %s %s %s\n"
;				 (if EEPGD "EEPGD" "-")
;				 (if WRERR "WRERR" "-")
;				 (if WREN "WREN" "-")
;				 (if WR "WR" "-")
;				 (if RD "RD" "-"))
		(if EEPGD
			 ;; accesses program memory
			(when RD
			  (print "start reading program memory...")
			   ;; start reading
;			   (file-set! EEDAT (eeprom-get 0))
			  (file-set! EECON1 (logand eecon1 #b11111110))
			  )
			 ;; accesses data memory
			(if RD
				(begin
				  (display "start reading data memory...")
				  (flush)
				  (printf "(eeprom-get %d) = %d\n"
						  (file-get EEADR)
						  (eeprom-get (file-get EEADR)))
				  (file-set! EEDAT (eeprom-get (file-get EEADR)))
				  ;; (file-set! EEDAT (+ 100 (file-get EEADR)))
				  (print "done")
				  (file-set! EECON1 (logand eecon1 #b11111110)) ;;(file-set-bit! EECON1 RD 0)
				  )
				(when (and WR WREN)
				  0 ;; start writing
				  (let1 thread (make-thread (lambda ()
											  (display "start writing data memory...")
											  (flush)
											  (printf "(eeprom-set! %d %d)\n"
													  (file-get EEADR)
													  (file-get EEDAT))
											  (eeprom-set! (file-get EEADR)
														   (file-get EEDAT))
											  (printf "=> %d\n"
													  (eeprom-get (file-get EEADR)))
;											  (sys-sleep 1)
											  (sys-nanosleep 100000000) ; 0.5sec
;											  (thread-sleep! 2)
											  (print "done")
											  (file-set! EECON1 (logand eecon1 #b11111101))
											  ;;(file-set-bit! EECON1 WR 0)
											  (set! sleep-stat #f)
											  ) 'eeprom-writing-data-memory)
					(thread-start! thread)
					)))
			)
										;		 (printf "* EECON1 := %b\n" eecon1)
		))

	(define (set-value! f newval)
	  (if f (file-set! f newval) (w-set! newval)))
	(define (direction f? f) (and (memq f? '(#t 1)) f))
	
	(define *mask* #(1 2 4 8 16 32 64 128))

	(define (file-get-bit f b)
	  (if (logbit? b (file-get f)) 1 0))
	(define (file-set-bit! f b val)
	  (let ([fval (file-get f)]
;			 [mask (ref *mask* b)])
			[mask (ash 1 b)])
		(let1 new-fval (if (= val 1)
						   (logior fval mask)
						   (logand fval (lognot mask)))
		  (file-set! f new-fval))))

	(define (status-c?) (logbit? 0 (status-get)))
	(define (status-set-c!) (status-set! (logior #b00000001 (status-get))))
	(define (status-unset-c!) (status-set! (logand #b11111110 (status-get))))
	
	(define (status-dc?) (logbit? 1 (status-get)))
	(define (status-set-dc!) (status-set! (logior #b00000010 (status-get))))
	(define (status-unset-dc!) (status-set! (logand #b11111101 (status-get))))
	
	(define (status-z?) (logbit? 2 (status-get)))
	(define (status-set-z!) (status-set! (logior #b00000100 (status-get))))
	(define (status-unset-z!) (status-set! (logand #b11111011 (status-get))))

	(define wait-1-cycle-proc (lambda () #f))

	 ;; snapshot
	(define (snapshot)
	  `(PIC16F690
		(w-reg ,w-reg)
		(file-regs ,(clone-vec file-regs))
		(program ,(clone-vec program-mem)) ;[program-mem'vec]))
		(pc ,pc ,second-proc)
		(call-stack ,(clone-vec call-stack) ,call-stack-head)
		(config ,(clone-vec config-mem)) ;[config-mem'vec]))
		(eeprom ,(clone-vec eeprom)) ;[eeprom'vec]))
		(power ,(if power-stat 'on 'off))
		(sleep ,(if sleep-stat 'yes 'no))
		(halt ,(if halt-stat 'yes 'no))
		))

	 ;; byte
	(define (addwf f f?) ; C DC Z
	  (let ([old-w w-reg]
			[old-f (file-get f)])
		(let1 newval (+ old-w old-f)
		  (set-value! (direction f? f) (logand #xff newval))
		  (if (< 255 newval)
			  (status-set-c!) (status-unset-c!))
		  (if (= 0 (logand #xff newval))
			  (status-set-z!) (status-unset-z!))
		  (if (<= 16 (+ (logand #x0f old-f) (logand #x0f old-w)))
										;		   (if (and (< (if f? old-f old-w) 16)
										;					(<= 16 newval))
			  (status-set-dc!) (status-unset-dc!))
		  ))
	  (pc++)
	  #f)

	(define (andwf f f?) ; Z
	  (let1 newval (logand w-reg (file-get f))
		(set-value! (direction f? f) newval)
		(if (zero? newval) (status-set-z!) (status-unset-z!)))
	  (pc++)
	  #f)

	(define (clrf f) ; Z
	  (file-set! f 0)
	  (status-set-z!)
	  (pc++)
	  #f)

	(define (clrw) ; Z
	  (w-set! 0)
	  (status-set-z!)
	  (pc++)
	  #f)
	
	(define (comf f f?) ; Z
	  (let1 newval (- #xff (file-get f))
		(set-value! (direction f? f) newval)
		(if (zero? newval) (status-set-z!) (status-unset-z!)))
	  (pc++)
	  #f)

	(define (decf f f?) ; Z
	  (let1 newval (logand #xff (- (file-get f) 1))
		(set-value! (direction f? f) newval)
		(if (zero? newval) (status-set-z!) (status-unset-z!)))
	  (pc++)
	  #f)

	(define (decfsz f f?)
	  (let1 newval (logand #xff (- (file-get f) 1))
;	   (let1 newval (logand #xff (- (u8vector-ref file-regs (actual-addr f)) 1))
		(set-value! (direction f? f) newval)
		(if (zero? newval)
			(begin (pc+=2);(set! pc (+ pc 2)) ; skip if zero
				   wait-1-cycle-proc)
			(begin (pc++)
				   #f))))
	
	(define (incf f f?) ; Z
	  (let1 newval (logand #xff (+ (file-get f) 1))
		(set-value! (direction f? f) newval)
		(if (zero? newval) (status-set-z!) (status-unset-z!)))
	  (pc++)
	  #f)

	(define (incfsz f f?)
	  (let1 newval (logand #xff (+ (file-get f) 1))
		(set-value! (direction f? f) newval)
		(if (zero? newval)
			(begin (pc+=2);(set! pc (+ pc 2)) ; skip if zero
				   wait-1-cycle-proc)
			(begin (pc++)
				   #f))))

	(define (iorwf f f?) ; Z
	  (let1 newval (logior w-reg (file-get f))
		(set-value! (direction f? f) newval)
		(if (zero? newval) (status-set-z!) (status-unset-z!)))
	  (pc++)
	  #f)
	
	(define (movf f f?) ; Z
	  (let1 newval (file-get f)
		(set-value! (direction f? f) newval)
		(if (zero? newval) (status-set-z!) (status-unset-z!)))
	  (pc++)
	  #f)

	(define (movwf f)
										;				(set-value! f w-reg)
	  (file-set! f w-reg)
	  (pc++)
	  #f)

	(define (nop)
	  (pc++)
	  #f)
	
	(define (rlf f f?) ; C
	  (let1 oldval (file-get f)
;		 (let1 newval (logand #xff (ash oldval 1))
		(let1 newval (logand #xff (+ oldval oldval))
		  (when (status-c?) (set! newval (logior #x1 newval)))
		  ;;					(printf "RLF: %x -> %x (%d)\n"
		  ;;							oldval newval (if (logbit? 7 oldval) 1 0))
		  (set-value! (direction f? f) newval)
		  (if (logbit? 7 oldval) (status-set-c!) (status-unset-c!))
		  ))
	  (pc++)
	  #f)

	(define (rrf f f?) ;; >>> C
	  (let1 oldval (file-get f)
		(let1 newval (ash oldval -1)
		  (when (status-c?) (set! newval (logior #x80 newval)))
		  ;;					(printf "RRF: %x -> %x (%d)\n"
		  ;;							oldval newval (if (logbit? 0 oldval) 1 0))
		  (set-value! (direction f? f) newval)
		  (if (logbit? 0 oldval) (status-set-c!) (status-unset-c!))
		  ))
	  (pc++)
	  #f)

	(define (subwf f f?) ;; >>> C DC Z
	  (let ([old-w w-reg]
			[old-f (file-get f)])
		(let1 newval (- old-f old-w)
		  (set-value! (direction f? f) (logand #xff newval))
		  (if (< newval 0)
			  (status-unset-c!) (status-set-c!))
		  (if (= 0 (logand #xff newval))
			  (status-set-z!) (status-unset-z!))
		  (if (< (- (logand #x0f old-f) (logand #x0f old-w)) 0)
;		   (if (and (< (if f? old-f old-w) 16)
;					(<= 16 newval))
			  (status-unset-dc!) (status-set-dc!))
		  ))
	  (pc++)
	  #f)

	(define (swapf f f?)
	  (let1 oldval (file-get f)
;		 (let1 newval (logior (ash oldval -4)
		(let1 newval (+ (ash oldval -4)
						(* (logand #xf oldval) 16))
		  ;;(ash (logand #xf oldval) 4))
		  (set-value! (direction f? f) newval)))
	  (pc++)
	  #f)

	(define (xorwf f f?) ; Z
	  (let1 newval (logxor w-reg (file-get f))
		(set-value! (direction f? f) newval)
		(if (zero? newval) (status-set-z!) (status-unset-z!)))
	  (pc++)
	  #f)

	;; bit
	(define (bcf f b)
	  (file-set-bit! f b 0)
	  (pc++)
	  #f)

	(define (bsf f b)
	  (file-set-bit! f b 1)
	  (pc++)
	  #f)

	(define (btfsc f b)
	  (if (= 0 (file-get-bit f b))
		  (begin (pc+=2) ;(set! pc (+ pc 2)) ; skip if clear
				 wait-1-cycle-proc)
		  (begin (pc++)
				 #f)))
	
	(define (btfss f b)
	  (if (= 1 (file-get-bit f b))
		  (begin (pc+=2) ;(set! pc (+ pc 2)) ; skip if set
				 wait-1-cycle-proc)
		  (begin (pc++)
				 #f)))
	
	;; literal, call/goto, other
	(define (addlw k) ; C DC Z
	  (let1 oldval w-reg
		(let1 newval (+ oldval k)
		  (w-set! (logand #xff newval))
		  (if (< 255 newval)
			  (status-set-c!) (status-unset-c!))
		  (if (<= 16 (+ (logand #x0f oldval) (logand #x0f k)))
			  (status-set-dc!) (status-unset-dc!))
		  (if (= 0 (logand #xff newval))
			  (status-set-z!) (status-unset-z!))
		  ))
	  (pc++)
	  #f)
	
	(define (andlw k) ; Z
	  (let1 oldval w-reg
		(let1 newval (logand oldval k)
		  (w-set! (logand #xff newval))
		  (if (= 0 (logand #xff newval))
			  (status-set-z!) (status-unset-z!))
		  ))
	  (pc++)
	  #f)
	
	(define (call k)
	  ;;(let1 long-addr (logior (ash (logand #b00011000 (file-get PCLATH)) 8)
;	   (let1 long-addr (logior (* (logand #b00011000 (file-get PCLATH)) 256)
	  (let1 long-addr (+ (* (logand #b00011000 (file-get PCLATH)) 256)
						 (logand #x07ff k))
		(do-call long-addr))
	  wait-1-cycle-proc)
	
	(define (clrwdt) ;; >>> ^TO, ^PD
	  (status-set! (logior #b00011000 ; ^TO=1,^PD=1
						   (logand #b11100111 (status-get))))
	  (pc++)
	  #f)

	(define (goto k)
;	   (let1 long-addr (logior (ash (logand #b00011000 (file-get PCLATH)) 8)
;	   (let1 long-addr (logior (* (logand #b00011000 (file-get PCLATH)) 256)
	  (let1 long-addr (+ (* (logand #b00011000 (file-get PCLATH)) 256)
						 (logand #x07ff k))
		 ; (do-call long-addr)
		(if (= long-addr pc)
			;; (GOTO $) でhaltする仕様にしてある
			(begin (set! halt-stat #t)
				   #f)
			(begin (pc= long-addr);(set! pc long-addr)
				   wait-1-cycle-proc))))
	
	(define (iorlw k) ; Z
	  (let1 oldval w-reg
		(let1 newval (logior oldval k)
		  (w-set! (logand #xff newval))
		  (if (= 0 (logand #xff newval))
			  (status-set-z!) (status-unset-z!))
		  ))
	  (pc++)
	  #f)
	
	(define (movlw k)
	  (w-set! k)
	  (pc++)
	  #f)

	(define (retfie)
	  (file-set-bit! INTCON 7 1)
										;	   (set! pc (tos))
	  (pc= (tos))
	  wait-1-cycle-proc)

	(define (retlw k)
	  (w-set! k)
	  (do-return)
	  wait-1-cycle-proc)
	
	(define (return)
	  (do-return)
	  wait-1-cycle-proc)

	(define (sleep) ; ^TO, ^PD
	  (status-set! (logior #b00010000 ; ^TO=1,^PD=0
						   (logand #b11100111 (status-get))))
	  (set! sleep-stat #t)
	  (pc++)
;	  (thread-yield!)
;	  (thread-yield!)
;	  (thread-yield!)
;	  (thread-yield!)
	  #f)
	
	(define (sublw k) ; C DC Z
	  (let1 oldval w-reg
		(let1 newval (- k oldval)
		  (w-set! (logand #xff newval))
		  (if (< newval 0)
			  (status-unset-c!) (status-set-c!)) ;; c = ^borrow
		  (if (< (- (logand #x0f k) (logand #x0f oldval)) 0)
			  (status-unset-dc!) (status-set-dc!)) ;; ^dc = half borrow
		  (if (= 0 (logand #xff newval))
			  (status-set-z!) (status-unset-z!))
		  ))
	  (pc++)
	  #f)
	
	(define (xorlw k) ;; >>> Z
	  (let1 oldval w-reg
		(let1 newval (logxor oldval k)
		  (w-set! (logand #xff newval))
		  (if (= 0 (logand #xff newval))
			  (status-set-z!) (status-unset-z!))
		  ))
	  (pc++)
	  #f)

	;;
	;; デバッグ用インストラクション
	;;
;	 (define (delete-led-garbage) (printf "            \r"))
	(define (print-pc) (printf "[%04x] " pc))
	 
	(define (debug-print-file-reg f)
	  (print-pc)
	  (printf "F<%02x> = 0x%02x\n" f (file-get f)))
	
	(define (debug-print-w-reg)
	  (print-pc)
	  (printf "W = %02x\n" w-reg))
	
	(define (display-pc-and-mnemonic-at addr)
	  (let1 pc-kept pc
		(set! pc addr)
		(print-pc)
		(printf "%s\n" (mnemonic-pp (mnemonic-at-pc)))
		(set! pc pc-kept)))
	
	(define (debug-print-pc)
	  (print-pc) (newline))

	(define (debug-print-nextinst)
	  (display-pc-and-mnemonic-at (+ pc 1)))
	
	(define (debug-print-snapshot)
	  (print "----------------------------------------------------------------------------------------------------------")
	  (display-pc-and-mnemonic-at (- pc 1))
	  (dump-snapshot (snapshot) 'full-mode)
	  (display-pc-and-mnemonic-at (+ pc 1))
	  (print "----------------------------------------------------------------------------------------------------------")
	  )
	
	(define (debug-print-scm-w)
	  (print-pc)
	  ;;	   (printf "W = %02x...\n" w-reg))
	  (printf "W = %02x = %s\n" w-reg (scmval->string w-reg)))
	(define (debug-print-scm-w-pp)
	  (printf "%s\n" (scmval->string w-reg)))

	(define (debug-print-scm-stack)
	  (print-pc)
	  (let1 top-of-stack (file-get #x25) ;; これは可変
		(printf "stack: %d %s\n" (- top-of-stack #x37)
				(map (lambda (addr)
					   (let1 sv (file-get addr)
						 (sprintf "%02x:%02x=%s" addr sv (scmval->string sv))))
					 (reverse! (iota (- top-of-stack #x37) #x38)))
				)))

	(define (scm-null? obj) (= #x4e obj))
	(define (scm-pair? obj) (= 0 (logand #b11 obj))) ;; cons ------00
	(define (scm-lref? obj) (= 1 (logand #b11 obj))) ;; lref ------01
	(define (scm-int16? obj) (= #b010 (logand #b111 obj))) ;; int16 -----010
	(define (scm-lambda? obj) (= #b0110 (logand #b1111 obj))) ;; proc ----0110
	(define (scm-continuation? obj) (= #b11110110 obj)) ;; proc 11110110
	(define (scm-misc-obj? obj) (= #b01110 (logand #b11111 obj))) ;; misc ---01110
	
	(define (scm-car pair)
	  (let1 ofs (ash pair -2)
		(file-get (+ #x0A0 ofs))
		))
	(define (scm-cdr pair)
	  (let1 ofs (ash pair -2)
		(file-get (+ #x120 ofs))
		))

	(define (scm-proc? obj)
	  (and (scm-pair? obj) (scm-lambda? (scm-car obj))))
	(define (scm-proc-arity proc)
	  (ash (scm-car proc) -4))
	(define (scm-proc-entrypoint proc)
	  (scm-car (scm-cdr proc)))
	(define (scm-proc-env proc)
	  (scm-cdr (scm-cdr proc)))

	(define (scm-pair->string sv)
	  (let1 ofs (ash sv -2)
		(let ([ca_ (scm-car sv)];(file-get (+ #x0A0 ofs))]
			  [cd_ (scm-cdr sv)]);(file-get (+ #x120 ofs))])
		  (if (scm-null? cd_)
			  (scmval->string ca_)
			  (string-append (scmval->string ca_) " "
							 (if (scm-proc? cd_)
								 (scm-proc->string cd_)
								 (if (scm-pair? cd_)
									 (scm-pair->string cd_)
									 #`" . ,(scmval->string cd_)"
									 )))))))
	(define (scm-proc->string sv)
	  (if (= 15 (scm-proc-arity sv)) ;(scm-continuation? sv)
		  (sprintf "#<continuation:#%04x:%02x>"
				   (scm-proc-entrypoint sv)
				   (scm-proc-env sv) )
		  (sprintf "#<λ%d:%04x:%02x>"
				   (scm-proc-arity sv)
				   (scm-proc-entrypoint sv)
				   (scm-proc-env sv) )
		  ))
	(define (scmval->string sv)
	  (cond
	   [(scm-proc? sv) ; = (logand #b1111 sv) #b0110) ; ----0110
		(scm-proc->string sv)]
	   [(scm-pair? sv) ; = (logand #b11 sv) 0) ;; cons ------00
		(string-append "(" (scm-pair->string sv) ")")]
	   [(scm-lref? sv) ; = (logand #b11 sv) #b01) ;; lref ------01
		(let ([dep (logand #b111 (ash sv -5))]
			  [pos (logand #b111 (ash sv -2))])
		  (sprintf "<%d,%d>" dep pos))]
	   [(scm-int16? sv) ; = (logand #b111 sv) #b010) ;; int16
		(let1 ofs (ash sv -3)
		  (let1 val (+ (* (file-get (+ #xD0 ofs)) 256)
					   (file-get (+ #x150 ofs)))
			(if (<= val 32767)
				(sprintf "%d" val)
				(sprintf "%d" (- val 65536))) ))]
	   [(scm-misc-obj? sv) ; = (logand #b11111 sv) #b01110)
		(case (ash sv -5)
		  [(0) "#f"]
		  [(1) "#t"]
		  [(2) "()"]
		  [(3) "#<eof>"]
		  [(4) "#<undef>"] 
		  [(5) "#<unbound>"])]
	   [else
		(sprintf "%02x" sv)]
	   ))
	(define (debug-print-scm-pairs)
	  (print-pc)
	  (let1 pair-addr (file-get #x27) ;; これは可変
		(printf "pairs: %d %s\n" (- pair-addr #x1f)
				(map (lambda (ofs)
					   (let1 sv (ash ofs 2)
						 (sprintf "%02x:%s" sv (scmval->string sv))
						 ))
					 (iota (- pair-addr #x1f)))
				)))
	(define (debug-print-scm-int16s)
	  (print-pc)
	  (let1 int16-addr (file-get #x26) ;; int16-addr: これは可変
		(printf "int16: %d %s\n" (- int16-addr #x4f)
				(map (lambda (ofs)
					   (let1 sv (logior (ash ofs 3) #b010)
						 (sprintf "%02x:%s" sv (scmval->string sv))
						 ))
										;					  (iota (+ 5 (- int16-addr #x4f))))
					 (iota (- int16-addr #x4f)))
				)))
	
	;;
	;;
	(define (compile-inst inst)
	  ;; まず上位３ビットで切り分け
	  (case (ash inst -12)
		[(0) ; byte
		 (let ([op (ash inst -8)]      ; (bit-field inst  8 12)
			   [f? (logbit? 7 inst)]   ; (bit-field inst  7  8)
			   [f (logand #x7f inst)]) ; (bit-field inst  0  7)
		   ;; (printf "(byte) %04b d=%d f=%02x" op (if f? 'F 'W) f)
		   (case op
			 [(#b0000)
			  (if f?
				  ;; 00 0000 1fff ffff - MOVWF
				  (values (lambda () (movwf f)) `(MOVWF ,f))
				  (if (= 0 (logand #b11111110011111 inst))
					  ;; 00 0000 0xx0 0000 - NOP
					  (values (lambda () (nop)) '(NOP))
					  (case inst
						[(#b1100100) (values (lambda () (clrwdt)) '(CLRWDT))]
						[(#b0001001) (values (lambda () (retfie)) '(RETFIE))]
						[(#b0001000) (values (lambda () (return)) '(RETURN))]
						[(#b1100011) (values (lambda () (sleep)) '(SLEEP))]
						[else (values (lambda () #f) '(---))]
						)))]
			 [(#b0111) (values (lambda () (addwf f f?)) `(ADDWF ,f ,(if f? 'F 'W)))] ; C DC Z
			 [(#b0101) (values (lambda () (andwf f f?)) `(ANDWF ,f ,(if f? 'F 'W)))] ; Z
			 [(#b0001) (if f?
						   (values (lambda () (clrf f)) `(CLRF ,f)) ; Z
						   (values (lambda () (clrw)) '(CLRW)) ; Z
						   )]
			 [(#b1001) (values (lambda () (comf f f?)) `(COMF ,f ,(if f? 'F 'W)))] ; Z
			 [(#b0011) (values (lambda () (decf f f?)) `(DECF ,f ,(if f? 'F 'W)))] ; Z
			 [(#b1011) (values (lambda () (decfsz f f?)) `(DECFSZ ,f ,(if f? 'F 'W)))]
			 [(#b1010) (values (lambda () (incf f f?)) `(INCF ,f ,(if f? 'F 'W)))] ; Z
			 [(#b1111) (values (lambda () (incfsz f f?)) `(INCFSZ ,f ,(if f? 'F 'W)))]
			 [(#b0100) (values (lambda () (iorwf f f?)) `(IORWF ,f ,(if f? 'F 'W)))]
			 [(#b1000) (values (lambda () (movf f f?)) `(MOVF ,f ,(if f? 'F 'W)))]
			 [(#b1101) (values (lambda () (rlf f f?)) `(RLF ,f ,(if f? 'F 'W)))]
			 [(#b1100) (values (lambda () (rrf f f?)) `(RRF ,f ,(if f? 'F 'W)))]
			 [(#b0010) (values (lambda () (subwf f f?)) `(SUBWF ,f ,(if f? 'F 'W)))]
			 [(#b1110) (values (lambda () (swapf f f?)) `(SWAPF ,f ,(if f? 'F 'W)))]
			 [(#b0110) (values (lambda () (xorwf f f?)) `(XORWF ,f ,(if f? 'F 'W)))]
			 ))]
		[(1) ; bit
		 (let ([op (logand 3 (ash inst -10))]  ; (bit-field inst 10 12)
			   [b (logand 7 (ash inst -7))]    ; (bit-field inst  7 10)
			   [f (logand #x7f inst)])         ; (bit-field inst  0  7)
		   (case op
			 [(0) (values (lambda () (bcf f b)) `(BCF ,f ,b))]
			 [(1) (values (lambda () (bsf f b)) `(BSF ,f ,b))]
			 [(2) (values (lambda () (btfsc f b)) `(BTFSC ,f ,b))]
			 [(3) (values (lambda () (btfss f b)) `(BTFSS ,f ,b))]
			 ))]
		[(2) ; goto / call
		 (let ([op (logbit? 11 inst)]          ; (bit-field inst 11 12)
			   [k (logand #x7ff inst)])        ; (bit-field inst  0 11)
		   (if op
			   (values (lambda () (goto k)) `(GOTO ,k))
			   (values (lambda () (call k)) `(CALL ,k))
			   ))]
		[(3) ; literal
		 (let ([op (logand #xf (ash inst -8))] ; (bit-field inst  8 12)
			   [k (logand #xff inst)])         ; (bit-field inst  0  8)
		   (case op
			 [(#b1110 #b1111) (values (lambda () (addlw k)) `(ADDLW ,k))] ; C DC Z
			 [(#b1001) (values (lambda () (andlw k)) `(ANDLW ,k))] ; Z
			 [(#b1000) (values (lambda () (iorlw k)) `(IORLW ,k))] ; Z
			 [(#b0000 #b0001 #b0010 #b0011)
			  (values (lambda () (movlw k)) `(MOVLW ,k))]
			 [(#b0100 #b0101 #b0110 #b0111)
			  (values (lambda () (retlw k)) `(RETLW ,k))]
			 [(#b1100 #b1101) (values (lambda () (sublw k)) `(SUBLW ,k))] ; C DC Z
			 [(#b1010) (values (lambda () (xorlw k)) `(XORLW ,k))] ; C DC Z
			 [else ;; #b11 1011 xxxx xxxx
			  ;; ここにデバッグプリント命令とかブレークポイントとか入れたい
			  (let1 arg (logand #x7f inst)
				(if (logbit? 7 inst) ; 1fff ffff
					(values (lambda ()
							  (debug-print-file-reg arg)
							  (pc++)
							  #f)
							`(DEBUG:file ,arg))
					(case arg
					  [(0) (values (lambda ()
									 (debug-print-w-reg)
									 (pc++)
									 #f)
								   `(DEBUG:w))]
					  [(#b1111111) (values (lambda ()
											 (pc++)
											 (pc++)
											 #f)
										   `(DEBUG:skip-if-emulator))]
					  [(#b1111) (values (lambda ()
										  (debug-print-snapshot)
										  (pc++)
										  #f)
										`(DEBUG:snapshot))]
					  [(#b0001) (values (lambda ()
										  (debug-print-pc)
										  (pc++)
										  #f)
										`(DEBUG:pc))]
					  [(#b0010) (values (lambda ()
										  (debug-print-nextinst)
										  (pc++)
										  #f)
										`(DEBUG:nextinst))]
					  [(#b10000) (values (lambda ()
										   (debug-print-scm-w)
										   (pc++)
										   #f)
										 `(DEBUG:scm:w))]
					  [(#b10001) (values (lambda ()
										   (debug-print-scm-w-pp)
										   (pc++)
										   #f)
										 `(DEBUG:scm:w))]
					  [(#b10010) (values (lambda ()
										   (debug-print-scm-stack)
										   (pc++)
										   #f)
										 `(DEBUG:scm:stack))]
					  [(#b10011) (values (lambda ()
										   (debug-print-scm-pairs)
										   (pc++)
										   #f)
										 `(DEBUG:scm:pairs))]
					  [(#b10100) (values (lambda ()
										   (debug-print-scm-int16s)
										   (pc++)
										   #f)
										 `(DEBUG:scm:int16s))]
					  [else (values (lambda ()
									  (pc++)
									  #f)
									(sprintf "(reserved) 11 1011 k=%02x"
											 (logand #xff inst)))
							])))]
			 ))]
		))
	
	(define (instruction-at-pc) (u16vector-ref program-mem (remainder pc *program-memory-size*)))
	(define (proc-at-pc) (vector-ref precompiled-inst-proc (remainder pc *program-memory-size*)))
	(define (mnemonic-at-pc) (vector-ref precompiled-inst-mnemonic (remainder pc *program-memory-size*)))
	
	
	;; addr番地の命令を読み、必要な処理をthunkにして返す。
	;; メモ化するなりプリコンパイルするなりしたい。
	(define (precompile-inst-at addr)
	  (let1 inst (u16vector-ref program-mem (remainder addr *program-memory-size*))
		(receive (proc mnemonic) (compile-inst inst)
		  ;;		  (printf "preompile inst at %x, %x => %s\n" addr inst mnemonic)
		  (vector-set! precompiled-inst-proc addr proc)
		  (vector-set! precompiled-inst-mnemonic addr mnemonic)
		  )))
	
	;; クロック信号が来たら
	(define (clock)
	  (unless (or sleep-stat halt-stat)
		(if second-proc
			;; 前の命令の残りがある場合
			(begin
			  (second-proc)
			  (set! second-proc #f))
			;; ない場合
			(let1 inst-proc (vector-ref precompiled-inst-proc (remainder pc *program-memory-size*))
			  ;;			 (let1 inst-proc (vector-ref precompiled-inst-proc pc)
			  (if (undefined? inst-proc)
				  (set! halt-stat #t) ;; no more instructions
				  (set! second-proc (inst-proc))
				  )))
;		 (when (>= pc *program-memory-size*) (set! pc 0))
		))

	(define (load-program offset-addr prog)
	  (u16vector-copy! program-mem offset-addr prog)
	  (for-each precompile-inst-at
				(iota (u16vector-length prog) offset-addr)))
	
	(define (poke-program-mem addr inst)
	   (u16vector-set! program-mem addr inst)
	   (precompile-inst-at addr))

	(define (load-hex-file hex-file)
	  (define (load-hex-record line)
		(match-let1 (record-type offset-addr data) (intel-hex-record line)
					(case record-type
					  ((0) (if (< offset-addr #x1000)
							   (load-program offset-addr (list->u16vector data))
										;;
							   ))
					  (else #f) )))
	  (with-input-from-file hex-file
		(lambda () (port-map load-hex-record read-line))))

	(lambda (m)
	  (case m
		[(snapshot) (snapshot)]
		
		[(on) (set! power-stat #t) (por-reset)]
		[(off) (set! power-stat #f)]
		[(on?) power-stat]
		
		[(reset) (other-reset)]
		[(step) (clock)]
		
		[(go-to-sleep) (set! sleep-stat #t)]
		[(wake-up) (set! sleep-stat #f)]
		[(sleep?) sleep-stat]
		
		[(halt) (set! halt-stat #t)]
		[(unhalt) (set! halt-stat #f)]
		[(halt?) halt-stat]
		
		[(load-program) load-program]
		[(write-inst-at-pc) (lambda (inst) (poke-program-mem pc inst))]
		[(interpret)
		 ;;プログラムメモリには書き込まずに実行。
		 ;;PCは通常通りに変化する
		 (lambda (inst)
		   (receive (proc mnemonic) (compile-inst inst)
										;(vector-set! precompiled-inst-proc addr proc)
										;(vector-set! precompiled-inst-mnemonic addr mnemonic)
			 (proc))
		   )]

		[(load-hex-file) load-hex-file]
		[(load-config) (lambda (config) )]

		[(pc) pc]
		[(pc-set!) pc=];(lambda (addr) (set! pc addr))]
										;		 [(goto) (lambda (addr) (set! pc addr))]

										; for monitoring
		[(inst) (instruction-at-pc)]
		[(proc) (proc-at-pc)]
		[(mnemonic) (mnemonic-at-pc)]
		[(finished?) (not second-proc)]
		[(next-inst) (if second-proc
						 (values #f '(--))
						 (proc-at-pc))]
		
		[(call-stack)
		 (reverse! (take (u16vector->list call-stack) call-stack-head))]
		[(tos) (tos)]
										;		[(call-stack-head) call-stack-head]

										; raw memory
		[(w w-reg) w-reg]
		[(file-regs) file-regs]
		
		[(w-set!) w-set!]
		[(f f-reg file) file-get]
		[(f-set! file-set!) file-set!]
		[(f-bit file-bit) file-get-bit]
		[(f-bit-set! file-bit-set!) file-set-bit!]
		
		[(status) (status-get)]
		[(status-set!) status-set!]
		[(c C) (status-c?)]
		[(c-set!) (status-set-c!)]
		[(c-unset!) (status-unset-c!)]
		[(dc DC) (status-dc?)]
		[(dc-set!) (status-set-dc!)]
		[(dc-unset!) (status-unset-dc!)]
		[(z Z) (status-z?)]
		[(z-set!) (status-set-z!)]
		[(z-unset!) (status-unset-z!)]

		[(program program-mem) program-mem]

										; raw instructions
		[(addwf) addwf]
		[(andwf) andwf]
		[(clrf) clrf]
		[(clrw) clrw]
		[(comf) comf]
		[(decf) decf]
		[(decfsz) decfsz]
		[(incf) incf]
		[(incfsz) incfsz]
		[(iorwf) iorwf]
		[(movf) movf]
		[(movwf) movwf]
		[(nop) nop]
		[(rlf) rlf]
		[(rrf) rrf]
		[(subwf) subwf]
		[(swapf) swapf]
		[(xorwf) xorwf]
		[(bcf) bcf]
		[(bsf) bsf]
		[(btfsc) btfsc]
		[(btfss) btfss]
		[(addlw) addlw]
		[(andlw) andlw]
		[(call) call]
		[(clrwdt) clrwdt]
		[(goto) goto]
		[(iorlw) iorlw]
		[(movlw) movlw]
		[(retfie) retfie]
		[(retlw) retlw]
		[(return) return]
		[(sleep) sleep]
		[(sublw) sublw]
		[(xorlw) xorlw]
		))))
;; end

;;
;; dump a snapshot table
;;
(define (dump-snapshot snapshot . mode)
  (let ([name (first snapshot)]
		[w-reg (second (assoc 'w-reg snapshot))]
		[file-regs (second (assoc 'file-regs snapshot))]
		[program (second (assoc 'program snapshot))]
		[pc (second (assoc 'pc snapshot))]
		[call-stack (second (assoc 'call-stack snapshot))]
		[config (second (assoc 'config snapshot))]
		[eeprom (second (assoc 'eeprom snapshot))]
		[power (second (assoc 'power snapshot))]
		[full-mode (if (null? mode) #f #t)]
		)

	(define (hex at)
	  (let1 masked (logand #x7f at)
		(cond
		 [(= 0 masked) ; INDF
		  (let1 FSR-value (u8vector-ref file-regs 4)
			(if (= 0 FSR-value)
				(sprintf "%2x"
						 (u8vector-ref file-regs 0))
				(hex FSR-value))
			)]
		 [(memq at '(8 9 #x1b
					   #x88 #x89 #x91 #x9c #x9d
					   #x108 #x109 #x110 #x111 #x112 #x113 #x114 #x117 #x11c #x11d
					   #x188 #x189 #x18e #x18f #x190 #x191 #x192 #x193 #x194 #x195
					   #x196 #x197 #x198 #x199 #x19a #x19b #x19c #x19f))
		  ".."]
		 [(<= #x1a0 at #x1ef)
		  ".."]
		 [(and (< #x80 at)
			   (or (<= #x70 masked)
				   (memq masked '(2 3 4 #xa #xb))))
		  "< "] ;(hex masked)]
		 [(and (<= #x100 at) (<= masked #x0b))
		  (hex (logand #xff at))]
		 [else
		  (sprintf "%2x" (u8vector-ref file-regs at))]
		 )))

	(if full-mode
		(begin
;		  (printf "  W: %02x\n" w-reg)
		  (printf "(%02x) +0 +1 +2 +3 +4 +5 +6 +7 +8 +9 +A +B +C +D +E +F  " w-reg)
		  (printf "     +0 +1 +2 +3 +4 +5 +6 +7 +8 +9 +A +B +C +D +E +F\n")
		  (for-each (lambda (h)
					  (let1 base (ash h 4)
						(printf "%03x: %s %s  %03x: %s %s\n"
								base
								(string-join (map hex (iota 8 base)) " ")
								(string-join (map hex (iota 8 (+ base 8))) " ")
								(+ base #x100)
								(string-join (map hex (iota 8 (+ base #x100))) " ")
								(string-join (map hex (iota 8 (+ base #x108))) " ")
						  )))
					(iota 16)
					))
		(begin
;		  (printf "  W: %02x\n" w-reg)
		  (printf "(%02x) +0 +1 +2 +3 +4 +5 +6 +7 +8 +9 +A +B +C +D +E +F\n" w-reg)
		  (for-each (lambda (h)
					  (let1 base (ash h 4)
						(unless (apply = (map (cut u8vector-ref file-regs <>)
											  (iota 16 base)))
						  (printf "%03x: %s %s\n"
								  base
								  (string-join (map hex (iota 8 base)) " ")
								  (string-join (map hex (iota 8 (+ base 8))) " ")
								  ))))
					(iota 32)
					))
		)))

(define (mnemonic-pp mnemonic)
  (if (pair? mnemonic)
	  (string-append (x->string (car mnemonic))
					 (if (<= 2 (length mnemonic))
						 (let1 arg1 (cadr mnemonic)
						   (string-append
							(if (number? arg1)
								(sprintf " %x" arg1)
								(string-append " " (x->string arg1)))
;								(sprintf " %s" arg1))
							(if (<= 3 (length mnemonic))
								(let1 arg2 (caddr mnemonic)
								  (if (number? arg2)
									  (sprintf ",%d" arg2)
									  (string-append "," (x->string arg2))))
;									  (sprintf ",%s" arg2)))
								"")
							))
						 ""))
	  "***"))
