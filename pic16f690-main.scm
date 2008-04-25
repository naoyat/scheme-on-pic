(require "./pic16f690-emulator")

(define cnt 0)
(define sometimes #f)
;(define sometimes 1)
;(define sometimes 1)
(define verbose-mode #f)
;(define verbose-mode #t)

(define (4bit val)
  (string-append (if (logbit? 3 val) "● " "○ ")
				 (if (logbit? 2 val) "● " "○ ")
				 (if (logbit? 1 val) "● " "○ ")
				 (if (logbit? 0 val) "●" "○")))
;  (string-join (map (lambda (b)
;					  (if (logbit? b val) "●" "○"))
;					'(3 2 1 0))
;			   " "))

;; ([pic'load-program] #u16(#x1683 #x1007 #x1283 #x1407 #x0ba0 #x2804 #x0ba1 #x2804 #x1007 #x0ba0 #x2809 #x0ba1 #x2809 #x2803))

(define (main args)
  (if (null? (cdr args))
	  (format (current-error-port) "usage: ~a hex-file\n" *program-name*)
	  (let1 hex-file (second args)
		(let1 pic (make-pic16f690)
		  ([pic'load-hex-file] hex-file)
		  (print "load hex-file completed")

		  ;; run
		  [pic'on]

		  (let loop ([last-portc -1])
			(when (or verbose-mode (and sometimes (zero? (remainder cnt sometimes))))
			  (if [pic'finished?]
				  (begin (printf "> %s\n" (4bit last-portc))
						 (dump-snapshot [pic'snapshot])
						 
						 (printf "%08x: %s  ;; %s\n"
								 [pic'pc] (mnemonic-pp [pic'mnemonic])
								 (map (cut sprintf "%04x" <>) [pic'call-stack])
								 ))
				  (print "// waiting 1 cycle... //")
				  ))

;			(profiler-reset)
;			(profiler-start)
			[pic'step]
;			(profiler-stop)
;			(profiler-show)

			(inc! cnt)

			(let1 portc (logand #b1111 ([pic'f] 7))
			  (unless (= portc last-portc)
;				(display (if (logbit? 3 portc) "> ●" "> ○"))
;				(display (if (logbit? 2 portc) " ●" " ○"))
;				(display (if (logbit? 1 portc) " ●" " ○"))
;				(display (if (logbit? 0 portc) " ●\n" " ○\n"))
;				(format #t "> ~a\r" (4bit portc))
				(printf "> %s\r" (4bit portc))
;				(print portc)
;				(display "> ")
;				(display (4bit portc))
;				(display "\r")
				(flush)
				(inc! cnt))

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
  0)
