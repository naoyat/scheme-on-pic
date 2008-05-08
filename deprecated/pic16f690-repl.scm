(require "./pic16f690-emulator")

(define (4bit val)
  (string-join (map (lambda (b) (if (logbit? b val) "●" "○"))
					'(3 2 1 0))
			   " "))

(define (main args)
  (let1 pic (make-pic16f690)

	(define (load-hex-file hex-file)
	  ([pic'load-hex-file] hex-file)
	  (print "load hex-file completed"))

	(define (run)
	  (let loop ([last-portc -1])

		[pic'step]
		
;		(inc! cnt)
		
		(let1 portc (logand #b1111 ([pic'f] 7))
		  (unless (= portc last-portc)
			(printf "> %s\r" (4bit portc))
			(flush))
										;		  (if verbose-mode
;			  1 2)
		  (unless [pic'halt?] (loop portc))
		  )))

	(define (repl)
	  (let1 input (read-line)
		(unless (eof-object? input)
		  (let1 cmd (read-from-string (string-append "(" input ")"))
			(unless (null? cmd)
			  (case (car cmd)
				[(load)
				 (load-hex-file (symbol->string (cadr cmd)))]
				
				[(on) [pic'on]]
				[(off) [pic'off]]
				[(reset) [pic'reset]]
				
				[(run) (run)]
				[(step) [pic'step]]
				[(pc) (print "pc=" [pic'pc])]
				
				[else 
				 (print "??.")
										;			   (print (format "~a" cmd))
				 ]
				))
;		  (cond
;		   [(symbol? cmd)
			(repl)))))

	(guard (e ;[(= (ref e 'signal) SIGINT)
			  ; (print "MOO")]
			  ;[(<unhandled-signal-error> e)
			  ; (print "UNHANDLED SIGNAL !!!")]
			  [(and (<unhandled-signal-error> e)  ; コンディションが<unhandled-signal-error>クラスであり
					(= (ref e 'signal) SIGINT))   ; シグナル番号がSIGINTである場合
										;	  (guard (e ((<unhandled-signal-error> e)  ; コンディションが<unhandled-signal-error>クラスであり
			   ;;					  (= (ref e 'signal) SIGINT))   ; シグナル番号がSIGINTである場合
			   (print "^C")
			   (print (if [pic'halt?] "WAITING" "RUNNING"))
			   (unless [pic'halt?]
				 [pic'halt])
;				 (set! running #f)
			   (repl)
			   ]
			  [else
			   (print "got ???")])             ; 「got SIGINT」を表示

			
	; load hex-file if specified
	(unless (null? (cdr args))
	  (load-hex-file (second args)))

	;; run
	[pic'on]

	(repl))))

