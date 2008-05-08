#!/usr/bin/env gosh

(require "./ikoma-pic-scheme")
(require "./pic-device")

(use gauche.parseopt)

(define (read-program-from-file path)
  (with-input-from-file path
	(lambda ()
	  (let loop ((program '()))
		(let1 s (read)
		  (if (eof-object? s)
			  (reverse! program)
			  (loop (cons s program))))))))

(define (main args)
  (let-args (cdr args)
	  ((show-source-code "s|source-code" #f)
	   (show-macro-asm   "m|macro-asm" #f)
	   (show-asm         "a|asm" #f)
	   (show-util-code   "u|show-util-code" #f)
	   (show-disasm      "d|disasm" #f)
	   (run-on-gauche    "g|gauche|run-on-gauche" #f)
	   (disasm-on-gauche "x|disasm-on-gauche" #f)
	   (emulate          "e|emulate" #f)

	   (write            "w|write" #f)
	   (write-and-run    "r|write-and-run=i" #f)

;	   (else _    (error "uuu" _) (print _))
										;			 (error "unrecognized option:" options)))
	   . rest)
	
	(define (show-usage)
	  (print
"Usage: test.scm [-m|macro-asm] [-a|asm] [-u|show-util-code] [-d|disasm] [-e|emulate] [-w|write] [-r|write-and-run sec] test-num
options:
  -s, --source                    Show source code
  -m, --macro-asm                 Show macro-asm code
  -a, --asm                       Show asm code
  -u, --show-util-code            Show utility library code
  -d, --disasm                    Show disasm code
  -g, --gauche, --run-on-gauche   Run on Gauche
  -x, --disasm-on-gauche          Disasm on Gauche
  -e, --emulate                   Run on Emulator
  -w, --write                     Write to PIC (w/ pk2)
  -r, --write-and-run <seconds>   Write and Run on PIC (w/ pk2)

Report bugs to @naoya_t"))

	(define (existing-test-nums)
	  (reverse! (map (lambda (path) ((#/tests_and_demos\/t(\d+).scm/ path) 1))
					 (sys-glob "tests_and_demos/t*.scm"))))

	(let1 test-num (if (pair? rest) (car rest) #f)
	  (if (and test-num (or show-source-code show-macro-asm show-asm show-util-code show-disasm
							run-on-gauche disasm-on-gauche emulate
							write write-and-run))
		  (let1 path (format "tests_and_demos/t~d.scm" test-num)
			(if (file-exists? path)
				(let1 program (read-program-from-file path)
				  (when show-source-code
					(print "/// source code")
					(print "---------")
					(map print program)
					(print "---------"))
				  
				  (when (or run-on-gauche disasm-on-gauche)
					(print "/// Gauche")
					(when disasm-on-gauche
					  (let1 c (eval `(lambda () ,@program)
									(interaction-environment))
						(disasm c)))
					(when run-on-gauche
					  (eval (cons 'begin program)
							(interaction-environment))))
				  
				  (if (or write write-and-run)
					  ;; on pic
					  (begin
						(ips program show-macro-asm show-asm show-util-code show-disasm #f)
						(pic-off)
						(pic-write "_ipstest.hex")
						(when write-and-run
						  (pic-on)
						  (sys-sleep write-and-run)
						  (pic-off)))
					  ;; on emu
					  (begin
						(print "/// Scheme on PIC <emulator>")
										;					(when emulate
										;(print "/// Scheme on PIC"))
						(ips program show-macro-asm show-asm show-util-code show-disasm emulate))))
				;; file not found
				(print (existing-test-nums))
				))
		  ;; no test num
		  (show-usage)))))

