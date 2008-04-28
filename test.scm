(require "./ikoma-pic-scheme")

(define (read-program-from-file fname)
  (with-input-from-file fname
	(lambda ()
	  (let loop ((program '()))
		(let1 s (read)
		  (if (eof-object? s)
			  (reverse! program)
			  (loop (cons s program))))))))

(define (main args)
  (let1 program (read-program-from-file (cadr args))
	(print "---------")
	(map print program)
	(print "---------")
	(ips program #f #f #f #f)
	;;(ips program #t #t #f #f)
	))
