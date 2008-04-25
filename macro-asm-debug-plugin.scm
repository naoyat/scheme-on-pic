(require "./macro-asm-plugin")

;;
;; Debug
;;
(define (plug-in:debug)

  (make-plugin
   '()
   '()
   '()
   (list 'DEBUG:file
		 (lambda (f)
		   `((asm (DEBUG:file ,f)))
		   ))
   (list 'DEBUG:w
		 (lambda ()
		   `((asm (DEBUG:w)))
		   ))
   (list 'DEBUG:pc
		 (lambda ()
		   `((asm (DEBUG:pc)))
		   ))
   (list 'DEBUG:nextinst
		 (lambda ()
		   `((asm (DEBUG:nextinst)))
		   ))
   (list 'DEBUG:snapshot
		 (lambda ()
		   `((asm (DEBUG:snapshot)))
		   ))

   (list 'DEBUG:scm:w
		 (lambda ()
		   `((asm (DEBUG:scm:w)))
		   ))
   (list 'DEBUG:scm:w-pp
		 (lambda ()
		   `((asm (DEBUG:scm:w-pp)))
		   ))
   (list 'DEBUG:scm:stack
		 (lambda ()
		   `((asm (DEBUG:scm:stack)))
		   ))
   (list 'DEBUG:scm:pairs
		 (lambda ()
		   `((asm (DEBUG:scm:pairs)))
		   ))
   (list 'DEBUG:scm:int16s
		 (lambda ()
		   `((asm (DEBUG:scm:int16s)))
		   ))

   ))