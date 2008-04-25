(require "./macro-asm-plugin")

;;
;; [plug-in] wait
;;
(define (plug-in:wait)

  (make-plugin
   ;; registers to allocate
   '(delay0 delay1 delay2)
   ;; init code
   '()
   ;; subroutines
   `((-- wait-100msec() #\: 100 msec loop)
	 wait-100msec
;	 (set     delay1 128)
	 (set     delay1 38) ;;;; FOR EMULATOR
	 (set     delay2 0)
	 loop-100msec
	 (dec-jnz delay2 loop-100msec)
	 (dec-jnz delay1 loop-100msec)
	 (RETURN)

	 (-- wait() #\: waiting W*100 msec)
	 wait
	 (MOVWF   delay0)
	 loop-wait
	 (CALL    wait-100msec)
	 (dec-jnz delay0 loop-wait)
	 (RETURN)
	 )

   (list 'wait
		 (lambda (sec)
		   (let1 unit (x->integer (* 10 sec))
			 `((MOVLW  ,unit)
			   (CALL   wait))
			 )))
   ))
