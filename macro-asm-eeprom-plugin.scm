(require "./macro-asm-plugin")

;;
;; [plug-in] eeprom
;;
(define (plug-in:eeprom)

  (make-plugin
   ;; registers to allocate
   '()
   ;; init code
   '()
   ;; subroutines
   `(
   eeprom-read
     (-- eeprom-read)
	 ; wにアドレス
	 (change-bank 0 2) ; bank2
	 (mov    EEADR w)
	 (change-bank 2 3) ; bank3
	 (BCF    EECON1 EEPGD)
	 (BSF    EECON1 RD)
;	 (BCF  EECON1 RD) ;;;;;
	 (change-bank 3 2) ; bank2
	 (mov    w EEDAT)
	 (change-bank 2 0) ; bank0
	 (RETURN)

   eeprom-write
     (-- eeprom-write)
	 (pop    FSR); EEADR
	 (change-bank 0 2) ; bank2
	 (mov    EEDAT w)
;	 (pop    EEADR)
	 (mov    EEADR FSR)
	 (change-bank 2 3) ; bank3
	 (BCF    EECON1 EEPGD)
	 (BSF    EECON1 WREN)

	 (BCF    INTCON GIE)
	 (mov    EECON2 #x55)
	 (mov    EECON2 #xAA)
	 (BSF    EECON1 WR)
	 (BSF    INTCON GIE)
	 
	 (SLEEP)
;	 (BCF EECON1 WR);;;;;
	 (BCF    EECON1 WREN)
	 (change-bank 3 0) ; bank0
	 (RETLW  ,scm-undefined)

   flash-program-read
	 (change-bank 0 2) ; bank2
	 ; (mov EEADRH high)
	 ; (mov EEADR  low)
	 (change-bank 2 3) ; bank3

	 (BSF    EECON1 EEPGD)
	 (BSF    EECON1 RD)
	 (NOP)
	 (NOP)
;	 (BCF EECON1 RD);;;;;

	 (change-bank 3 2) ; bank2
	 (mov    w EEDAT)
	 ;;//
	 (mov    w EEDATH)
	 (change-bank 2 0) ; bank0
	 (RETURN)
	 )

   (list 'eeprom-read (lambda () '((CALL eeprom-read))))
   (list 'eeprom-write (lambda () '((CALL eeprom-write))))
;   (list 'program-read (lambda () '((CALL program-read))))
   ))
