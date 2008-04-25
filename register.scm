;;;
;;; registers
;;;
;;;   Copyright (c) 2008 naoya_t (naoya.t@aqua.plala.or.jp)
;;;

(use srfi-1)

(define (make-register-system)
  (let1 registers
	  (append (zip
			   '(INDF TMR0 PCL STATUS FSR PORTA PORTB PORTC
					  PORTD PORTE PCLATH INTCON PIR1 PIR2 TMR1L TMR1H
					  T1CON TMR2 T2CON SSPBUF SSPCON CCPR1L CCPR1H CCP1CON
					  RCSTA TXREG RCREG CCPR2L CCPR2H CCP2CON ADRESH ADCON0)
			   (iota #x20 0))
			  (zip
			   '(INDF OPTION_REG PCL STATUS FSR TRISA TRISB TRISC
					  TRISD TRISE PCLATH INTCON PIE1 PIE2 PCON -
					  - SSPCON2 PR2 SSPADD SSPSTAT - - -
					  TXSTA SPBRG - - CMCON CVRCON ADRESL ADCON1)
			   (iota #x20 #x080))
			  (zip
			   '(ANSEL ANSELH)
			   (iota 2 #x11e))
			  )

	(define (resolve reg)
	  (let1 c (assoc reg registers)
		(if c (second c) #f)))
	
	(define (address-available? addr)
	  (let1 taken-addresses (map second registers)
		(if (memq addr taken-addresses) #t #f)))
	
	(define (alloc reg range)
	  (if (resolve reg)
		  (error "Register already taken -- REGISTER" reg)
		  (let1 available-range (remove address-available? range)
			(if (null? available-range)
				(error "Address not available in range -- REGISTER" range)
				(let1 addr (car available-range)
				  (push! registers (list reg addr))
				  addr)))))

	(lambda (m) (case m
				  [(resolve) resolve]
				  [(alloc) alloc]
				  ))))
