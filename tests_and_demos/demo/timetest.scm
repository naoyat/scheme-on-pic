(use srfi-1)
(use nt.printf)

(require "./pic16f690")

(define (tick)
  (let1 t (current-time)
	(+ (ref t 'second) (/ (ref t 'nanosecond) 1000000000))))

(define pic (make-pic16f690))

;(define time-to-reset (let1 since (tick)
;						(dotimes (i 100000) ([pic'pc-set!] 0))
;						(- (tick) since)))

(define ts (make-vector #x4000 0))

(for-each (lambda (inst)
			([pic'pc-set!] 0)
			([pic'write-inst-at-pc] inst)
			(let1 since (tick)
			  (dotimes (i 100)
				[pic'step]
				([pic'pc-set!] 0)
				)
			  (let1 usec (* 10000 (- (tick) since))
				(when (< 1.0 usec)
				  ([pic'pc-set!] 0)
				  (printf "%04x %s: %g\n" inst [pic'mnemonic] usec)))
			  ))
		  (iota #x4000))

;(define ts (make-vector #x4000 0))
;(dotimes (i 100)
;  (for-each (lambda (inst)
;			  ([pic'pc-set!] 0)
;			  ([pic'write-inst-at-pc] inst)
;			  (let1 since (tick)
;				[pic'step]
;				([pic'pc-set!] 0)
;				(let1 usec (* 1000 (- (tick) since))
;				  (vector-set! ts inst usec)
;				  )))
;			(iota #x4000)
;			))
 
;(dotimes (inst #x4000)
;  (let1 t (* 1000 (vector-ref ts inst))
;	(when (< 1.0 t)
;	  (printf "%0x: %g\n" inst t)
;	  )))
