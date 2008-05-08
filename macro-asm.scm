;;;
;;; macro-asm
;;;
;;;   Copyright (c) 2008 naoya_t (naoya.t@aqua.plala.or.jp)
;;;

(use srfi-1)

;
; レジスタアロケータ
;
(require "./register")
(define register-system (make-register-system))

;
; ラベル
;
(define (make-label-gen)
  (let1 *label-id* 0
	(lambda (name)
	  (string->symbol (string-append name (x->string (inc! *label-id*)))))))
(define label-gen (make-label-gen))

;
; (pluggableな)マクロアセンブラ
;
(require "./pic-asm") ; *asm-instructions*
(define (make-macro-assembler plugin-packages)
  (let1 unpacked (map (lambda (package) (package)) plugin-packages)
	(let ([registers-to-allocate (append-map first unpacked)]
		  [init-code (append-map second unpacked)]
		  [subroutine-code (append-map third unpacked)]
		  [plug-ins (append-map fourth unpacked)])
;	  (map print plug-ins)
;		  [plug-ins (reverse! (append-map fourth unpacked))])
	  
	  (lambda (src)
		(define (pass inst)
		  (cond [(not (pair? inst)) ; ラベル
				 (list inst)]
				[(and (<= 3 (length inst)) (eq? 'EQU (second inst)))
				 (list inst)] ; EQU文。アセンブラにそのまま渡す (label EQU addr)
				[(memq (car inst) '(-- *rem remark))
				 `((*rem ,@(cdr inst)))]
				[(memq (car inst) '(END End end))
				 '((GOTO $))]
				[(memq (car inst) '(org ORG))
				 `((ORG ,(cadr inst)))]
				[(memq (car inst) *asm-instructions*)
				 (list inst)]
				[else
				 (let1 plug-in (let1 pair (assoc (car inst) plug-ins)
								 (if pair (cadr pair) #f))
				   (if plug-in
					   (append-map pass (apply plug-in (cdr inst)))
					   (error "Unknown instrution -- PASS" inst)))]))

		(append-map pass
					`(initialize
					  ,@(map (lambda (reg) `(alloc ,reg)) registers-to-allocate)
					  ,@init-code
					  body
					  ,@src
					  (GOTO $)
					  utility_subroutines
					  ,@subroutine-code
					  )))
	  )))

;;
;; compile
;;
;(require "./macro-asm-plugin")
(require "./macro-asm-basic-plugin")
(require "./macro-asm-wait-plugin")
(require "./macro-asm-LED-plugin")
(require "./macro-asm-A2D-plugin")
(require "./macro-asm-register-machine-plugin")
(require "./macro-asm-scheme-plugin")
(require "./macro-asm-debug-plugin")
(require "./macro-asm-eeprom-plugin")

(define (pic-macro-asm src)
  (let1 macro-asm (make-macro-assembler (list plug-in:basic
											  plug-in:register-machine
											  plug-in:wait
											  plug-in:LED
											  plug-in:A2D
											  plug-in:scheme
											  plug-in:debug))
	(macro-asm src)))
