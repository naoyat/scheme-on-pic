;;;
;;; macro-asm-pluin
;;;
(use srfi-1)

(define (reg-alloc reg)
  ([register-system'alloc] reg (iota #x50 #x20))) ;#x20-6f
(define (reg-resolve reg)
  ([register-system'resolve] reg))

;;
;; pic utility plug-ins
;;
(define (make-plugin registers-to-allocate init-code subroutines . command-table)
  (list registers-to-allocate init-code subroutines command-table))
