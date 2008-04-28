;;
;; scm-object
;;
(define-macro (make-scm-misc-object id)
  `(logior #b01110 (ash ,id 5)))
(define-macro (make-proc-tag arity)
  `(logior #b0110 (ash (logand #b1111 ,arity) 4)))

(define scm-false      (make-scm-misc-object 0)) ; 000 01110 (0E)
(define scm-true       (make-scm-misc-object 1)) ; 001 01110 (2E)
(define scm-nil        (make-scm-misc-object 2)) ; 010 01110 (4E)
(define scm-eof-object (make-scm-misc-object 3)) ; 011 01110 (6E)
(define scm-undefined  (make-scm-misc-object 4)) ; 100 01110 (8E)
(define scm-unbound    (make-scm-misc-object 5)) ; 101 01110 (AE)

(define scm-lambda     (make-scm-misc-object 7)) ; 111 01110 (EE)


(define scm-continuation  (make-proc-tag #b1111)) ; 1111 0110 (F6)

;;
;; scm integer
;;
;;(define (make-scm-6bit-integer i)
;;  (logior #x01 (ash i 2)))

(define (make-scm-16bit-integer i)
  (let1 ui (logand #xffff i) ; これは i<0 でも [0,65535] 内で返ってくる
	(let ([upper (ash ui -8)]
		  [lower (logand ui #xff)])
	  `((-- ,(format "consti ~d" ui))
		(mov    w ,upper)
		(push)
		(mov    w ,lower)
;		(save-w)
		(int16)
		))))

(define (make-scm-proc-object arity entrypoint)
  `((-- λ < ,arity > { ,entrypoint } )
	(mov   w ,(make-proc-tag arity))
	(push) ; [----0110]
	(mov   w (label ,entrypoint))
	(push) ; [entrypoint ----0110]
	(mov   w env)
	(cons) ; (entrypoint . env) [----0110]
;	(asm (DEBUG:scm:w))
	(cons) ; (----0110 entrypoint . env)
;	(asm (DEBUG:scm:w))
	))
