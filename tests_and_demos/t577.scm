(let ((f (lambda (x)
		   (call/cc (lambda (k) k)))))
  (print (f 0))
  (print (f 0))
  )

;(let ((f (lambda (*)
;		   <call/cc identity> <-- here
;		   )))
;  (print (f 0))
;  (print (f 0))
;  )
