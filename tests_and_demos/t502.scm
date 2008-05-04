;; higepon #3
(define cont #f)
(print (if (call/cc (lambda (c) (set! cont c) #f)) 100 0))

(cont #t)
(cont #f)


