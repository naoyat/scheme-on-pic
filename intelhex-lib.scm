;
; Intel HEX format
;
(use srfi-1)
(use util.list) ; slices

; 16進で記述された文字列をオクテット単位で数値(u8)化したリストを返す
; "0123" --> (#x01 #x23)
(define (hexstring->numbers hs)
  (let1 len (/ (string-length hs) 2)
	(map (lambda (i)
		   (let1 2i (* i 2)
			 (string->number (substring hs 2i (+ 2i 2)) 16)))
		 (iota len))))

; u8数値２つをまとめてu16数値にして返す
(define (bytes->word h l) (logior (ash h 8) l))
; u8数値リストを２バイトずつまとめてu16数値リストにして返す
; デフォルトはlittle-endian。
; big-endianで取りたい場合は (bytes->words bs #t)
(define (bytes->words bs . args)
  (let1 big-endian? (and (not (null? args)) (car args))
	(let loop ([rest bs] [ws '()])
	  (if (null? rest)
		  (reverse! ws)
		  (loop (cddr rest)
				(cons (if big-endian?
						  (bytes->word (car rest) (cadr rest))
						  (bytes->word (cadr rest) (car rest))
						  ) ws))
		  ))))

; HEXレコードの整合性をチェックサムで確認
(define (verify-checksum nums checksum)
  (let loop ([rest nums] [sum 0])
	(if (null? rest)
		(= 0 (modulo (+ sum checksum) 256))
		(loop (cdr rest) (+ sum (car rest))))))

; HEXファイルの１レコードを解読して返す
; :020000040000FA
; :0A000000831607108312071404286A
; :02400E00D430AC
; :00000001FF
(define (intel-hex-record l)
  (rxmatch-if (#/^:([0-9A-Fa-f]+)([0-9A-Fa-f][0-9A-Fa-f])$/ l)
	  (#f hs cs)
	(let ([nums (hexstring->numbers hs)]
		  [checksum (car (hexstring->numbers cs))])
	  (if (verify-checksum nums checksum)
		  (let ([record-length (car nums)]
				[load-offset (bytes->word (cadr nums) (caddr nums))]
				[record-type (cadddr nums)]
				[data (cddddr nums)])
			(if (= record-length (length data))
				(list record-type (/ load-offset 2) (bytes->words data))
				(error "record length mismatch")))
		  (error "checksum error")))
	(error "invalid hex data")))

; end
(define (hex-line load-offset record-type data)
  (define (in-big-endian x) (list (logand (ash x -8) #xff) (logand x #xff)))
  (define (in-little-endian x) (list (logand x #xff) (logand (ash x -8) #xff)))
  (define (checksum data)
	(let loop ([rest data] [sum 0])
	  (if (null? rest)
		  (logand #xff (- 256 (modulo sum 256)))
		  (loop (cdr rest) (+ sum (car rest))))))

  (let1 data `(,(* (length data) 2)
			   ,@(in-big-endian (* load-offset 2))
			   ,record-type
			   ,@(append-map in-little-endian data))
	(string-append ":" (string-upcase!
						(string-join (map (cut format "~2,'0x" <>) (append data (list (checksum data)))) "")))
	))

(define (hex-pack code)
  (string-join `(,(hex-line 0 4 '(#x0))
				 ,@(let loop ([ofs 0]
							  [sliced (slices code 8)]
							  [lines '()])
					 (if (null? sliced)
						 (reverse! lines)
						 (loop (+ ofs 8)
							   (cdr sliced)
							   (cons (hex-line ofs 0 (car sliced)) lines)
							   )))
				 ,(hex-line #x2007 0 '(#x30d4))
				 ,(hex-line 0 1 '()))
			   "\n"))

(define (save-obj-code obj-code file)
  (with-output-to-file file
	(lambda () (display (hex-pack obj-code)))))
