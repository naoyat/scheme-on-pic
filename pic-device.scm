;;;
;;; pic-device
;;;
;;;   Copyright (c) 2008 naoya_t (naoya.t@aqua.plala.or.jp)
;;;

;
; オブジェクトコードをhex化して書き込み
;
(define (pic-write obj-code)
  (let1 hex-file "_test.hex"
	(save-obj-code obj-code hex-file)
	(pic-write-hex-file hex-file)))
  
;
; hexファイルを書き込み
;
(define (pic-write-hex-file hex-file)
  (sys-system (string-append "pk2 -write " hex-file)))

;
; 給電ON
;
(define (pic-on) (sys-system "pk2 -on > /dev/null"))
;
; 給電OFF
;
(define (pic-off) (sys-system "pk2 -off > /dev/null"))

;
; オブジェクトコードを書き込んで走らせる
;
(require "./intelhex-lib")
(define (pic-run obj-code name sec)
  (let1 hex-file (string-append name ".hex")
	(save-obj-code obj-code hex-file)
	(pic-write-hex-file hex-file)
	(pic-on)
	(sys-sleep sec)
	(pic-off)
	;delete hex-file here
	))
;;
