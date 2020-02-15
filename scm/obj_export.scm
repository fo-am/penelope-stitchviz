
(define (obj-write-triface f n v)
  (when (not (zero? n))	
	(display (string-append
		"f "
		(number->string v) "/" (number->string v) "/" (number->string v) " "
		(number->string (+ v 1)) "/" (number->string (+ v 1)) "/" (number->string (+ v 1)) " "
		(number->string (+ v 2)) "/" (number->string (+ v 2)) "/" (number->string (+ v 2)) "\n") f)
	(obj-write-triface f (- n 1) (+ v 3))))

(define (obj-write-data f n v type name)
  (when (not (zero? n))
	(let ((v (pdata-ref type v)))
	  (display (string-append
		    name " "
		    (number->string (vx v)) "\t"
		    (number->string (vy v)) "\t"
		    (number->string (vz v)) "\n") f))	
	(obj-write-data f (- n 1) (+ v 1) type name)))

(define (obj-export filename)
  (let ((f (open-output-file filename)))
    ;; assume triangles
    (msg (/ (pdata-size) 3))
    (obj-write-triface f (inexact->exact (floor (/ (pdata-size) 3))) 1)
    (obj-write-data f (pdata-size) 0 "p" "v")
    (obj-write-data f (pdata-size) 0 "n" "vn")
    (obj-write-data f (pdata-size) 0 "t" "vt")
    (close-output-port f)))
