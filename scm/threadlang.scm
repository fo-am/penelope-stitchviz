;; Copyright (C) 2015 Dave Griffiths
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; weave simulation extrusion
;; scheme and jellyfish lisp code

;; increase the primitive size so we can have long yarn
(set! prim-size 8000)

;; addresses into the jellyfish VM code
(define addr-seq-size 21)
(define addr-profile-scale 28)
(define addr-profile-size 29)
(define addr-t-normal-offset 512)

(define instr-pull 0)
(define instr-turn-left 1)
(define instr-turn-right 2)
(define instr-over 3)
(define instr-under 4)
(define instr-twist 5)
(define instr-over2 6)
(define instr-under2 7)

; build a circular profile shape
(define (write-circular-profile size)
  (define (_ n addr)
    (when (<= n size) ;; overflow one at the end
          (pdata-set!
           "x" addr
           (vmul
            (vector 0
                    (sin (* 2 3.141 (/ n size)))
                    (cos (* 2 3.141 (/ n size))))
            1.5))
          (_ (+ n 1) (+ addr 1))))
  (pdata-set! "x" addr-profile-size (vector size 0 0))
  (_ 0 (+ addr-profile-size 1)))

(define (print-circular-profile size)
  (define (_ n addr)
    (when (<= n size) ;; overflow one at the end
          (msg (pdata-ref "x" addr))
          (_ (+ n 1) (+ addr 1))))
  (_ 0 (+ addr-profile-size 1)))

; build a circular profile shape
(define (write-rect-profile)
  (let ((l 1.5))
    (pdata-set! "x" addr-profile-size (vector 4 0 0))
    (pdata-set! "x" (+ addr-profile-size 1) (vector 0 l l))
    (pdata-set! "x" (+ addr-profile-size 2) (vector 0 l (- l)))
    (pdata-set! "x" (+ addr-profile-size 3) (vector 0 (- l) (- l)))
    (pdata-set! "x" (+ addr-profile-size 4) (vector 0 (- l) 0))

    (pdata-set! "t" (+ addr-t-normal-offset 0) (vector 0 1 0))
    (pdata-set! "t" (+ addr-t-normal-offset 1) (vector 0 0 -1))
    (pdata-set! "t" (+ addr-t-normal-offset 2) (vector 0 -1 0))
    (pdata-set! "t" (+ addr-t-normal-offset 3) (vector 0 0 1))
    (pdata-set! "t" (+ addr-t-normal-offset 4) (vector 0 1 0))

    ))

;; write sequence data to (otherwise unused) texture coordinates
(define (write-seq seq)
  (define addr 0)
  (pdata-set! "x" addr-seq-size (vector (length seq) 0 0))
  (for-each
   (lambda (v)
     (pdata-set! "t" addr (vector v 0 0))
     (set! addr (+ addr 1)))
   seq))

(define test-prog
  '(let ((vertex positions-start))
     (loop (< vertex positions-end)
	   (write! vertex (* (rndvec) 10))
	   (set! vertex (+ vertex 1)))))

(define (load-code fn)
  (let* ((f (open-input-file fn))
         (r (read f)))
    (close-input-port f) r))

(define extruder (load-code "jelly/extruder.jelly"))

;;(set! debug #t)
;;(msg (disassemble-compiled extruder))
;;(set! debug #f)

;; initial turn state
(define turn-state instr-turn-right)

(define (clear-turn-state!)
  (set! turn-state instr-turn-right))

;; flip and return turn state
(define (flip-turn-state)
  (cond
   ((eqv? turn-state instr-turn-left)
    (set! turn-state instr-turn-right))
   (else
    (set! turn-state instr-turn-left)))
  turn-state)

;; convert from yaxu's yarn code to rendering instructions
(define (convcode c)
  (cond
   ((equal? (car c) "under") (list instr-under))
   ((equal? (car c) "over") (list instr-over))
   ((equal? (car c) "twist") (list instr-twist))
   ((equal? (car c) "turn")
    (if (equal? (cadr c) "in")
        (list turn-state)
        (list (flip-turn-state))))
   ((equal? (car c) "pull") (build-list (lambda (_) instr-pull)
                                        (string->number (cadr c))
                                        ))))

;; convert a string containing a list of yarn code
(define (compile-thread incode)    
  (clear-turn-state!)
  (foldl
   (lambda (i r)
     (append r (convcode (string-split i))))
   '()
   (string-split incode (list #\,))))
