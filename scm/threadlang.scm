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
;; 0 = pull
;; 1 = turn left
;; 2 = turn right
;; 3 = over
;; 4 = under
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

(define extruder
  ;; jellyfish code starts here
  '(let ((vertex positions-start)     ;; current vertex
         (pos (vector 0 0 0))         ;; current position (centre of profile)
         (last-pos (vector 0 0 0))    ;; last position
         (dir (vector 0 0 0))         ;; current direction we are heading in

         (tx-a (vector 1 0 0))        ;; working rotation matrix row 1
         (tx-b (vector 0 1 0))        ;; row 2
         (tx-c (vector 0 0 1))        ;; row 3
         (cur-tx-a (vector 1 0 0))    ;; current rotation matrix row 1
         (cur-tx-b (vector 0 1 0))    ;; row 2
         (cur-tx-c (vector 0 0 1))    ;; row 3
         (last-tx-a (vector 1 0 0))   ;; last rotation matrix row 1
         (last-tx-b (vector 0 1 0))   ;; row 2
         (last-tx-c (vector 0 0 1))   ;; row 3

         (t 0)                        ;; slice counter
         (seq-size 14)                ;; number of sequence instructions
         (seq-pos 0)                  ;; current sequence position
         (seq-cur 0)                  ;; current instruction

         (segments 4)                 ;; profile segment count
         (seq-next 0)                 ;; next sequence instruction
         (seq-last 0)                 ;; last sequence instruction
         (profile-pos 0)              ;; current profile position
         (profile-scale 0.1)
         (profile-size 3)             ;; size of profile

         (profile-start (vector 0 0 1)) ;; profile data starts here
         (profile-b (vector 0 1 0))  ;; some space for extra follows
         (profile-c (vector 0 0 -1)) ;; (this default profile gets overwritten))
         (profile-d (vector 0 -1 0))
         (profile-loop (vector 0 0 1))
         (space-0 0)
         (space-1 0)
         (space-2 0)
         (space-3 0)
         (space-4 0)
         (space-5 0)
         (space-6 0)
         (space-7 0)
         (space-8 0)
         (space-9 0)
         (space-10 0)
         (space-12 0)
         )


     (trace (addr profile-scale))

     ;; set working matrix to identity
     (define init-mat
        (lambda ()
          (set! tx-a (vector 1 0 0))
          (set! tx-b (vector 0 1 0))
          (set! tx-c (vector 0 0 1))))

     ;; rotate working matrix in x by a
     (define rotate-mat-x
       (lambda (a)
         (set! tx-a (vector 1 0 0))
         (set! tx-b (*v (swizzle zyx (sincos a)) (vector 1 1 -1)))
         (set! tx-c (swizzle zxy (sincos a)))))

     ;; rotate working matrix in y by a
     (define rotate-mat-y
       (lambda (a)
         (set! tx-a (*v (swizzle yzx (sincos a)) (vector 1 1 -1)))
         (set! tx-b (vector 0 1 0))
         (set! tx-c (swizzle xzy (sincos a)))))

     ;; rotate working matrix in z by a
     (define rotate-mat-z
       (lambda (a)
         (set! tx-a (swizzle yxz (sincos a)))
         (set! tx-b (*v (sincos a) (vector -1 1 1)))
         (set! tx-c (vector 0 0 1))))

     ;; project vector by a matrix at supplied address
     (define tx-proj
       (lambda (tx-addr v)
         (+
          (+ (dot v (read tx-addr))
             (swizzle yxy (dot v (read (+ tx-addr 1)))))
          (swizzle yyx (dot v (read (+ tx-addr 2)))))))

     (forever
      (set! vertex positions-start)
      (loop (< vertex positions-end)
            ;; build a new segment
            ;; read current, next and last sequence instructions
            (set! seq-cur (read (+ seq-pos texture-start)))
            (set! seq-next (read (+ (+ seq-pos 1) texture-start)))
            (set! seq-last (read (+ (- seq-pos 1) texture-start)))
	    
            ;; reset working matrix
            (init-mat)

            (cond
             ((eq? seq-cur 0) ;; forward (pull 1)
              ;; make a single segment by advancing pos to end
              (set! pos (+ pos (* dir (- segments 1))))
              ;; force skip to next segment
              (set! t segments))

             ((eq? seq-cur 1) ;; rotate 90 degrees left
              (rotate-mat-y (/ 90 segments))
              ;; tweak turning circle to fit segment
              (set! pos (+ pos (* dir -0.2))))

             ((eq? seq-cur 2) ;; rotate 90 degrees right
              (rotate-mat-y (/ -90 segments))
              (set! pos (+ pos (* dir -0.2))))


             ((eq? seq-cur 3) ;; over
              (when
               ;; check next and last, either skip beginning or end
               ;; so we join smoothly with the last segment
               (and
                (or (not (eq? seq-last 3)) (> t (/ segments 2)))
                (or (not (eq? seq-next 3)) (< t (- (/ segments 2) 1))))

               (set! pos (+ pos (* dir -0.25)))

               (when (eq? t 0) (rotate-mat-z -45))
               (when (eq? t 1)
                     ;; move a bit more up and down
                     (set! pos (+ pos (* dir 1)))
                     (rotate-mat-z 45))
               (when (eq? t 2)
                     (rotate-mat-z 45))
               (when (eq? t 3)
                     (set! pos (+ pos (* dir 1)))
                     (rotate-mat-z -45))))

              ((eq? seq-cur 4) ;; under
               (when
                ;; check next and last, either skip beginning or end
                ;; so we join smoothly with the last segment
                (and
                 (or (not (eq? seq-last 4)) (> t (/ segments 2)))
                 (or (not (eq? seq-next 4)) (< t (- (/ segments 2) 1))))

                (set! pos (+ pos (* dir -0.25)))

                (when (eq? t 0) (rotate-mat-z 45))
                (when (eq? t 1)
                      ;; move a bit more up and down
                      (set! pos (+ pos (* dir 2)))
                      (rotate-mat-z -45))
                (when (eq? t 2)
                      (rotate-mat-z -45))
                (when (eq? t 3)
                      (set! pos (+ pos (* dir 2)))
                      (rotate-mat-z 45))))	      
	      
              ((eq? seq-cur 5) ;; twist
	       (rotate-mat-x (/ -90 segments))) 	      
	      )
	    
            ;; apply working matrix to the to current one
            (*m (addr tx-a) (addr cur-tx-a) (addr cur-tx-a))

            ;; smooth by moving a bit here...
            (set! pos (+ pos (* dir 0.5)))
            ;; update direction with current rotation matrix
            (set! dir (tx-proj (addr cur-tx-a) (vector 1 0 0)))
            ;; and the rest post update here...
            (set! pos (+ pos (* dir 0.5)))


            ;; setup for constructing a new profile
            (set! profile-pos (addr profile-start))
            (loop (< profile-pos (+ (addr profile-start) profile-size))
                  ;; stitch with last rotation matrix and position
                  (write! vertex
                          (+ pos (tx-proj (addr cur-tx-a) (* (read (+ profile-pos 1)) profile-scale)))
                          (+ pos (tx-proj (addr cur-tx-a) (* (read profile-pos) profile-scale)))
                          (+ last-pos (tx-proj (addr last-tx-a) (* (read profile-pos) profile-scale)))

                          (+ last-pos (tx-proj (addr last-tx-a) (* (read profile-pos) profile-scale)))
                          (+ last-pos (tx-proj (addr last-tx-a) (* (read (+ profile-pos 1)) profile-scale)))
                          (+ pos (tx-proj (addr cur-tx-a) (* (read (+ profile-pos 1)) profile-scale))))
                  ;; transform and apply the normals
                  (write! (+ vertex prim-size)
                          (* (tx-proj (addr cur-tx-a) (read (+ profile-pos 1))) -1)
                          (* (tx-proj (addr cur-tx-a) (read profile-pos)) -1)
                          (* (tx-proj (addr last-tx-a) (read profile-pos)) -1)

                          (* (tx-proj (addr last-tx-a) (read profile-pos)) -1)
                          (* (tx-proj (addr last-tx-a) (read (+ profile-pos 1))) -1)
                          (* (tx-proj (addr cur-tx-a) (read (+ profile-pos 1))) -1))
		  
                  ;; (write! (+ vertex prim-size)
                  ;;         (tx-proj (addr cur-tx-a) (read profile-normal-pos))
                  ;;         (tx-proj (addr cur-tx-a) (read profile-normal-pos))
                  ;;         (tx-proj (addr last-tx-a) (read profile-normal-pos))

                  ;;         (tx-proj (addr last-tx-a) (read profile-normal-pos))
                  ;;         (tx-proj (addr last-tx-a) (read profile-normal-pos))
                  ;;         (tx-proj (addr cur-tx-a) (read profile-normal-pos)))


                  ;; increment the vertex
                  (set! vertex (+ vertex 6))
                  ;; increment profile vertex
                  (set! profile-pos (+ profile-pos 1))
                  ;;(set! profile-normal-pos (+ profile-normal-pos 1))
		  )


            (set! t (+ t 1))
            ;; have we finished this section yet?
            (when (> t segments)
                  (set! t 0)
                  ;; sort out fixed point rounding errors
                  ;; by snapping the matrix to 90 degrees
                  (set! cur-tx-a (round cur-tx-a))
                  (set! cur-tx-b (round cur-tx-b))
                  (set! cur-tx-c (round cur-tx-c))
                  (set! seq-pos (+ seq-pos 1)))

            ;; finished, so loop forever
	    (when (> seq-pos seq-size)
		  (set! seq-pos 0)
                  (forever))

	    ;; record the current pos and rotation
            (set! last-pos pos)
            (set! last-tx-a cur-tx-a)
            (set! last-tx-b cur-tx-b)
            (set! last-tx-c cur-tx-c)
            ))))




;;(set! debug #t)
;;(msg (disassemble-compiled extruder))
;;(set! debug #f)

;; initial turn state
(define turn-state 1)

(define (clear-turn-state!)
  (set! turn-state 0))

;; flip and return turn state
(define (flip-turn-state)
  (cond
   ((eqv? turn-state 1)
    (set! turn-state 2))
   (else
    (set! turn-state 1)))
  turn-state)

;; convert from yaxu's yarn code to rendering instructions
(define (convcode c)
  (cond
   ((equal? (car c) "under") (list 3))
   ((equal? (car c) "over") (list 4))
   ((equal? (car c) "twist") (list 5))
   ((equal? (car c) "turn")
    (if (equal? (cadr c) "in")
        (list turn-state)
        (list (flip-turn-state))))
   ((equal? (car c) "pull") (build-list (lambda (_) 0)
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
