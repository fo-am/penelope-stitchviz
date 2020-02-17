(load "scm/threadlang.scm")
(load "scm/obj_export.scm")

(define machine-seq
  (compile-thread "pull 4,
                   under, pull 1,
                   under, pull 1,
                   under, pull 1,
                   under, pull 1,
                   under, pull 1,
                   under, pull 1,
                   under, pull 1,
                   under, pull 1,
                twist,
                twist,
                twist,
                turn out, turn in,
                twist,
                pull 3,
                under, pull 1,
                under, pull 1,
                under, pull 1,
                under, pull 1,
                under, pull 1,
                under, pull 1,
                under, pull 1,
                under, pull 1,
                pull 1"))

(define conductive-seq
  (compile-thread "pull 1,
                   twist,
                   pull 2,
                   over, over,
                   under, under,

                   turn in,
                   turn out,

                   over, over,
                   under, under,
                   over, over,
                   under, under,
                   over, over,
                   under, under,

                   
                pull 1"))

(define thread-root (build-locator))

(define fabric
  (with-state
   (translate (vector 30 -2 0))
   (scale (vector 80 0.2 50))
   (build-cube)))

(with-primitive
 fabric
 (parent thread-root)
 (shader (slurp "shaders/transparent.vert.glsl")
	 (slurp "shaders/transparent.frag.glsl"))
 (shader-set! "LightPos" (vector 0.0 100.0 0.0))
 (shader-set! "DiffuseColour" (vector 1.0 0.3 0.2))
 (shader-set! "Opacity" 0.9)
 )



;; setup and build the primitive
(define machine-thread (build-jellyfish prim-size))

;; program the primitive
(with-primitive
 machine-thread
 (parent thread-root)
 (shader (slurp "shaders/gooch.vert.glsl")
         (slurp "shaders/gooch.frag.glsl"))
 (shader-set! "LightPos" (vector 1 10 -50))
 (shader-set! "WarmColour" (vector 0.2 0.8 0.1))
 (shader-set! "CoolColour" (vector 0.1 0.5 0.5))
 (shader-set! "SurfaceColour" (vector 0.2 0.2 0.3))
 (shader-set! "OutlineWidth" 0.0)
 (program-jelly 5000 prim-triangles 1 extruder)
 )

(define conductive-thread (build-jellyfish prim-size))

;; program the primitive
(with-primitive
 conductive-thread
 (parent thread-root)
 (translate (vector 0 -0.5 0))
 (shader (slurp "shaders/gooch.vert.glsl")
         (slurp "shaders/gooch.frag.glsl"))
 (shader-set! "LightPos" (vector 1 10 -50))
 (shader-set! "WarmColour" (vector 0.8 0.8 0.8))
 (shader-set! "CoolColour" (vector 0.1 0.1 0.1))
 (shader-set! "SurfaceColour" (vector 0.5 0.5 0.5))
 (shader-set! "OutlineWidth" 0.0) 
 (program-jelly 5000 prim-triangles 1 extruder)
 )

(define camera (build-locator))
(with-primitive
 camera
 (translate (vector 15 5 10)))
 (lock-camera camera)
(define frame 0)

(with-primitive
 thread-root
 (rotate (vector 20 45 0)))

;; todo: fix with-primitive with no thunk (due to when)
;; todo: fix pdata-ref/set! wrt to non-vector arguments!!
;; todo: document the fact that we need to wait for a couple of frames
;;       because the values in the "let" for the jfish program
;;       are initialised in code - so values written to addresses there
;;       will be overwritten if set in init stage!!

(every-frame
 (begin
   (when (eqv? frame 2)
	 (with-primitive
	  machine-thread
	  (pdata-set! "x" addr-profile-scale (vector 0.2 0 0))			
	  (write-circular-profile 7)
	  (write-seq machine-seq))
	 (with-primitive
	  conductive-thread
	  (pdata-set! "x" addr-profile-scale (vector 0.4 0 0))			
	  (write-circular-profile 7)
	  (write-seq conductive-seq)))

   (when (eqv? frame 100)
	 (with-primitive
	  machine-thread
	  (obj-export "models/machine-thread.obj"))
	  (with-primitive
	  conductive-thread
	  (obj-export "models/conductive-thread.obj")))
   
   ;;(with-primitive thread-root (rotate (vector 0 0.5 0)))
   (set! frame (+ frame 1))))

