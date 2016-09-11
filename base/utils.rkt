#lang typed/racket/base/no-check
(require (for-syntax racket/base)
         racket/path racket/file racket/format)

(provide singleton? singleton-ref)
(define #:forall (T) (singleton? [l : (Listof T)]) : Boolean
  (and (not (null? l))
       (null? (cdr l))))

(define #:forall (T) (singleton-ref [l : (Listof T)]) : T
  (cond ((null? l)
         (error 'singleton-ref "Not a singleton ~A" l))
        ((null? (cdr l))
         (car l))
        (else
         (error 'singleton-ref "Not a singleton ~A" l))))

(provide division)
(define-sequence-syntax division
  (lambda () #'division/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(v) (clause from to elems)]
       #'[(v)
          (clause from to elems #t)]]
      [[(v) (_ from to elems last?)]
       #`[(v)
          (:do-in
           ([(a) (cast from Real)] [(b) (cast to Real)] [(n) (cast elems Integer)]
            #,@(case (syntax->datum #'last?)
                 ((#t #f) #'())
                 (else #'([(pred) (if last? <= <)]))))
           (unless (exact-positive-integer? n)
             (raise-type-error 'division "exact non-negative integer" n))
           ([i 0])
           (#,(case (syntax->datum #'last?)
               ((#t) #'<=)
               ((#f) #'<)
               (else #'pred))
            i n)
           ([(v) (if (= i n) b (+ a (/ (* i (- b a)) n)))]) ;Extra test to avoid rounding errors
           #true
           #true
           ((+ i 1)))]])))

(define (division/proc [a : Real] [b : Real] [n : Integer] [last? : Boolean #t]) : (Listof Real)
  (if last?
      (for/list : (Listof Real) ([t (division a b n #t)])
        t)
      (for/list : (Listof Real) ([t (division a b n #f)])
        t)))

(provide map-division)
(define map-division
  (case-lambda #:forall (R)
    [([f : (Real -> R)] [t0 : Real] [t1 : Real] [n : Integer])
     (for/list : (Listof R)
       ((t (division t0 t1 n)))
       (f t))]
    [([f : (Real -> R)] [t0 : Real] [t1 : Real] [n : Integer] [last? : Boolean])
     (for/list : (Listof R)
       ((t (division t0 t1 n last?)))
       (f t))]
    [([f : (Real Real -> R)] [u0 : Real] [u1 : Real] [nu : Integer] [v0 : Real] [v1 : Real] [nv : Integer])
     (for/list : (Listof (Listof R)) 
       ((u (division u0 u1 nu)))
       (for/list : (Listof R) ((v (division v0 v1 nv)))
         (f u v)))]
    [([f : (Real Real -> R)] [u0 : Real] [u1 : Real] [nu : Integer] [lastu? : Boolean] [v0 : Real] [v1 : Real] [nv : Integer])
     (for/list : (Listof (Listof R)) 
       ((u (division u0 u1 nu lastu?)))
       (for/list : (Listof R) 
         ((v (division v0 v1 nv)))
         (f u v)))]
    [([f : (Real Real -> R)] [u0 : Real] [u1 : Real] [nu : Integer] [lastu? : Boolean] [v0 : Real] [v1 : Real] [nv : Integer] [lastv? : Boolean])
     (for/list : (Listof (Listof R)) 
       ((u (division u0 u1 nu lastu?)))
       (for/list : (Listof R)
         ((v (division v0 v1 nv lastv?)))
         (f u v)))]))

(provide in-interval)
(define-sequence-syntax in-interval
  (lambda () #'in-interval/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(v) (_ from to elems)]
       #'[(v)
          (:do-in
           ([(a) from] [(b) to] [(n) elems])
           (unless (exact-positive-integer? n)
             (raise-type-error 'in-interval "exact non-negative integer" n))
           ([i 0])
           (<= i n)
           ([(v) (+ a (/ (* i (- b a)) n))])
           #true
           #true
           ((+ i 1)))]])))

(define (in-interval/proc [a : Real] [b : Real] [n : Integer])
  (for/list : (Listof Real) ([t (in-interval a b n)])
    t))

(provide in-period)
(define-sequence-syntax in-period
  (lambda () #'in-period/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(v) (_ from to elems)]
       #'[(v)
          (:do-in
           ([(a) from] [(b) to] [(n) elems])
           (unless (exact-positive-integer? n)
             (raise-type-error 'in-period "exact non-negative integer" n))
           ([i 0])
           (< i n)
           ([(v) (+ a (/ (* i (- b a)) n))])
           #true
           #true
           ((+ i 1)))]])))

(define (in-period/proc [a : Real] [b : Real] [n : Integer])
  (for/list : (Listof Real) ([t (in-period a b n)])
    t))

(provide map-in-interval map-in-period)

(define map-in-interval
  (case-lambda #:forall (R)
    (([f : (Real -> R)] [t0 : Real] [t1 : Real] [n : Integer])
     (for/list : (Listof R)
       ((t (in-interval t0 t1 n)))
       (f t)))
    (([f : (Real Real -> R)] [u0 : Real] [u1 : Real] [nu : Integer] [v0 : Real] [v1 : Real] [nv : Integer])
     (for/list : (Listof (Listof R)) 
       ((u (in-interval u0 u1 nu)))
       (for/list : (Listof R) 
         ((v (in-interval v0 v1 nv)))
         (f u v))))))

(define map-in-period
  (case-lambda #:forall (R)
    (([f : (Real -> R)] [t0 : Real] [t1 : Real] [n : Integer])
     (for/list : (Listof R)
       ((t (in-period t0 t1 n)))
       (f t)))
    (([f : (Real Real -> R)] [u0 : Real] [u1 : Real] [nu : Integer] [v0 : Real] [v1 : Real] [nv : Integer])
     (for/list : (Listof (Listof R)) 
       ((u (in-period u0 u1 nu)))
       (for/list : (Listof R) 
         ((v (in-period v0 v1 nv)))
         (f u v))))))

;;Typed lazy functions
(define-syntax-rule
  (define-cached (name) : type expr ...)
  (define name : (-> type)
    (lambda ()
      (let ((val (begin expr ...)))
        (set! name (lambda () val))
        val))))

(provide define-cached)


(provide random-range random random-seed!)

(define ultimo-aleatorio-gerado : Integer 12345)

(define (set-ultimo-aleatorio-gerado! [v : Integer])
  (set! ultimo-aleatorio-gerado v))

(define (random-seed! [v : Integer])
  (set! ultimo-aleatorio-gerado v))

(define (proximo-aleatorio [ultimo-aleatorio : Integer])
  (let ((teste (- (* 16807 (remainder ultimo-aleatorio 127773))
                  (* 2836  (quotient ultimo-aleatorio 127773)))))
    (if (> teste 0)
        (if (> teste 2147483647)
            (- teste 2147483647)
            teste)
        (+ teste 2147483647))))

(define (aleatorio)
  (set! ultimo-aleatorio-gerado
        (proximo-aleatorio ultimo-aleatorio-gerado))
  ultimo-aleatorio-gerado)

;This cannot be easily converted to a contract (and typed/untyped-utils is not helping)
;(: random (case-> (-> Integer Integer) (-> Float Float)))
;so we use this instead
(: random (-> Real Real))
(define (random x)
  (if (exact-integer? x)
      (remainder (aleatorio) x)
      (* x (cast (/ (aleatorio) 2147483647.0) Float))))

(provide random-integer)
(define (random-integer [x : Integer]) : Integer
  (remainder (aleatorio) x))

;This cannot be easily converted to a contract (and typed/untyped-utils is not helping)
;(: random-range (case-> (-> Integer Integer Integer) (-> Float Float Float)))
;so we use this instead
(: random-range (-> Real Real Real))
(define (random-range x0 x1)
  (if (= x0 x1)
      x0
      (+ x0 (random (- x1 x0)))))

(provide random-integer-range)
(define (random-integer-range [x0 : Integer] [x1 : Integer]) : Integer
  (+ x0 (random-integer (- x1 x0))))

(provide maximize-combination)
(define #:forall (T) (maximize-combination [op : (-> T T (Option T))] [rs : (Listof T)]) : (Listof T)
  (define (combine [r0 : T] [rs : (Listof T)]) : (Listof T)
    (if (null? rs)
        (list r0)
        (let ([r1 (car rs)]
              [rs (cdr rs)])
          (let ((r (op r0 r1)))
            (if r
                (cons r rs)
                (cons r1 (combine r0 rs)))))))
  (cond ((null? rs) (list))
        ((null? (cdr rs)) rs)
        (else
         (let loop : (Listof T)
           ([rs : (Listof T) rs] [combs : (Listof T) (list)] [n : Integer (length rs)])
           (if (null? rs)
               (if (= n (length combs)) ;;no changes
                   combs
                   (loop combs (list) (length combs)))
               (let ([r1 (car rs)]
                     [rs (cdr rs)])
                 (loop rs (combine r1 combs) n)))))))


;;Colors

(define-type Color (U rgb))

(struct rgb
  ([red : Byte]
   [green : Byte]
   [blue : Byte]))

(provide Color (struct-out rgb))

;;Renders and Films
(provide render-dir
         render-user-dir
         render-backend-dir
         render-kind-dir
         render-color-dir
         render-ext
         render-width
         render-height
         set-render-dir
         render-size
         prepare-for-saving-file
         render-pathname)

(define-type PathParameter (Parameterof (U Path-For-Some-System Path-String 'up 'same)))

;;There is a render directory
(define render-dir : PathParameter (make-parameter (find-system-path 'home-dir)))
;;with a user-specific subdirectory
(define render-user-dir : PathParameter (make-parameter 'same))
;;with a backend-specific subdirectory
(define render-backend-dir : PathParameter (make-parameter 'same))
;;and with subdirectories for static images, movies, etc
(define render-kind-dir : PathParameter (make-parameter "Render"))
;;and with subdirectories for white, black, and colored renders
(define render-color-dir : PathParameter (make-parameter 'same))
;;containing files with different extensions
(define render-ext : (Parameterof String) (make-parameter "png"))

(define (render-pathname [name : String]) : Path-String
  (some-system-path->string
   (simplify-path
    (build-path (render-dir)
                (render-user-dir)
                (render-backend-dir)
                (render-kind-dir)
                (render-color-dir)
                (format "~A.~A" name (render-ext)))
    #f)))

(provide )

(define render-width (make-parameter 1024))
(define render-height (make-parameter 768))
(define render-floor-width (make-parameter 1000))
(define render-floor-height (make-parameter 1000))

(define (set-render-dir [val : String])
  (render-dir (path->directory-path (build-path val))))

(define (render-size [width : Integer] [heigth : Integer])
  (render-width width)
  (render-height heigth))

(define (prepare-for-saving-file [path : Path-String])
  (let ((dir (path-only path)))
    (when dir
      (make-directory* dir)))
  (when (file-exists? path)
    (delete-file path))
  path)


(provide film-active? film-filename film-frame start-film frame-filename)
(define film-active? : (Parameterof Boolean) (make-parameter #f))
(define film-filename : (Parameterof String) (make-parameter ""))
(define film-frame : (Parameterof Natural) (make-parameter 0))

(define (start-film [name : String])
  (film-active? #t)
  (film-filename name)
  (film-frame 0))

(define (frame-filename [filename : String] [i : Integer])
  (~a filename "-frame-" (~r i #:min-width 3  #:pad-string "0")))

(provide radians<-degrees)
(define (radians<-degrees [d : Real])
  (* d (/ pi 180)))
