#lang typed/racket/base/no-check
(require (for-syntax racket/base))
(require (except-in "autocad/backend.rkt" division translating))
(provide (all-from-out "autocad/backend.rkt"))

(provide division
         in-period
         in-interval
         translating
         scaling
         x-rotating
         y-rotating
         z-rotating)

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
           ([(v) (+ a (/ (* i (- b a)) n))])
           #true
           #true
           ((+ i 1)))]])))

(define (division/proc [a : Real] [b : Real] [n : Integer] [last? : Boolean #t]) : (Listof Real)
  (if last?
      (for/list : (Listof Real) ([t (division a b n #t)])
        t)
      (for/list : (Listof Real) ([t (division a b n #f)])
        t)))

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


(define-syntax-rule
  (translating dx dy dz body ...)
  (parameterize ((current-cs (translated-cs dx dy dz (current-cs))))
    body ...))

(define-syntax-rule
  (scaling s body ...)
  (let ((r s))
    (parameterize ((current-cs (scaled-cs r r r (current-cs))))
      body ...)))

(define-syntax-rule
  (x-rotating phi body ...)
  (parameterize ((current-cs (x-rotated-cs phi (current-cs))))
    body ...))

(define-syntax-rule
  (y-rotating phi body ...)
  (parameterize ((current-cs (y-rotated-cs phi (current-cs))))
    body ...))

(define-syntax-rule
  (z-rotating phi body ...)
  (parameterize ((current-cs (z-rotated-cs phi (current-cs))))
    body ...))
