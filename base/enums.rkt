#lang typed/racket

(provide define-enum define-incr-enum)

(define-syntax (define-enum stx)
  (syntax-case stx ()
    [(_ enum-name (name value) ...)
     (quasisyntax/loc stx
       (begin
         (provide enum-name)
         (define-type enum-name (U value ...))
         (begin
           (provide name)
           (define name : enum-name value)) 
         ...))]))

(define-syntax (define-incr-enum stx)
  (syntax-case stx ()
    [(_ enum-name const ...)
     (with-syntax ([(const ...)
                    (let ([i -1])
                      (for/list ([const (syntax->list #'(const ...))])
                        (syntax-case const ()
                          [(name value)
                           (set! i (syntax-e #'value))
                           const]
                          [name
                           (set! i (+ i 1))
                           (with-syntax ([i (datum->syntax const i)])
                             (syntax/loc const
                               (name i)))])))])
       (syntax/loc stx
         (define-enum enum-name const ...)))]))