#lang typed/racket/base/no-check
(require (for-syntax racket/base))
(require typed/racket/unsafe)

(require/typed/provide
 "com.rkt"
 [#:opaque Com-Object com-object?]
 [#:opaque CLSID clsid?]
 [#:opaque Type-Described type-described?]
 [#:opaque Type-Description type-description?]
 [progid->clsid         (String -> CLSID)]
 [com-create-instance   (CLSID -> Com-Object)]
 [com-get-active-object (CLSID -> Com-Object)]
 [com-invoke            (Com-Object String Any * -> Any)]
 [type-describe         (Any Any -> Type-Described)]
 [com-get-property      (Com-Object String String * -> Any)]
 [com-get-property*     (Com-Object String Any * -> Any)]
 [com-set-property!     (case-> (Com-Object String Any -> Any)
                                (Com-Object String String Any -> Any)
                                (Com-Object String String String Any -> Any))]
 [#:opaque Com-Omit com-omit?]
 [com-omit Com-Omit])

(require (for-syntax racket/list racket/syntax))

(provide with-conversions
         with-upgrade-types
         (for-syntax upgrade-type)
         convert-to-type
         def-com-method
         def-rw-property
         def-ro-property
         com-omit
         com-omit?
         com-object?)

;;Com types rarely match Racket types. We will use a conversion mechanism:
;;To convert between types

(begin-for-syntax
  (require syntax/id-table racket/dict)
  
  (define conversion-table (make-free-id-table))
  
  (define (set-conversion-from-to! from to conv)
    (let ((to-table
           (dict-ref 
            conversion-table from
            (lambda ()
              (let ((to-table (make-free-id-table)))
                (dict-set! conversion-table from to-table)
                to-table)))))
      (dict-set! to-table to conv)))
  
  (define (get-conversion-from-to expr from to)
    (define (conversion-error)
      (raise-syntax-error 
       #f 
       (format "Can't convert from ~a to ~a" 
               (syntax->datum from) (syntax->datum to))
       expr from (list to)))
    (let ((to-table (dict-ref conversion-table from conversion-error)))
      (dict-ref to-table to conversion-error)))
  
  (define (types-converting-to type)
    (for/fold ([types (list)]) ([(k m) (in-dict conversion-table)])
      (if (dict-ref m type #f)
          (cons k types)
          types)))
  
  (define (type-converting-to type)
    (let ((types (types-converting-to type)))
      (cond ((null? types) (error "No types convert to" type))
            ((null? (cdr types)) (car types))
            (else (error "Multiple types convert to" type)))))
  
  #;(provide set-conversion-from-to! types-converting-to type-converting-to))

#;(provide convert-to-type)
(define-syntax (convert-to-type stx)
  (syntax-case stx ()
    [(to t0 t1 expr)
     (if (and (identifier? #'t0) (identifier? #'t1))
         (if (free-identifier=? #'t0 #'t1)
             (syntax/loc stx expr)
             (with-syntax ([conv (get-conversion-from-to stx #'t0 #'t1)])
               (syntax/loc stx (conv expr))))
         (if (equal? (syntax->datum #'t0) (syntax->datum #'t1))
             (syntax/loc stx expr)
             (raise-syntax-error 
              #f 
              (format "Can't convert from ~a to ~a"
                      (syntax->datum #'t0) (syntax->datum #'t1)))))]))

(define-syntax (identity-macro stx)
  (syntax-case stx ()
    [(_ e) (syntax/loc stx e)]))

(define-syntax (with-conversions stx)
  (syntax-case stx ()
    [(_ (conv ...))
     (quasisyntax/loc stx
       (with-actual-conversions
           #,(map (lambda (stx)
                    (syntax-case stx ()
                      [(from to) (syntax/loc stx (from to identity-macro))]
                      [_ stx]))
                  (syntax->list #'(conv ...)))))]))

(define-syntax (with-actual-conversions stx)
  (syntax-case stx ()
    [(_ ([from to conv] ...))
     (quasisyntax/loc stx
       (begin
         (begin-for-syntax
           (set-conversion-from-to! #'Real #'Float #'real->double-flonum)
           (set-conversion-from-to! #'Float #'Real #'identity-macro)
           (set-conversion-from-to! #'from #'to #'conv) ...)))]))

(begin-for-syntax
  (require syntax/id-table racket/dict)
  
  (define upgrade-table (make-free-id-table))
  
  (define (set-upgrade-type! from to)
    (dict-set! upgrade-table from to))
  
  (define (upgrade-type from)
    (if (identifier? from)
        (dict-ref upgrade-table from from)
        from))
  
  (provide set-upgrade-type! upgrade-type))

(define-syntax (with-upgrade-types stx)
  (syntax-case stx ()
    [(_ ([from to] ...))
     (syntax/loc stx
       (begin-for-syntax
         (set-upgrade-type! #'Float #'Real)
         (set-upgrade-type! #'from #'to)
         ...))]))


(define-for-syntax (UpperCamelCaseString str)
  (regexp-replace* #rx"-" (string-titlecase str) ""))

(define-for-syntax (UpperCamelCaseSymbol stx)
  (string->symbol (UpperCamelCaseString (symbol->string (syntax->datum stx)))))

(define-syntax (def-com-method stx)
  (syntax-case stx ()
    [(def (name comName) com (param-type ...) rtype)
     (let-values (((required opt-optional)
                   (splitf-at (syntax->list #'(param-type ...))
                              (lambda (stx)
                                (not (eq? (syntax-e stx) '#:opt))))))
       (let ((optional (if (null? opt-optional) (list) (rest opt-optional)))
             (needs-com-param? (not (syntax->datum #'com)))
             (expand-params (lambda (params opt?)
                              (map (lambda (p)
                                     (syntax-case p ()
                                       [(name type)
                                        (with-syntax ([upgraded-type (upgrade-type #'type)])
                                          (if (not opt?)
                                              (syntax/loc p
                                                (name
                                                 type
                                                 type
                                                 [name : upgraded-type]
                                                 (convert-to-type upgraded-type type name)))
                                              (syntax/loc p
                                                (name
                                                 type
                                                 (U Com-Omit type)
                                                 [name : (U Com-Omit upgraded-type) com-omit]
                                                 (if (com-omit? name) name (convert-to-type upgraded-type type name))))))]))
                                   params))))
         (with-syntax* ([((param type req-type decl conversion) ...)
                         (append (expand-params required #f)
                                 (expand-params optional #t))]
                        [upgraded-rtype (upgrade-type #'rtype)]
                        [comName (symbol->string (syntax->datum #'comName))]
                        [com-invoke-typed
                         (format-id stx "com-invoke-~a" #'name)
                         #;
                         (datum->syntax
                          stx
                          (gensym
                           (format "com-invoke-~a-~a" (syntax->datum #'(type ...)))))])
           (quasisyntax/loc stx
             (begin
               (require/typed #;unsafe-require/typed
                ffi/com
                [(com-invoke com-invoke-typed) (Com-Object String req-type ... -> rtype)])
               (provide name)
               #,(if needs-com-param?
                     #'(define (name [COM : Com-Object] decl ...) : upgraded-rtype
                         (convert-to-type
                          rtype upgraded-rtype
                          (com-invoke-typed COM comName conversion ...)))
                     #'(define (name decl ...) : upgraded-rtype
                         (convert-to-type
                          rtype upgraded-rtype
                          (com-invoke-typed com comName conversion ...)))))))))]
    [(def name com (param-type ...) rtype)
     (with-syntax ([comName (UpperCamelCaseSymbol #'name)])
       (syntax/loc stx
         (def (name comName) com (param-type ...) rtype)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (def-rw-property stx)
  (syntax-case stx ()
    [(def ((name prop-name) obj-type) type)
     (with-syntax ([upgraded-type (upgrade-type #'type)]
                   [prop-name (symbol->string (syntax->datum #'prop-name))])
       (syntax/loc stx
         (begin
           (provide name)
           (define name
             (case-lambda
               (([obj : obj-type])
                (convert-to-type
                 type upgraded-type
                 (cast (com-get-property obj prop-name) type)))
               (([obj : obj-type] [arg : upgraded-type])
                (com-set-property!
                 obj prop-name (convert-to-type upgraded-type type arg))
                (void)))))))]
    [(def (name com) type)
     (with-syntax ([comName (UpperCamelCaseSymbol #'name)])
       (syntax/loc stx
         (def ((name comName) com) type)))]))

(define-syntax (def-ro-property stx)
  (syntax-case stx ()
    [(def ((name prop-name) obj-type) type)
     (with-syntax ([upgraded-type (upgrade-type #'type)]
                   [prop-name (symbol->string (syntax->datum #'prop-name))])
       (syntax/loc stx
         (begin
           (provide name)
           (define (name [obj : obj-type]) : upgraded-type
             (convert-to-type
              type upgraded-type
              (cast (com-get-property obj prop-name) type))))))]
    [(def (name com) type)
     (with-syntax ([comName (UpperCamelCaseSymbol #'name)])
       (syntax/loc stx
         (def ((name comName) com) type)))]))

