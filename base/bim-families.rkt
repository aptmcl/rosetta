#lang typed/racket/base/no-check
(require typed/racket/unit)
(require (for-syntax racket/base racket/list racket/syntax)
         (only-in racket/base [box rk:box]))

(struct bim-family
  ([path : String]
   [map : (Listof (Cons Keyword (Option Any)))]
   [layer-name : String]
   [layer-ref : (Box (Option Layer))])
  #:type-name BIM-Family)

(provide bim-family-layer-ref
         bim-family-layer-name)

#;(provide ;;BIM extensions
 beam
 column
 door
 roof
 roof-rectangle
 slab
 slab-rectangle
 wall)



(define (layer-name-from-path [path : String]) : Layer
  (last (regexp-match #rx"([a-zA-Z]+).rfa" path)))

(define-for-syntax (build-name fmt id)
  (format-id id #:source id fmt (syntax-e id)))

(define-for-syntax (build-keyword id)
  (string->keyword (symbol->string (syntax->datum id))))

(define-syntax (def-bim-family stx)
  (syntax-case stx ()
    [(def name (param ...))
     (with-syntax ([struct-name (build-name "~A-family" #'name)]
                   [instance-name (build-name "~A-family-element" #'name)]
                   [load-name (build-name "load-~A-family" #'name)]
                   [default-name (build-name "default-~A-family" #'name)]
                   [layer-name (string-titlecase (symbol->string (syntax-e #'name)))]
                   [([param-name param-key param-type param-default class-comb instance-comb] ...)
                    (map (lambda (p)
                           (syntax-case p (:)
                             [[name : type default]
                              (with-syntax ([key (build-keyword #'name)])
                                #'[name key type default
                                        (key [name : (Option Any) #f])
                                        (key [name : type default])])]
                             #;[[name : type] #'[name type]]))
                         (syntax->list #'(param ...)))])
       (with-syntax ([class-params
                      (append* (map syntax->list (syntax->list #'(class-comb ...))))]
                     [instance-params
                      (append* (map syntax->list (syntax->list #'(instance-comb ...))))])
         (syntax/loc stx
           (begin
             (provide (except-out (struct-out struct-name) struct-name) ;;to avoid conflicts
                      default-name
                      load-name
                      instance-name)
             (struct struct-name bim-family
               ([param-name : param-type] ...))
             (define default-name (make-parameter (struct-name "" '() layer-name (rk:box #f) param-default ...)))
             (define (load-name [path : String] . class-params)
               (struct-name path
                            (list (cons 'param-key param-name) ...)
                            (layer-name-from-path path)
                            (rk:box #f)
                            param-default ...))
             (define (instance-name [family : BIM-Family] . instance-params)
               (struct-name (bim-family-path family)
                            (bim-family-map family)
                            (bim-family-layer-name family)
                            (bim-family-layer-ref family)
                            param-name ...))))))]))

(def-bim-family beam
  ([width : Real 10]
   [height : Real 10]))

(def-bim-family slab
  ([thickness : Real 0.3]))

(def-bim-family roof
  ([thickness : Real 0.3]))

(def-bim-family wall
  ([thickness : Real 1]))

(def-bim-family door
  ([width : (Option Real) #f]
   [height : (Option Real) #f]))

(def-bim-family column
  ([width : Real 10]
   [depth : (Option Real) #f]))

#;#;
(define (slab-rectangle [p : Loc] [length : Real] [width : Real] [level : Level (current-level)] [family : Slab-Family (default-slab-family)])
  (slab (list p (+x p length) (+xy p length width) (+y p width))
        level
        family))

(define (roof-rectangle [p : Loc] [length : Real] [width : Real] [level : Level (current-level)] [family : Roof-Family (default-roof-family)])
  (roof (list p (+x p length) (+xy p length width) (+y p width))
        level
        family))
