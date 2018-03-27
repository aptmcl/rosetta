#lang typed/racket/base/no-check
(require typed/racket/unit)
(require (for-syntax racket/base racket/list racket/syntax)
         (only-in racket/base [box rk:box]))

(struct bim-family
  ([path : String]
   [map : (Listof (Cons Keyword (Option Any)))]
   [layer-name : String]
   [layer-ref : (Box (Option Layer))]
   [properties : Any])
  #:type-name BIM-Family)

(provide def-bim-family
         bim-family-layer-ref
         bim-family-layer-name)

(define (layer-name-from-path [path : String]) : Layer
  (let ((revit-name (regexp-match #rx"([a-zA-Z]+).rfa" path)))
    (or (and revit-name (last revit-name))
        path)))

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
             (define default-name (make-parameter (struct-name "" '() layer-name (rk:box #f) (list) param-default ...)))
             (define (load-name [path : String]
                                #:properties [properties : Any #f]
                                . class-params)
               (struct-name path
                            (list (cons 'param-key param-name) ...)
                            (layer-name-from-path path)
                            (rk:box #f)
                            properties
                            param-default ...))
             (define (instance-name [family : BIM-Family]
                                    #:properties [properties : Any (bim-family-properties family)]
                                    . instance-params)
               (struct-name (bim-family-path family)
                            (bim-family-map family)
                            (bim-family-layer-name family)
                            (bim-family-layer-ref family)
                            properties
                            param-name ...))))))]))

(def-bim-family beam
  ([width : Real 0.1]
   [height : Real 0.1]
   [profile : String ""]
   [material : (Option String) #f]))

(def-bim-family slab
  ([thickness : Real 0.3]
   [coating-thickness : Real 0]))

(def-bim-family roof
  ([thickness : Real 0.3]
   [coating-thickness : Real 0]))

(def-bim-family wall
  ([thickness : Real 0.2]))

(def-bim-family door
  ([width : (Option Real) 0.8]
   [height : (Option Real) 2.0]))

(def-bim-family window
  ([width : (Option Real) 1.0]
   [height : (Option Real) 1.0]))

(def-bim-family column
  ([width : Real 0.1]
   [depth : (Option Real) #f]
   [circular-section? : Boolean #f]))

(def-bim-family panel
  ([thickness : Real 0.01]
   [material : String "Glass"]))

(provide (struct-out node-support))
(struct node-support
  ([name : String]
   [ux : Boolean]
   [uy : Boolean]
   [uz : Boolean]
   [rx : Boolean]
   [ry : Boolean]
   [rz : Boolean]
   [created? : Boolean])
  #:mutable)

(provide create-node-support)
(define (create-node-support [name : String]
                             #:ux [ux : Boolean #f]
                             #:uy [uy : Boolean #f]
                             #:uz [uz : Boolean #f]
                             #:rx [rx : Boolean #f]
                             #:ry [ry : Boolean #f]
                             #:rz [rz : Boolean #f])
  (node-support name ux uy uz rx ry rz #f))

(def-bim-family truss-node
  ([radius : Real 0.1]
   [support : (Option node-support) #f]))

(def-bim-family truss-bar
  ([created? : (Box Boolean) (rk:box #f)]
   [radius : Real 0.03]
   [section : (Option Any) #f]
   [material : (Option Any) #f]))
