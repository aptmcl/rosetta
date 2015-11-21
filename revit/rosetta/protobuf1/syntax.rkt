#lang racket/base
;; This file is part of Protocol Buffers for Racket.
;; Copyright (c) 2012 by Thomas Chust <chust@web.de>
;;
;; Protocol Buffers for Racket is free software: you can redistribute
;; it and/or modify it under the terms of the GNU Lesser General
;; Public License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;;
;; Protocol Buffers for Racket is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU Lesser General Public License for more
;; details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with Protocol Buffers for Racket. If not, see
;; <http://www.gnu.org/licenses/>.
(require
 (for-syntax
  srfi/26
  racket/base
  racket/list
  racket/struct-info
  racket/syntax)
 srfi/26
 racket/promise
 racket/set
 "encoding.rkt"
 "reflection.rkt")

(define-syntax (define-primitive-type stx)
  (syntax-case stx ()
    [(define-primitive-type name type
       reader writer)
     (with-syntax ([primitive:info
                    (datum->syntax stx (format-symbol "primitive:~a" #'name))])
       #'(define primitive:info
           (primitive-info 'name 'type reader writer)))]))

(define-primitive-type int32 int*
  read-int* write-int*)
(define-primitive-type int64 int*
  read-int* write-int*)
(define-primitive-type uint32 int*
  read-uint* write-uint*)
(define-primitive-type uint64 int*
  read-uint* write-uint*)
(define (primitive:uint* max-size)
  (primitive-info
   'uint* 'int*
   (cut read-uint* <> max-size)
   (cut write-uint* <> <> max-size)))
(define-primitive-type sint32 int*
  read-sint* write-sint*)
(define-primitive-type sint64 int*
  read-sint* write-sint*)
(define (primitive:sint* max-size)
  (primitive-info
   'sint* 'int*
   (cut read-sint* <> max-size)
   (cut write-sint* <> <> max-size)))
(define-primitive-type fixed32 32bit
  read-fixed32 write-fixed32)
(define-primitive-type fixed64 64bit
  read-fixed64 write-fixed64)
(define-primitive-type sfixed32 32bit
  read-sfixed32 write-sfixed32)
(define-primitive-type sfixed64 64bit
  read-sfixed64 write-sfixed64)
(define-primitive-type bool int*
  read-bool write-bool)
(define-primitive-type float 32bit
  read-float write-float)
(define-primitive-type double 64bit
  read-double write-double)
(define-primitive-type bytes sized
  read-sized-bytes write-sized-bytes)
(define-primitive-type string sized
  read-sized-string write-sized-string)

(provide
 primitive:int32 primitive:int64
 primitive:uint32 primitive:uint64 primitive:uint*
 primitive:sint32 primitive:sint64 primitive:sint*
 primitive:fixed32 primitive:fixed64
 primitive:sfixed32 primitive:sfixed64
 primitive:bool
 primitive:float primitive:double
 primitive:bytes primitive:string)

(define-syntax (define-enum-type stx)
  (syntax-case stx ()
    [(define-enum-type name
       ([alt tag] ...))
     (with-syntax ([enum:info
                    (datum->syntax stx (format-symbol "enum:~a" #'name))]
                   [integer->enum
                    (datum->syntax stx (format-symbol "integer->~a" #'name))]
                   [enum->integer
                    (datum->syntax stx (format-symbol "~a->integer" #'name))])
       #'(begin
           (define (integer->enum i)
             (case i
               [(tag) 'alt] ...
               [else (error 'integer->enum "unknown enumeration tag: ~e" i)]))
           (define (enum->integer s)
             (case s
               [(alt) tag] ...
               [else (error 'enum->integer "unknown enumeration value: ~e" s)]))
           (define enum:info
             (enum-info 'name integer->enum enum->integer))))]))

(define-syntax required-tags
  (syntax-rules (required)
    [(required-tags (required tag) . more)
     (cons tag (required-tags . more))]
    [(required-tags (_ tag) . more)
     (required-tags . more)]
    [(required-tags)
     null]))

(define-syntax (define-message-type stx)
  (syntax-case stx ()
    [(define-message-type name
       ([label type field tag . default] ...))
     (let ([fields (syntax-e #'(field ...))]
           [defaults (syntax-e #'(default ...))])
       (with-syntax ([nfields
                      (length fields)]
                     [(field-arg ...)
                      (append*
                       (for/list ([s (in-list fields)] [v (in-list defaults)])
                         (list
                          (string->keyword (symbol->string (syntax->datum s)))
                          (list s #'(void)))))]
                     [constructor
                      (datum->syntax stx (format-symbol "~a*" #'name))]
                     [(field-default ...)
                      (for/list ([v (in-list defaults)])
                        (if (null? (syntax->datum v)) #'((void)) v))]
                     [(accessor ...)
                      (datum->syntax
                       stx (map (cut format-symbol "~a-~a" #'name <>)
                                fields))]
                     [(mutator ...)
                      (datum->syntax
                       stx (map (cut format-symbol "set-~a-~a!" #'name <>)
                                fields))])
         #'(begin
             (struct name message
               ([field #:auto] ...)
               #:transparent
               #:mutable
               #:auto-value (void)
               #:property prop:protobuf
               (delay
                 (message-info
                  'name constructor
                  (make-immutable-hasheqv
                   (list
                    (cons
                     tag
                     (field-info
                      type (memq 'label '(repeated packed)) (eq? 'label 'packed)
                      accessor mutator))
                    ...))
                  (list->seteqv
                   (required-tags (label tag) ...)))))
             (define (constructor field-arg ...)
               (let ([msg (name (hasheqv))])
                 (unless (void? field) (mutator msg field)) ...
                 msg))
             (set! accessor
               (let ([message-ref accessor]
                     [default-thunk
                      (case 'label
                        [(required)
                         (λ () (error 'accessor "missing required field"))]
                        [(optional)
                         (λ () . field-default)]
                        [(repeated packed)
                         null])])
                 (λ (msg [alt default-thunk])
                   (let ([v (message-ref msg)])
                     (cond
                       [(not (void? v)) v]
                       [(procedure? alt) (alt)]
                       [else alt])))))
             ...)))]))

(define-syntax (define-message-extension stx)
  (syntax-case stx ()
    [(define-message-type name
       [label type field tag . default])
     (with-syntax ([(struct:info _ message? _ _ _)
                    (extract-struct-info (syntax-local-value #'name))]
                   [field-default
                    (if (null? (syntax->datum #'default)) #'((void)) #'default)]
                   [accessor
                    (datum->syntax
                     stx (format-symbol "~a-~a" #'name #'field))]
                   [mutator
                    (datum->syntax
                     stx (format-symbol "set-~a-~a!" #'name #'field))])
       #'(begin
           (define accessor
             (let ([default-thunk
                     (case 'label
                       [(required)
                        (λ () (error 'accessor "missing required field"))]
                       [(optional)
                        (λ () . field-default)]
                       [(repeated)
                        null])])
               (λ (msg [alt default-thunk])
                 (unless (message? msg)
                   (raise-type-error 'accessor (symbol->string name) msg))
                 (let ([v (hash-ref (message-extensions msg) tag void)])
                   (cond
                     [(not (void? v)) v]
                     [(procedure? alt) (alt)]
                     [else alt])))))
           (define (mutator msg v)
             (unless (message? msg)
               (raise-type-error 'mutator (symbol->string name) msg))
             (set-message-extensions! msg
               (hash-set (message-extensions msg) tag v)))
           (let ([info (force (protobuf-ref struct:info))])
             (set-message-info-fields! info
               (hash-set (message-info-fields info) tag
                 (field-info
                  type (memq 'label '(repeated packed)) (eq? 'label 'packed)
                  accessor mutator)))
             (set-message-info-required! info
               (set-union
                (message-info-required info)
                (list->seteqv
                 (required-tags (label tag))))))))]))

(provide
 define-enum-type
 define-message-type
 define-message-extension)
