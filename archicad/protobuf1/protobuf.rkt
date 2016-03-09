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
 srfi/26
 racket/contract
 racket/match
 racket/port
 racket/promise
 racket/set
 syntax/readerr
 "encoding.rkt"
 "reflection.rkt")

(provide
 (struct-out message))

(define ((appender accessor mutator) msg v)
  (mutator msg (append (accessor msg null) (if (list? v) v (list v)))))

(define (deserialize type/msg [port (current-input-port)])
  (let ([info (force (protobuf-ref type/msg))])
    (letrec ([msg (if (struct-type? type/msg) ((message-info-constructor info)) type/msg)]
             [fields (message-info-fields info)]
             [required (message-info-required info)]
             [unknown (open-output-bytes)])
      (let loop ()
        (let*-values ([(line col pos) (port-next-location port)]
                      [(tag type) (read-tag/type port)])
          (unless (or (eof-object? tag) (eof-object? type))
            (set! required (set-remove required tag))
            (cond
              [(hash-ref fields tag #f)
               => (match-lambda
                    [(field-info (primitive-info _ ptype read _) repeated? _
                                 accessor mutator)
                     ((if repeated? (appender accessor mutator) mutator)
                      msg
                      (cond
                        [(eq? type ptype)
                         (read port)]
                        [(and repeated? (eq? type 'sized) (not (eq? ptype 'sized)))
                         (read-sized (cut port->list read <>) port)]
                        [else
                         (let-values ([(line1 col1 pos1) (port-next-location port)])
                           (raise-read-error
                            (format "~s: wire type does not match declared type: ~e"
                                    'deserialize type)
                            (object-name port)
                            line col pos (and pos pos1 (- pos1 pos))))]))]
                    [(field-info (enum-info _ integer->enum _) repeated? _
                                 accessor mutator)
                     ((if repeated? (appender accessor mutator) mutator)
                      msg
                      (cond
                        [(eq? type 'int*)
                         (integer->enum (read-int* port))]
                        [(and repeated? (eq? type 'sized))
                         (map integer->enum
                              (read-sized (cut port->list read-int* <>) port))]
                        [else
                         (let-values ([(line1 col1 pos1) (port-next-location port)])
                           (raise-read-error
                            (format "~s: wire type does not match declared type: ~e"
                                    'deserialize type)
                            (object-name port)
                            line col pos (and pos pos1 (- pos1 pos))))]))]
                    [(field-info (? struct-type? stype) repeated? _
                                 accessor mutator)
                     ((if repeated? (appender accessor mutator) mutator)
                      msg
                      (cond
                        [(eq? type 'sized)
                         (let ([proto
                                (if repeated?
                                    stype
                                    (let ([proto (accessor msg)])
                                      (if (void? proto) stype proto)))])
                           (read-sized (cut deserialize proto <>) port))]
                        [else
                         (let-values ([(line1 col1 pos1) (port-next-location port)])
                           (raise-read-error
                            (format "~s: wire type does not match declared type: ~e"
                                    'deserialize type)
                            (object-name port)
                            line col pos (and pos pos1 (- pos1 pos))))]))])]
              [else
               (write-tag/type tag type unknown)
               (case type
                 [(int*)
                  (write-uint* (read-uint* port) unknown)]
                 [(64bit)
                  (copy-port (make-limited-input-port port 8 #f) unknown)]
                 [(32bit)
                  (copy-port (make-limited-input-port port 4 #f) unknown)]
                 [(sized)
                  (let ([size (read-uint* port)])
                    (write-uint* size unknown)
                    (copy-port (make-limited-input-port port size #f) unknown))])])
            (loop))))
      (set-message-unknown! msg (get-output-bytes unknown))
      (unless (set-empty? required)
        (error 'deserialize "missing required fields: ~e" required))
      msg)))

(define (serialize msg [port (current-output-port)])
  (let ([info (force (protobuf-ref msg))])
    (let ([fields (message-info-fields info)]
          [required (message-info-required info)])
      (for ([tag (sort (hash-keys fields) <)]
	    #:when #t
	    [field (in-value (hash-ref fields tag))]
            #:when #t
            [vs (in-value ((field-info-accessor field) msg))]
            #:when (not (void? vs))
            [repeated? (in-value (field-info-repeated? field))]
            [packed? (in-value (field-info-packed? field))]
            #:when #t
            [v ((if (and repeated? (not packed?)) in-list in-value) vs)])
        (set! required (set-remove required tag))
        (match (field-info-type field)
          [(primitive-info _ ptype _ write)
           (cond
             [(and repeated? packed?)
              (when (eq? ptype 'sized)
                (error 'serialize "cannot apply packed encoding to sized type"))
              (write-tag/type tag 'sized port)
              (write-sized
               (cut for-each write <> <>) vs port)]
             [else
              (write-tag/type tag ptype port)
              (write v port)])]
          [(enum-info _ _ enum->integer)
           (cond
             [(and repeated? packed?)
              (write-tag/type tag 'sized port)
              (write-sized
               (cut for-each write-int* <> <>) (map enum->integer vs) port)]
             [else
              (write-tag/type tag 'int* port)
              (write-int* (enum->integer v) port)])]
          [_
           (write-tag/type tag 'sized port)
           (write-sized serialize v port)]))
      (write-bytes (message-unknown msg) port)
      (unless (set-empty? required)
        (error 'serialize "missing required fields: ~e" required)))))

(provide/contract
 [deserialize
  (->* ((or/c struct-type? struct?))
       (input-port?)
       any)]
 [serialize
  (->* (struct?)
       (output-port?)
       any)])
