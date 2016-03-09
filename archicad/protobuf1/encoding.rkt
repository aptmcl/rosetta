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
 racket/port
 syntax/readerr)

(define (read-uint* [port (current-input-port)] [max-size 10])
  (let-values ([(line col pos) (port-next-location port)])
    (let loop ([span 0])
      (if (and max-size (>= span max-size))
          (raise-read-error
           (format "~s: maximum integer size ~v exceeded" 'read-uint* max-size)
           (object-name port)
           line col pos span)
          (let ((b (read-byte port)))
            (if (and (not (eof-object? b)) (bitwise-bit-set? b 7))
                (+ (bitwise-and b #x7f)
                   (* 128 (loop (add1 span))))
                b))))))

(define (write-uint* n [port (current-output-port)] [max-size 10])
  (let loop ([n n] [span 0])
    (if (and max-size (>= span max-size))
        (error
         'write-uint* "maximum integer size ~v exceeded"
         max-size)
        (let*-values ([(r b) (quotient/remainder n 128)]
                      [(last?) (zero? r)])
          (write-byte (if last? b (bitwise-ior #x80 b)) port)
	  (unless last? (loop r (add1 span)))))))

(provide/contract
 [read-uint*
  (->* ()
       (input-port? (or/c exact-positive-integer? #f))
       (or/c exact-nonnegative-integer? eof-object?))]
 [write-uint*
  (->* (exact-nonnegative-integer?)
       (output-port? (or/c exact-positive-integer? #f))
       any)])

(define (read-sint* [port (current-input-port)] [max-size 10])
  (let ([z (read-uint* port max-size)])
    (if (eof-object? z)
        z
        (/ (if (odd? z) (- -1 z) z)
           2))))

(define (write-sint* i [port (current-output-port)] [max-size 10])
  (let ([2i (* 2 i)])
    (write-uint* (if (negative? i) (- -1 2i) 2i) port max-size)))

(provide/contract
 [read-sint*
  (->* ()
       (input-port? (or/c exact-positive-integer? #f))
       (or/c exact-integer? eof-object?))]
 [write-sint*
  (->* (exact-integer?)
       (output-port? (or/c exact-positive-integer? #f))
       any)])

(define (read-int* [port (current-input-port)])
  (let ([n (read-uint* port)])
    (if (eof-object? n)
	n
        (if (positive? (- n #x8000000000000000))
            (- n #x10000000000000000)
            n))))

(define (write-int* i [port (current-output-port)])
  (write-uint* (if (negative? i) (+ i #x10000000000000000) i) port))

(provide/contract
 [read-int*
  (->* ()
       (input-port?)
       (or/c exact-integer? eof-object?))]
 [write-int*
  (->* (exact-integer?)
       (output-port?)
       any)])

(define (read-bool [port (current-input-port)])
  (let ([n (read-uint* port)])
    (if (eof-object? n) n (not (zero? n)))))

(define (write-bool v [port (current-output-port)])
  (write-uint* (if v 1 0) port))

(provide/contract
 [read-bool
  (->* ()
       (input-port?)
       (or/c boolean? eof-object?))]
 [write-bool
  (->* (any/c)
       (output-port?)
       any)])

(define ((read-fixed* size signed?) [port (current-input-port)])
  (let*-values ([(line col pos) (port-next-location port)]
                [(bstr) (read-bytes size port)])
    (if (eof-object? bstr)
        bstr
        (let ([span (bytes-length bstr)])
          (if (< span size)
              (raise-read-error
               (format "~s: found truncated fixed integer bytes" 'read-fixed*)
               (object-name port)
               line col pos span)
              (integer-bytes->integer bstr signed? #f))))))

(define read-fixed32
  (read-fixed* 4 #f))
(define read-fixed64
  (read-fixed* 8 #f))
(define read-sfixed32
  (read-fixed* 4 #t))
(define read-sfixed64
  (read-fixed* 8 #t))

(define ((write-fixed* size signed?) n [port (current-output-port)])
  (let ([bstr (integer->integer-bytes n size signed? #f)])
    (write-bytes bstr port)))

(define write-fixed32
  (write-fixed* 4 #f))
(define write-fixed64
  (write-fixed* 8 #f))
(define write-sfixed32
  (write-fixed* 4 #t))
(define write-sfixed64
  (write-fixed* 8 #t))

(provide/contract
 [read-fixed32
  (->* ()
       (input-port?)
       (or/c exact-nonnegative-integer? eof-object?))]
 [read-fixed64
  (->* ()
       (input-port?)
       (or/c exact-nonnegative-integer? eof-object?))]
 [read-sfixed32
  (->* ()
       (input-port?)
       (or/c exact-integer? eof-object?))]
 [read-sfixed64
  (->* ()
       (input-port?)
       (or/c exact-integer? eof-object?))]
 [write-fixed32
  (->* (exact-nonnegative-integer?)
       (output-port?)
       any)]
 [write-fixed64
  (->* (exact-nonnegative-integer?)
       (output-port?)
       any)]
 [write-sfixed32
  (->* (exact-integer?)
       (output-port?)
       any)]
 [write-sfixed64
  (->* (exact-integer?)
       (output-port?)
       any)])

(define ((read-float* size) [port (current-input-port)])
  (let*-values ([(line col pos) (port-next-location port)]
                [(bstr) (read-bytes size port)])
    (if (eof-object? bstr)
        bstr
        (let ([span (bytes-length bstr)])
          (if (< span size)
              (raise-read-error
               (format "~s: found truncated fixed floating point bytes" 'read-float*)
               (object-name port)
               line col pos span)
              (floating-point-bytes->real bstr #f))))))

(define read-float
  (read-float* 4))
(define read-double
  (read-float* 8))

(define ((write-float* size) x [port (current-output-port)])
  (let ([bstr (real->floating-point-bytes x size #f)])
    (write-bytes bstr port)))

(define write-float
  (write-float* 4))
(define write-double
  (write-float* 8))

(provide/contract
 [read-float
  (->* ()
       (input-port?)
       (or/c real? eof-object?))]
 [read-double
  (->* ()
       (input-port?)
       (or/c real? eof-object?))]
 [write-float
  (->* (real?)
       (output-port?)
       any)]
 [write-double
  (->* (real?)
       (output-port?)
       any)])

(define (read-sized-bytes [port (current-input-port)])
  (let*-values ([(line col pos) (port-next-location port)]
                [(size) (read-uint* port)])
    (if (eof-object? size)
        size
        (let ([bstr (read-bytes size port)])
          (if (or (eof-object? bstr) (< (bytes-length bstr) size))
              (let-values ([(line1 col1 pos1) (port-next-location port)])
                (raise-read-error
                 (format "~s: found truncated bytes" 'read-sized-bytes)
                 (object-name port)
                 line col pos (and pos pos1 (- pos1 pos))))
              bstr)))))

(define (write-sized-bytes bstr [port (current-output-port)])
  (write-uint* (bytes-length bstr) port)
  (write-bytes bstr port))

(define (read-sized-string [port (current-input-port)])
  (let ([bstr (read-sized-bytes port)])
    (if (eof-object? bstr)
        bstr
        (bytes->string/utf-8 bstr))))

(define (write-sized-string str [port (current-output-port)])
  (write-sized-bytes (string->bytes/utf-8 str) port))

(provide/contract
 [read-sized-bytes
  (->* ()
       (input-port?)
       (or/c bytes? eof-object?))]
 [write-sized-bytes
  (->* (bytes?)
       (output-port?)
       any)]
 [read-sized-string
  (->* ()
       (input-port?)
       (or/c string? eof-object?))]
 [write-sized-string
  (->* (string?)
       (output-port?)
       any)])

(define (read-sized read [port (current-input-port)])
  (let*-values ([(line col pos) (port-next-location port)]
                [(size) (read-uint* port)])
    (if (eof-object? size)
        size
        (let ([v (read (make-limited-input-port port size #f))])
          (if (eof-object? v)
              (let-values ([(line1 col1 pos1) (port-next-location port)])
                (raise-read-error
                 (format "~s: found truncated data" 'read-sized)
                 (object-name port)
                 line col pos (and pos pos1 (- pos1 pos))))
              v)))))

(define (write-sized write v [port (current-output-port)])
  (let ([bstr (call-with-output-bytes (cut write v <>))])
    (write-uint* (bytes-length bstr) port)
    (write-bytes bstr port)))

(provide/contract
 [read-sized
  (->* ((-> input-port? any/c))
       (input-port?)
       any/c)]
 [write-sized
  (->* ((-> any/c output-port? any) any/c)
       (output-port?)
       any)])

(define (read-tag/type [port (current-input-port)])
  (let-values ([(line col pos) (port-next-location port)]
               [(tag/type) (read-uint* port)])
    (if (eof-object? tag/type)
        (values tag/type
                tag/type)
        (values (arithmetic-shift tag/type -3)
                (let ([type (bitwise-and tag/type #b111)])
                  (case type
                    [(0) 'int*]
                    [(1) '64bit]
                    [(5) '32bit]
                    [(2) 'sized]
                    [else
                     (let-values ([(line1 col1 pos1) (port-next-location port)])
                       (raise-read-error
                        (format "~s: found unknown field type: ~e" 'read-tag/type type)
                        (object-name port)
                        line col pos (and pos pos1 (- pos1 pos))))]))))))

(define (write-tag/type tag type [port (current-output-port)])
  (write-uint*
   (bitwise-ior
    (arithmetic-shift tag 3)
    (case type
      [(int*) 0]
      [(64bit) 1]
      [(32bit) 5]
      [(sized) 2]))
   port))

(define type/c
  (symbols 'int* '64bit '32bit 'sized))

(provide
 type/c)
(provide/contract
 [read-tag/type
  (->* ()
       (input-port?)
       (values (or/c exact-nonnegative-integer? eof-object?)
               (or/c type/c eof-object?)))]
 [write-tag/type
  (->* (exact-nonnegative-integer? type/c)
       (output-port?)
       any)])
