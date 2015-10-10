#lang racket/base
(require ffi/com)

(define (com-omit? v)
  (eq? v com-omit))

(provide (all-from-out ffi/com) com-omit?)
