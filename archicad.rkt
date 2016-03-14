#lang typed/racket/base/no-check
(require (for-syntax racket/base))
(require (except-in "archicad/backend.rkt" division translating))
(provide (all-from-out "archicad/backend.rkt"))
(require racket/include)
(include "base/macros.rkc")

(require (except-in typed/racket/no-check send random box box?))
(provide (except-out (all-from-out typed/racket/no-check) #%module-begin #%top-interaction)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]))

(define-syntax-rule
  (module-begin form ...)
  (#%module-begin form ... (disconnect)))

(define-syntax-rule
  (top-interaction . form)
  (#%top-interaction . (send form)))
