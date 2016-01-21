#lang typed/racket/base/no-check
(require (for-syntax racket/base))
(require (except-in "glfast/backend.rkt" division translating))
(provide (all-from-out "glfast/backend.rkt"))
(require racket/include)
(include "base/macros.rkc")