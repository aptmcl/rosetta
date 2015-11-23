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

(struct type-info
  (name)
  #:transparent)

(struct primitive-info type-info
  (type reader writer))

(struct enum-info type-info
  (integer->enum enum->integer))

(define-values (prop:protobuf protobuf? protobuf-ref)
  (make-struct-type-property 'protobuf 'can-impersonate))

(struct message-info type-info
  (constructor
   [fields #:mutable] [required #:mutable]))

(struct field-info
  (type
   repeated? packed?
   accessor mutator))

(provide
 (struct-out type-info)
 (struct-out primitive-info)
 (struct-out enum-info)
 prop:protobuf protobuf? protobuf-ref
 (struct-out message-info)
 (struct-out field-info))

(struct message
  (extensions
   [unknown #:auto])
  #:transparent
  #:mutable
  #:auto-value #"")

(provide
 (struct-out message))
