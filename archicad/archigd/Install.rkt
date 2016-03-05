#lang racket
(provide (except-out (all-defined-out)
                     ))

(require racket/runtime-path)

(define addon-in-use 32)

(define-runtime-path base (build-path 'up))

(define from-addon (build-path base "x64" "Geometry_Test.apx"))

(define to-folder (string->some-system-path "C:\\Program Files\\GRAPHISOFT\\ArchiCAD 18\\Add-ons" 'windows)) 

;TODO - Add more errors
(define (move-addon)
  (with-handlers ([exn:fail? (lambda (exn)
                               (cond [(eq? addon-in-use (car (exn:fail:filesystem:errno-errno exn)))
                                      (displayln "Warning: Addon cannot be moved because it is being used")]
                                     [else (raise exn)]))])
    (copy-file from-addon (build-path to-folder "Geometry_Test.apx") #t)))

#|
(define (move-addon)
  (let ((from-addon (build-path base "x64" "Geometry_Test.apx"))
        (to-folder (string->some-system-path "C:\\Program Files\\GRAPHISOFT\\ArchiCAD 18\\Add-Ons" 'windows)))
    (when (and (file-exists? from-addon)
               (directory-exists? to-folder))
      (copy-file from-addon (build-path to-folder "Geometry_Test.apx") #t))))
|#

#|
(define (move-addon-file)
  (let ((internal-path-addon (build-path base "x64" "Geometry_Test.apx"))
        ;(internal-path-directory "D:/GRAPHISOFT/ArchiCAD 18/Add-Ons")
        (internal-path-directory base)
        (internal-path-directory-addon (build-path base 'up 'up 'up 'up 'up 'up 'up 'up "Geometry_Test.apx"))
        ;(internal-path-directory (string->some-system-path "C:" 'windows))
        ;(internal-path-directory "C:/Program Files/GRAPHISOFT/ArchiCAD 18/Add-Ons")
        ;(internal-path-directory-addon (string->some-system-path "C:\\Geometry_Test.apx" 'windows))

        #;(internal-path-directory-addon "C:/Program Files/GRAPHISOFT/ArchiCAD 18/Add-Ons/Geometry_Test.apx"))
    (when (and (directory-exists? internal-path-directory)
               (file-exists? internal-path-addon))
      (copy-file internal-path-addon internal-path-directory-addon #t))))
(move-addon-file)
|#