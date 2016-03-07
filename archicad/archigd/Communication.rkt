#lang racket
(provide (except-out (all-defined-out)
                     ))

(require "protobuf1/protobuf.rkt")
(require "protobuf1/encoding.rkt")
(require "Messages.rkt")
(require rosetta/revit)
(require srfi/26)

(define visual-feedback? (make-parameter #f))

(define input #f)
(define output #f)
(define server-addr "localhost")

(define (set-input! new)
  (set! input new))

(define (set-output! new)
  (set! output new))

(define (connect)
  (ensure-connection))

(define (ensure-connection)
  (define time-out-tries 10)
  (let rec ((n time-out-tries))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (if (> n 0)
                           (begin (displayln (string-append "[" (number->string (+ 1 (- time-out-tries n))) "/" (number->string time-out-tries) "] "
                                                            "You must first use the connect button on ArchiCAD.\n Try in the top bar, Addon->Rosetta"))
                                  (sleep 2)
                                  (rec (- n 1)))
                           (raise e)))])
      (start-connection))))

(define (start-connection)
  (begin
    (call-with-values(lambda () (tcp-connect server-addr 53800))
                     (lambda (a b)
                       ;(set! input a)
                       ;(set! output b)
                       (set-input! a)
                       (set-output! b)
                       (file-stream-buffer-mode input 'none)
                       (file-stream-buffer-mode output 'none)
                       ;(set! current-level (make-parameter (check-level)))
                       (set-current-level! (make-parameter (check-level)))
                       ))))

;;Function to quit
(define (disconnect)
  (write-msg-name "quit")
  "quit successful")

;;Usage: (send (create-...) (create-...) ... )
(define-syntax-rule (send expr ...)
  (begin
    (connect)
    (parameterize ((current-level (check-level)))
      expr ...)
    (disconnect)))

;;Function to send name 
(define (write-msg-name name)
  (write-sized serialize (namemessage* #:name name) output))

;;Function to call 
;;a function with a 'name' and 'strct'
(define (write-msg name strct)
  (write-msg-name name)
  (write-sized serialize strct output))

;;Function to enable 3D visual feedback
(define (visual-feedback-on)
  (write-msg-name "RefreshOn"))

;;Function to disable 3D visual feedback
(define (visual-feedback-off)
  (write-msg-name "RefreshOff"))

;;Function to send a double
(define (send-double d)
  (write-sized serialize (doublemessage* #:d d) output))

;;Function to send list of points
(define (send-list-points lst)
  (for-each (lambda (point)
              (write-sized serialize (pointmessage* #:p0x (car point) 
                                                    #:p0y (cdr point)) output)) 
            lst))

;;Function to send list of points using repeated fields
(define (send-points-old lst)
  (let ((lst-x (list))
        (lst-y (list))
        (lst-z (list)))
    (for-each (lambda (point)
                (set! lst-x (append lst-x (list (car point))))
                (set! lst-y (append lst-y (list (cdr point)))))
              lst)
    (write-sized serialize (pointsmessage* #:px lst-x 
                                           #:py lst-y 
                                           #:pz lst-z) output)))

;;Function to send list of points using repeated fields with XYZ Rosetta implementation
(define (send-points lst)
  (let ((lst-x (list))
        (lst-y (list))
        (lst-z (list)))
    (for-each (lambda (point)
                (set! lst-x (append lst-x (list (cx point))))
                (set! lst-y (append lst-y (list (cy point))))
                (set! lst-z (append lst-z (list (cz point)))))
              lst)
    (write-sized serialize (pointsmessage* #:px lst-x 
                                           #:py lst-y 
                                           #:pz lst-z) output)))

;;Function to send list of arcs
(define (send-list-arcs lst)
  (for-each (lambda (arc)
              (write-sized serialize (polyarcmessage* #:begindex (car arc) 
                                                      #:endindex (car (cdr arc)) 
                                                      #:arcangle (car (cdr (cdr arc)))) output)) 
            lst))

;;Function to send list of arcs using repeated fields
(define (send-arcs-complex lst)
  (let ((lst-beg-index (list))
        (lst-end-index (list))
        (lst-arc-angle (list)))
    (for-each (lambda (arc)
                (set! lst-beg-index (append lst-beg-index (list (car arc))))
                (set! lst-end-index (append lst-end-index (list (car (cdr arc)))))
                (set! lst-arc-angle (append lst-arc-angle (list (car (cdr (cdr arc)))))))
              lst)
    (write-sized serialize (polyarcsmessage* #:begindex lst-beg-index 
                                             #:endindex lst-end-index 
                                             #:arcangle lst-arc-angle) output)))

#|
 Function to send list of arcs with assumed order
 The first arc will be applied to the line that is formed 
 by the first and second point. The logic is applied to
 the following arcs.
 Ex.: List with two arcs, will apply the first arc to
      line(p1, p2), and the second arc to line(p2, p3)
|#
(define (send-arcs lst)
  (let ((lst-beg-index (list))
        (lst-end-index (list))
        (lst-arc-angle (list))
        (index 1))
    (for-each (lambda (arc)
                (set! lst-beg-index (append lst-beg-index (list index)))
                (set! lst-end-index (append lst-end-index (list (+ index 1))))
                (set! lst-arc-angle (append lst-arc-angle (list  arc)))
                (set! index (+ index 1)))
              lst)
    (write-sized serialize (polyarcsmessage* #:begindex lst-beg-index 
                                             #:endindex lst-end-index 
                                             #:arcangle lst-arc-angle) output)))

#|
Function to update file materials
The file is merely used for consulting
(send (update-material-file))
|#
(define (update-material-file)
  (write-msg-name "WriteMaterialsFile"))

(define (refresh-3d-view)
  (write-msg-name "Refresh"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       Levels       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-level-to-level-height (make-parameter 3))
(define current-level (make-parameter #f))
(define (set-current-level! level)
  (set! current-level level))

(define (check-level)
  (write-msg-name "CheckStory")
  (read-sized (cut deserialize (storyinfo*) <>)input))

#|
Function to create a level given a height, this uses absolute height
Will create a level with 10 height, if there isn't one
 height: height of the level
Example of usage: 
(send (create-level 10.0))
|#
(define (create-level #:height [height 0])
  (let ((msg (storymsg* #:height height
                        #:name "Story")))
    (write-msg "Story" msg)
    (read-sized (cut deserialize (storyinfo*) <>)input)))

(define (upper-level #:level [level (current-level)]
                     #:height [height (default-level-to-level-height)])
  (let ((msg (upperlevelmsg* #:height height
                             #:index (storyinfo-index level))))
    (write-msg "UpperLevel" msg)
    (read-sized (cut deserialize (storyinfo*) <>)input)))

(define (current-level-elevation)
  (storyinfo-level (current-level)))
(define (current-level-index)
  (storyinfo-index (current-level)))

#|
Function to delete all stories
Example of usage: 
(send (delete-stories))
|#
(define (delete-stories)
  (write-msg-name "DeleteStories"))
(define (delete-levels) (delete-stories))
