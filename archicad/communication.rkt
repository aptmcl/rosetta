#lang racket
(provide (except-out (all-defined-out)
                     ))

(require "protobuf1/protobuf.rkt")
(require "protobuf1/encoding.rkt")
(require "messages.rkt")
(require "../base/coord.rkt")
(require "../base/connection.rkt")
(require "install.rkt")
(require srfi/26)

(define visual-feedback? (make-parameter #f))

(define conn #f)

(define (bim-connection)
  (unless conn
    (set! conn (start-connection))
    (set-current-level! (make-parameter (check-level)))
    (create-layer "Non Trim Layer" 0)
    (create-layer "Trim Layer"))
  conn)

;;Function to quit
(define (disconnect)
  (when conn
    (write-msg-name "quit")
    (set! conn #f))
  (void))


(define server-addr "localhost")

(define moved-addon-files? #f)

(define (move-addon-files)
  (unless moved-addon-files?
    (display "Checking plugin...")
    (display "Installing plugin...")
    (move-addon)
    (displayln "done!")
    (set! moved-addon-files? #t)))

(define (start-connection)
  (move-addon-files)
  (define time-out-tries 10)
  (let rec ((n time-out-tries))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (if (> n 0)
                           (begin
                             (printf "[~A/~A] You must first use the connect button on ArchiCAD.\n"
                                     (+ 1 (- time-out-tries n))
                                     time-out-tries)
                             (printf "Try in the top bar, Addon->Rosetta")
                             (sleep 2)
                             (rec (- n 1)))
                           (raise e)))])
  (let-values([(in out) (tcp-connect server-addr 53800)])
    (file-stream-buffer-mode in 'none)
    (file-stream-buffer-mode out 'none)
    (connection in out)))))

;;Usage: (send (create-...) (create-...) ... )
(define-syntax-rule
  (send expr ...)
  (begin0
    (begin expr ...)
    (disconnect)))
#;(define-syntax-rule (send expr ...)
  (begin
    (connect)
    (parameterize ((current-level (check-level)))
      expr ...)
    (disconnect)))

;;Function to send name 
(define (write-msg-name name)
  (write-sized serialize
               (namemessage* #:name name)
               (connection-out (bim-connection))))

;;Function to call 
;;a function with a 'name' and 'strct'
(define (write-msg name strct)
  (write-msg-name name)
  (write-sized serialize
               strct
               (connection-out (bim-connection))))

;;Function to enable 3D visual feedback
(define (visual-feedback-on)
  (write-msg-name "RefreshOn"))

;;Function to disable 3D visual feedback
(define (visual-feedback-off)
  (write-msg-name "RefreshOff"))

;;Function to send a double
(define (send-double d)
  (write-sized serialize
               (doublemessage* #:d d)
               (connection-out (bim-connection))))

;;Function to send list of points
(define (send-list-points lst)
  (for-each (lambda (point)
              (write-sized serialize
                           (pointmessage* #:p0x (car point) 
                                          #:p0y (cdr point))
                           (connection-out (bim-connection)))) 
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
    (write-sized serialize
                 (pointsmessage* #:px lst-x 
                                 #:py lst-y 
                                 #:pz lst-z)
                 (connection-out (bim-connection)))))

;;Function to send list of points using repeated fields with XYZ Rosetta implementation
#|
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
                                           #:pz lst-z) (connection-out (bim-connection)))))
|#

;;TODO rework conditions, too confusing...
(define (close-guide points first-el [no-lists? #t])
  (cond
    [(and (null? points) no-lists?)(list first-el)]
    [(null? points) (list)]
    [(and (list? (car points)) no-lists?) (cons first-el (cons (append (car points) (list (caar points))) (close-guide (cdr points) first-el #f)))]
    [(list? (car points))(cons (append (car points) (list (caar points))) (close-guide (cdr points) first-el #f))]
    [else (cons (car points) (close-guide (cdr points) first-el no-lists?))]))


(define (prepare-points-to-send lst)
  (let ((lst-x (list))
        (lst-y (list))
        (lst-z (list)))
    (for-each (lambda (point)
                (set! lst-x (append lst-x (list (cx point))))
                (set! lst-y (append lst-y (list (cy point))))
                (set! lst-z (append lst-z (list (cz point)))))
              lst)
    (pointsmessage* #:px lst-x 
                    #:py lst-y 
                    #:pz lst-z)))

(define (send-points lst)
  (write-sized serialize
               (prepare-points-to-send lst)
               (connection-out (bim-connection))))

;;Function to send list of arcs
(define (send-list-arcs lst)
  (for-each (lambda (arc)
              (write-sized serialize
                           (polyarcmessage* #:begindex (car arc) 
                                            #:endindex (car (cdr arc)) 
                                            #:arcangle (car (cdr (cdr arc))))
                           (connection-out (bim-connection)))) 
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
    (write-sized serialize
                 (polyarcsmessage* #:begindex lst-beg-index 
                                   #:endindex lst-end-index 
                                   #:arcangle lst-arc-angle)
                 (connection-out (bim-connection)))))

#|
 Function to send list of arcs with assumed order
 The first arc will be applied to the line that is formed 
 by the first and second point. The logic is applied to
 the following arcs.
 Ex.: List with two arcs, will apply the first arc to
      line(p1, p2), and the second arc to line(p2, p3)
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
                                             #:arcangle lst-arc-angle) (connection-out (bim-connection)))))
|#

(define (prepare-arcs-to-send lst)
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
    (polyarcsmessage* #:begindex lst-beg-index 
                      #:endindex lst-end-index 
                      #:arcangle lst-arc-angle)))
(define (send-arcs lst)
  (write-sized serialize
               (prepare-arcs-to-send lst)
               (connection-out (bim-connection))))

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
(define current-level (make-parameter (storyinfo* #:exists #t
                                                  #:index 0
                                                  #:level 0
                                                  #:name "")))
(define (set-current-level! level)
  (set! current-level level))

(define (check-level)
  (write-msg-name "CheckStory")
  (read-sized (cut deserialize (storyinfo*) <>)
              (connection-in (bim-connection))))

(define (read-guid)
  (elementid-guid (read-guid-aux)))

(define (read-guid-aux)
  (read-sized (cut deserialize (elementid*) <>)
              (connection-in (bim-connection))))

(define (read-guids*)
  (read-sized (cut deserialize (elementidlist*) <>)
              (connection-in (bim-connection))))

(define-syntax-rule
  (send/no-rcv name body ...)
  (let ((output (connection-out (bim-connection))))
    (write-sized serialize (namemessage* #:name name) output)
    (write-sized serialize body output) ...))

(define-syntax-rule
  (send/rcv-id name body ...)
  (let ((input (connection-in (bim-connection))))
    (send/no-rcv name body ...)
    (elementid-guid (read-sized (cut deserialize (elementid*) <>) input))))


#|
Function to create a level given a height, this uses absolute height
Will create a level with 10 height, if there isn't one
 height: height of the level
Example of usage: 
(send (level 10.0))
|#
(define (level height)
  (let ((msg (storymsg* #:height height
                        #:name "Story")))
    (write-msg "Story" msg)
    (read-sized (cut deserialize (storyinfo*) <>)
                (connection-in (bim-connection)))))

(define (upper-level [level (current-level)]
                     [height (default-level-to-level-height)])
  (let ((msg (upperlevelmsg* #:height height
                             #:index (storyinfo-index level))))
    (write-msg "UpperLevel" msg)
    (read-sized (cut deserialize (storyinfo*) <>)
                (connection-in (bim-connection)))))

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


;;Extras
(define (view-3d)
  (write-msg-name "3D"))

(define (view-2d)
  (write-msg-name "2D"))

;(send (open-file "D:\\GitHubRep\\Tese\\Development\\Examples\\Models\\AT for eCADDe.pln"))
;(send (open-file "D:\\GitHubRep\\Tese\\Development\\Examples\\Models\\AT for eCADDe.ifc"))
;(send (open-file "C:\\Users\\Client\\Desktop\\SmallTower.ifc"))
(define (open-file path)
  (let ((msg (openmessage* #:path path
                           #:extension (last (string-split path ".")))))
    (write-msg "OpenFile" msg)))

;When connection is 0, elements on that layer will not intersect
(define (create-layer name [connection 1])
  (let ((msg (layermsg* #:name name
                        #:connection connection)))
    (write-msg "Layer" msg)))

(define (shape-layer guid name)
  (let ((msg (layerelementmsg* #:layer name
                               #:guid guid)))
    (write-msg "LayerElem" msg)))

(define (hide-layer name)
  (let ((msg (layermsg* #:name name)))
    (write-msg "HideLayer" msg)))

(define (show-layer name)
  (let ((msg (layermsg* #:name name)))
    (write-msg "ShowLayer" msg)))