#lang typed/racket/base
(require (for-syntax racket/base))
(require racket/port)

#;
(define (write-string [str : String] [out : Output-Port])
  (%write-string str (current-error-port))
  (%write-string str out))
  
(require racket/tcp)

(provide Connection
         tracing-connection
         accept-connection
         shutdown-connection
         establish-connection
         connection-out
         connection-in
         write-line-to
         read-line-from
         read-line-string
         defcall
         defencoder
         defdecoder)

(define-type Connection connection)
(: accept-connection (Integer -> Connection))
(: shutdown-connection (Connection -> Void))
(: establish-connection (String Integer -> Connection))
(: write-line-to (String Connection -> Void))
(: read-line-from (Connection -> String))
(: read-line-string (Input-Port -> String))

(struct: connection
  ([in : Input-Port]
   [out : Output-Port]))

(: tracing-connection (-> Connection))
(define (tracing-connection)
  (connection (current-input-port) (current-output-port)))

(define (broadcast-port . [ports : Output-Port *])
  (make-output-port
   'broadcast-port
   always-evt
   (lambda ([s : Bytes]
            [start : Natural]
            [end : Natural]
            [non-block? : Boolean]
            [breakable? : Boolean]) : Integer
     (if non-block?
         (for ((port (in-list ports)))
           (write-bytes-avail s port start end))
         (if breakable?
             (for ((port (in-list ports)))
               (write-bytes-avail/enable-break s port start end))
             (for ((port (in-list ports)))
               (write-bytes s port start end))))
     (- end start))
   void))

(provide debuging-connection?)
(define debuging-connection? : (Parameterof Boolean) (make-parameter #f))
;;Client protocol

(define (accept-connection port-no)
  (define listener (tcp-listen port-no 1 #t))
  (define-values (i o) (tcp-accept listener)) ;;we only accept a client, for now
  (connection i
              (if (debuging-connection?)
                  (broadcast-port o (current-error-port))
                  o)))
  
(define (shutdown-connection conn)
  (close-input-port (connection-in conn))
  (close-output-port (connection-out conn)))

;;Line protocol

(define (write-line-to str conn)
  (write-string str (connection-out conn))
  (newline (connection-out conn))
  (flush-output (connection-out conn)))

(define (read-line-from conn)
  (read-line-string (connection-in conn)))

(define (read-line-string in)
  (let ((str-or-EOF (read-line in)))
    (if (string? str-or-EOF)
        (if (string=? str-or-EOF "ERROR")
            (let ((msg (read-line in)))
              (if (string? msg)
                  (error (string-append "Server error: " msg))
                  (error "Expecting a Server error message but got " msg)))
            str-or-EOF)
        (error "Server error: Expecting a String but got " str-or-EOF))))

;;Connection protocol

(define (establish-connection client port)
  (let ((conn (accept-connection port)))
    (let ((ack (read-line-from conn)))
      (if (and (string? ack)
               (string=? ack "connected"))
          conn
          (error (format "~A did not connect! Reply was:" client) ack)))))

;;Funtion call protocol

#|

Each operation is defined as:

(defcall (operation [p0 t0] [p1 t1] ...) t)

and it implies the type 

(: operation (Connection t0 t1 ... -> t))

By default, each parameter is of type Real. Thus

(defcall (addSphere [x Real] [y Real] [z Real] [r Real]) String)

can be simplified to 

(defcall (addSphere x y z r) String)

By default, the return type is String. Thus

(defcall (addSphere x y z r) String)

can be simplified to

(defcall (addSphere x y z r))

|#

(begin-for-syntax
  (define encoders (list))
  (define decoders (list))
  
  (define (add-encoder! type f)
    (set! encoders (cons (cons type f) encoders))
    encoders)
  
  (define (add-decoder! type f)
    (set! decoders (cons (cons type f) decoders))
    decoders)

  (define (found-type? type t-f)
    (and (free-identifier=? type (car t-f))
         (cdr t-f)))

  (define (encoder-for type)
    (or
     (for/or ([t-f (in-list encoders)])
       (found-type? type t-f))
     (error "Missing encoder for" type)))
  
  (define (decoder-for type)
    (or
     (for/or ([t-f (in-list decoders)])
       (found-type? type t-f))
     (error "Missing decoder for" type))))

(define-syntax (encode-arg stx)
  (syntax-case stx ()
    ((_ type)
     (encoder-for #'type))))

(define-syntax (decode-result stx)
  (syntax-case stx ()
    ((_ type)
     (decoder-for #'type))))

(define-syntax (defcall stx)
  (syntax-case stx ()
    ((defcall ((name strname) conn p0 ps ...) t)
     (with-syntax (((t0 ts ...)
                    (map (lambda (p)
                           (if (list? (syntax-e p)) (cadr (syntax-e p)) 'Real))
                         (syntax->list #'(p0 ps ...))))
                   ((p0 ps ...)
                    (map (lambda (p)
                           (if (list? (syntax-e p)) (car (syntax-e p)) p))
                         (syntax->list #'(p0 ps ...)))))
       (quasisyntax/loc stx
         (begin
           (: name (t0 ts ... -> t))
           (provide name)
           (define (name p0 ps ...)
             (let ((out (connection-out conn)))
               (write-string strname out)
               (write-string "(" out)
               ((encode-arg t0) p0 out)
               (begin
                 (write-string "," out)
                 ((encode-arg ts) ps out))
               ...
               (write-string ")\n" out)
               (flush-output out)
               (let ((in (connection-in conn)))
                 ((decode-result t) in))))))))
    [(defcall ((name strname) conn) t)
     (quasisyntax/loc stx
       (begin
         (: name (-> t))
         (provide name)
         (define (name)
           (let ((out (connection-out conn)))
             (write-string strname out)
             (write-string "()\n" out)
             (flush-output out)
             (let ((in (connection-in conn)))
               ((decode-result t) in))))))]
    ((defcall (name ps ...))
     (quasisyntax/loc stx
       (defcall (name ps ...) String)))
    ((defcall (name ps ...) t)
     (quasisyntax/loc stx
       (defcall ((name #,(symbol->string (syntax-e #'name))) ps ...) t)))))

(define-syntax (defencoder stx)
  (syntax-case stx ()
    ((_ type f)
     (add-encoder! #'type #'f)
     #'(void))))

(define-syntax (defdecoder stx)
  (syntax-case stx ()
    ((_ type f)
     (add-decoder! #'type #'f)
     #'(void))))

