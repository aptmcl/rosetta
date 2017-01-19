#lang typed/racket/base/no-check
(require (for-syntax racket/base))
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require racket/port)

#;
(define (write-string [str : String] [out : Output-Port])
  (%write-string str (current-error-port))
  (%write-string str out))
  
(require racket/tcp)

(provide Connection
         tracing-connection
         debuging-connection
         debuging-connection?
         (struct-out connection)
         start-connection
         current-connection
         shutdown-connection
         shutdown-current-connection!
         connection-out
         connection-in
         write-line-to
         read-line-from
         read-line-string
         read-int32
         write-int32
         read-double
         write-double
         read-c-sharp-string
         write-c-sharp-string
         defcall
         defencoder
         defdecoder
         encode
         decode
         use-port-number!
         defop)

(define-type Connection connection)
(: start-connection (Integer -> Connection))
(: shutdown-connection (Connection -> Void))
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

(define debuging-connection? : (Parameterof Boolean) (make-parameter #f))

(define (debuging-connection [in : Input-Port] [out : Output-Port]) : Connection
  (connection in
              (if (debuging-connection?)
                  (broadcast-port out (current-error-port))
                  out)))

;;Client protocol
(define port-no 11000)
(define (use-port-number! i)
  (set! port-no i))

(define (start-connection [port-no port-no])
  (with-handlers ([exn:fail:network?
                   (lambda (exn)
                     (error "Unable to connect. Did you start Khepri in the backend?"))])
    (let-values ([(i o) (tcp-connect "127.0.0.1" port-no)])
      (debuging-connection i o))))
  
(define (shutdown-connection conn)
  (close-input-port (connection-in conn))
  (close-output-port (connection-out conn)))

(define conn #f)

(define (current-connection)
  (unless conn
    (set! conn (start-connection)))
  conn)

(define (shutdown-current-connection!)
  (when conn
    (shutdown-connection conn)
    (set! conn #f)))


(define (request-operation name)
  (let ((conn (current-connection)))
    (let ((out (connection-out conn)))
      (write-byte 0 out)
      (write-c-sharp-string name out)
      (flush-output out)
      (let ((in (connection-in conn)))
        (read-int32 in)))))

;;Line protocol
(define big-endian? (system-big-endian?))

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

(define (read-int32 i)
  (integer-bytes->integer (read-bytes 4 i) #t big-endian?))

(define (write-int32 [i : Integer] [o : Output-Port])
    (write-bytes (integer->integer-bytes i 4 #t big-endian?) o))

(define (read-double [i : Input-Port])
  (floating-point-bytes->real (read-bytes 8 i) big-endian?))

(define (write-double [x : Real] [o : Output-Port])
  (write-bytes (real->floating-point-bytes x 8 big-endian?) o))

(define (read-c-sharp-string [i : Input-Port])
  (let loop ((size 0) (shift 0))
    (let ((byte (read-byte i)))
      (let ((size (bitwise-ior size (arithmetic-shift (bitwise-and byte #x7f) shift))))
        (if (= (bitwise-and byte #x80) 0)
            (bytes->string/utf-8 (read-bytes size i))
            (loop size (+ shift 7)))))))

(define (write-c-sharp-string [s : String] [o : Output-Port])
  (let ((bytes (string->bytes/utf-8 s)))
    (let loop ((size (bytes-length bytes)))
      (let ((byte (bitwise-and size #x7f)))
        (let ((size (arithmetic-shift size -7)))
          (if (> size 0)
              (begin
                (write-byte (bitwise-ior byte #x80) o)
                (loop size))
              (write-byte byte o)))))
    (write-bytes bytes o)))

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

(define-syntax (encode stx)
  (syntax-case stx ()
    ((_ type)
     (encoder-for #'type))))

(define-syntax (decode stx)
  (syntax-case stx ()
    ((_ type)
     (decoder-for #'type))))

(define-syntax (defcall stx)
  (syntax-case stx ()
    [(defcall ((name code conn) ps ...) t)
     (with-syntax (((ts ...)
                    (map (lambda (p)
                           (if (list? (syntax-e p)) (cadr (syntax-e p)) 'Real))
                         (syntax->list #'(ps ...))))
                   ((ps ...)
                    (map (lambda (p)
                           (if (list? (syntax-e p)) (car (syntax-e p)) p))
                         (syntax->list #'(ps ...)))))
       (quasisyntax/loc stx
         (begin
           (: name (Connection ts ... -> t))
           (provide name)
           (define (name ps ...)
             (let ((out (connection-out conn)))
               (write-byte code out)
               ((encode ts) ps out)
               ...
               (flush-output out)
               (let ((in (connection-in conn)))
                 ((decode t) in)))))))]))

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


(define-for-syntax (type-id stx type-str array?)
  (datum->syntax
   stx
   (string->symbol
    (if array?
        (string-append type-str "Array")
        type-str))))

(define-syntax (defop stx)
  (syntax-case stx ()
    [(def str)
     (match (regexp-match #px"^ *(public|) *(\\w+) *(\\[\\])? +(\\w+) *\\( *(.*) *\\)" (syntax->datum #'str))
       ((list _ visibility return-type-str array? name-str params-str)
        (let ((name (datum->syntax stx (string->symbol name-str)))
              (return-type (type-id stx return-type-str array?)))
          (with-syntax ((name-op (format-id #'name (format "~a-op" name-str)))
                        (name name)
                        (return-type return-type)
                        (name-str name-str))
            (if (eq? (string-length params-str) 0)
                (syntax/loc stx
                  (begin
                    (define name-op (request-operation name-str))
                    (defcall ((name name-op (current-connection))) return-type)))
                (let ((params
                       (for/list ((param-str (in-list (regexp-split #rx" *, *" params-str))))
                         (match (regexp-match #px"^ *(\\w+) *(\\[\\])? *(\\w+)$" param-str)
                           [(list _ type array? name)
                            (list (datum->syntax stx (string->symbol name))
                                  (type-id stx type array?))]))))
                    (with-syntax ((([pname ptype] ...) params))
                      (syntax/loc stx
                        (begin
                          (define name-op (request-operation name-str))
                          (defcall ((name name-op (current-connection)) [pname ptype] ...) return-type))))))))))]))