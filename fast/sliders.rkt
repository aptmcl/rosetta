#lang racket


(require racket/gui/dynamic)


(define frame% (if (gui-available?) (gui-dynamic-require 'frame%) #f))
(define tab-panel% (if (gui-available?) (gui-dynamic-require 'tab-panel%) #f))
(define slider% (if (gui-available?) (gui-dynamic-require 'slider%) #f))
(define horizontal-pane% (if (gui-available?) (gui-dynamic-require 'horizontal-pane%) #f))
(define vertical-pane% (if (gui-available?) (gui-dynamic-require 'vertical-pane%) #f))
(define check-box% (if (gui-available?) (gui-dynamic-require 'check-box%) #f))
(define button% (if (gui-available?) (gui-dynamic-require 'button%) #f))
(define the-font-list (if (gui-available?) (gui-dynamic-require 'the-font-list) #f))

(when (gui-available?) (dynamic-require 'racket/gui/base 0))


;(require (only-in "backends.rkt" delete-all-shapes realize disable-update enable-update))

(provide sliders interactive)


(define (argument<-slider param slider)
  (let ((s (send slider get-value)))
    (if (or (empty? (cdr param)) (empty? (cddr param)) (empty? (cdddr param)))
        s
        (* s (cadddr param)))))

(define (parameter? obj)
  (and (cons? obj)
       (string? (car obj))))

(define (label<-parameter param)
  (car param))

(define (min-value<-parameter param)
  (if (empty? (cdr param))
      0
      (cadr param)))

(define (max-value<-parameter param)
  (if (or (empty? (cdr param)) (empty? (cddr param)))
      100
      (caddr param)))

(define (cur-value<-parameter param)
  (if (or (empty? (cdr param)) (empty? (cddr param)) (empty? (cdddr param)))
      (if (empty? (cdr param))
          0
          (cadr param))
      (cadddr param)))

#;
(define (make-draw-fn fn params sliders-fn)
  (lambda ()
    (begin ;time
      (begin
        (delete-all-shapes)
        (let ((shape
               (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (exn) #f)))
                 (apply fn (map argument<-slider params (sliders-fn))))))
          (realize shape)
          (refresh))))))


(define (make-draw-fn fn sliders-fn)
  (lambda ()
    (begin ;time
      (begin
        ;(disable-update)
        ;(delete-all-shapes)
        (let ((shape
               (with-handlers ((exn:fail:contract:divide-by-zero? (lambda (exn) #f)))
                 (apply fn (map (lambda (slider) (send slider get-value)) (sliders-fn))))))
          1
          #;#;(realize shape)
          (refresh))
        ;(enable-update)
        ))))

(define (make-widgets-frame label)
  (new frame% 
       [label label] 
       [alignment '(left center)]))

(define (make-widget parent label min-value max-value cur-value callback)
  (cond ((number? min-value)
         (new slider%
              [parent parent]
              #;[style '(horizontal vertical-label)]
              #;[font (send the-font-list
                          find-or-create-font 
                          12 'script 'italic 'bold #f 'smoothed)]
              [label label]
              [min-value min-value]
              [max-value max-value]
              [init-value cur-value]
              [callback callback]))
        ((boolean? min-value)
         (new check-box%
              [parent parent]
              [label label]
              [callback callback]
              [enabled cur-value]))
        (else
         (error "Unknown type of widget for value" min-value))))

(define (make-widget-callback fn)
  (let ((prev #f))
    (λ (slider event)
      (let ((new (send slider get-value)))
        (unless (eqv? new prev)
          (set! prev new)
          (fn))))))

(define (make-widgets fn-name parent fn param ignore)
  (letrec ((draw-fn (make-draw-fn fn (λ () sliders)))
           (sliders
            (let loop ((param param) (parent parent) (curr vertical-pane%) (next horizontal-pane%))
              (if (parameter? param)
                  (list (make-widget parent
                                     (label<-parameter param)
                                     (min-value<-parameter param)
                                     (max-value<-parameter param)
                                     (cur-value<-parameter param)
                                     (make-widget-callback draw-fn)))
                  (let ((parent (new curr [parent parent])))
                    (append* (map (λ (param) (loop param parent next curr))
                                  param)))))))
    (let ((button (new button%
                       [parent parent]
                       [label "Show"]
                       [callback (lambda (b e)
                                   (displayln (cons fn-name 
                                                    (map (lambda (slider)
                                                           (send slider get-value))
                                                         sliders))))])))
      (draw-fn)
      (send parent show #t))))

(define (sliders label fn params)
  (make-widgets 'params (make-widgets-frame label) fn params #f))

(define-syntax-rule (interactive label fn-name param ...)
  (make-widgets 'fn-name (make-widgets-frame label) fn-name '(param ...) #f))
    
(define (tabbed-sliders label fn params)
  (make-widgets 'params (make-widgets-frame label) fn params #t))
#;#;#;
(sliders
 "Foo1"
 (lambda args (displayln args))
 '("Bar" 1 10))

(sliders
 "Foo2"
 (lambda args (displayln args))
 '(("Bar" 1 10)
   ("Baz" 1 100)))

(sliders
 "Foo3"
 (lambda args (displayln args))
 '((("Bar" 1 10)
    ("Baz" 1 100))
   (("Quux" 1 10)
    ("Yep" 1 100))))

#;(tabbed-sliders
 "Foo4"
 (lambda args (displayln args))
 '(("Tab1"
    ("Bar1" 1 10))
   ("Tab2"
    ("Bar2" 1 10)
    ("Baz2" 1 100))))


#;
(tabbed-sliders
 "Foo4"
 (lambda args (displayln args))
 '(("Tab1"
    (("Bar1" 1 10)
     ("Baz1" 1 100))
    (("Quux1" 1 10)
     ("Yep1" 1 100)))
   ("Tab2"
    (("Bar2" 1 10)
     ("Baz2" 1 100))
    (("Quux2" 1 10)
     ("Yep2" 1 100)))))
