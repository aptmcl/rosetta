#lang typed/racket/base
(require "../base/typed-com.rkt")

(provide workbooks
         open-workbook
         new-workbook
         close-workbook
         workbook
         sheets
         sheet
         range
         get-value
         set-value!
         get-values
         set-values!
         get-range-value
         set-range-value!
         get-range-values
         set-range-values!)

(define-type Cell-Value (U Void String Real Boolean))
(define-type Cells-Values (Vectorof (Vectorof Cell-Value)))

(define (excel) : Com-Object
  (let ((clsid (progid->clsid "Excel.Application")))
    (with-handlers ([exn? (lambda (ex)
                            (com-create-instance clsid))])
      (com-get-active-object clsid))))


(def-ro-property ((_workbooks WorkBooks) Com-Object) Com-Object)
(def-ro-property ((_sheets Sheets) Com-Object) Com-Object)
(def-rw-property ((_value Value2) Com-Object) Cell-Value)
(def-rw-property ((_values Value2) Com-Object) Cells-Values)

(def-com-method open #f ([path String]) Com-Object)
(def-com-method add #f () Com-Object)
(def-com-method save #f ([save? Boolean]) Void)

(define (workbooks [e : Com-Object (excel)])
  (_workbooks e))

;; Open an existing workbook.
(define (open-workbook [path : Path-String] [workbooks : Com-Object (workbooks)]) : Com-Object
  (open workbooks (if (path? path) (path->string path) path)))

;; Create a workbook
(define (new-workbook [workbooks : Com-Object (workbooks)]) : Com-Object
  (add workbooks))

;; Close a workbook
(define (close-workbook [doc : Com-Object] [save? : Boolean]) : Void
  (save doc save?))

;; Obtain an already opened workbook
(define (workbook [name : (U String Integer) 1] [workbooks : Com-Object (workbooks)]) : Com-Object
  (cast (com-get-property* workbooks "Item" name) Com-Object))

;; Sheets of the workbook
(define (sheets [workbook : Com-Object (workbook)])
  (_sheets workbook))

;; Obtain a specific sheet
(define (sheet [name : (U String Integer) 1] [sheets : Com-Object (sheets)]) : Com-Object
  (cast (com-get-property* sheets "Item" name) Com-Object))

;; Obtain a specific range
(define (range [rng : String] [sheet : Com-Object (sheet)]) : Com-Object
  (cast (com-get-property* sheet "Range" rng) Com-Object))

;; Obtain the value of a range
(define (get-value [range : Com-Object]) : Cell-Value
  (_value range))

;; Set the value of a range
(define (set-value! [range : Com-Object] [value : Cell-Value]) : Void
  (_value range value)
  (void))

(define (get-values [range : Com-Object]) : Cells-Values
  (_values range))

;; Set the value of a range
(define (set-values! [range : Com-Object] [value : Cells-Values]) : Void
  (_values range value)
  (void))

;; Composition

(define (get-range-value [rng : String] [sheet : Com-Object (sheet)]) : Cell-Value
  (get-value (range rng sheet)))

(define (set-range-value! [rng : String] [value : Cell-Value] [sheet : Com-Object (sheet)]) : Void
  (set-value! (range rng sheet) value))

(define (get-range-values [rng : String] [sheet : Com-Object (sheet)]) : Cells-Values
  (get-values (range rng sheet)))

(define (set-range-values! [rng : String] [value : Cells-Values] [sheet : Com-Object (sheet)]) : Void
  (set-values! (range rng sheet) value))