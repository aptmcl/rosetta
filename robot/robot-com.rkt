#lang typed/racket/base/no-check
(require (for-syntax racket/base racket/list))
(require racket/function racket/string racket/match racket/port racket/file)
(require (except-in math random-integer)
         "../base/typed-com.rkt"
         "../base/utils.rkt"
         "../base/coord.rkt"
         "robot-enums.rkt")

(provide (all-defined-out) com-omit)

(define-type Long Integer)
(define-type Integers (Listof Integer))
(define-type Shorts (Listof Integer))
(define-type Com-Objects (Listof Com-Object))
(define-type Ref Com-Object)
(define-type Refs (Listof Ref))
(provide Ref Refs)
(define-type Double Float)
(define-type (Variant T) T)
;;To simplify type checking
(define-type Double3 (Vector Double Double Double))
(define-type Double4x4 (Vector (Vector Double Double Double Double)
                               (Vector Double Double Double Double)
                               (Vector Double Double Double Double)
                               (Vector Double Double Double Double)))
(define-type VarDouble3 (Variant Double3))
(define-type VarDouble4x4 (Variant Double4x4))
(define-type VarDouble3N (Variant (Vectorof Double)))
(define-type VarDouble2N (Variant (Vectorof Double)))
(define-type VarIntegerN Type-Described #;(Variant (Vectorof Integer)))

(define-syntax-rule (mf m i j) (real->double-flonum (matrix-ref m i j)))

(define (loc->transformation-matrix [p : Loc]) : VarDouble4x4
  (let ((m (world-transformation p)))
    (vector (vector (mf m 0 0) (mf m 0 1) (mf m 0 2) (mf m 0 3))
            (vector (mf m 1 0) (mf m 1 1) (mf m 1 2) (mf m 1 3))
            (vector (mf m 2 0) (mf m 2 1) (mf m 2 2) (mf m 2 3))
            (vector (mf m 3 0) (mf m 3 1) (mf m 3 2) (mf m 3 3)))))

;;Several types must be treated just as Com-Objects

(define-type 3DFace Com-Object)

(begin-for-syntax
  (require syntax/id-table)
  (define com-obj (make-immutable-free-id-table 
                   (list (cons #'IRobotApplication #'(application))
                         )))
  (define com-type (make-immutable-free-id-table
                    (map (lambda (id) (cons id id #;#'Com-Object))
                         (list 
                               #'IRobotApplication
                               #'IRobotProject
                               #'IRobotProjectType
                               #'IRobotStructure
                               #'IRobotLabel
                               #'IRobotNodeServer
                               #'IRobotBarServer
                               #'IRobotLabelServer
                               #'IRobotNode
                               #'IRobotBar
                               #'All))))
  (define (translate table stx default)
    (if (identifier? stx)
        (free-id-table-ref table stx default)
        stx)))

; initialization

(define (initialize) : Void
  (let* ((app
          (let ((clsid (progid->clsid "Robot.Application")))
            (with-handlers ((exn?
                             (λ (e)
                               (display "Starting Robot...")
                               (flush-output)
                               (begin0
                                 (com-create-instance clsid)
                                 (displayln "done!")))))
              (cast (com-get-active-object clsid) IRobotApplication)))))
    (com-set-property! app "Visible" 1)
    (set! application (lambda () app))
    app))

(define (application) : Com-Object
  (initialize)
  (application))

(define vbCr "\r\n")
(define vbEsc "\e")

;;We upgrade types automatically, from the types that Rhino's COM requires and the types that Rosetta provides

(with-upgrade-types ([Double Real]
                     [VarDouble3 Loc]
                     [VarDouble3N Locs]
                     [VarDouble2N Locs]
                     [VarDouble4x4 Loc]
                     [VarIdN Refs]
                     [VarIntegerN Shorts]
                     [AcCmColor Color]))

;;In most cases, upgrading from one type to another requires a conversion function for the type values

(with-conversions
    ([Real        Double       real->double-flonum]
     [Double      Real         identity]
     [Loc         VarDouble3   loc->vector-double-flonum]
     [Loc         VarDouble4x4 loc->transformation-matrix]
     [VarDouble3  Loc          vector-double-flonum->loc]
     [VarDouble3N Locs         vector-3-double-flonums->locs]
     [VarDouble2N Locs         vector-2-double-flonums->locs]
     [Locs        VarDouble3N  locs->vector-3-double-flonums]
     [Locs        VarDouble2N  locs->vector-2-double-flonums]
     [Refs        VarIdN       list->vector]
     [VarIdN      Refs         vector->list]
     [IdsOrVoid   Refs         ids-or-void->refs]))


;;PROPERTIES
(def-ro-property (project IRobotApplication) IRobotProject)
(def-ro-property (nodes IRobotStruture) IRobotNodeServer)
(def-ro-property (bars IRobotStruture) IRobotBarServer)
(def-ro-property (structure IRobotProject) IRobotStructure)
(def-ro-property (calc-engine IRobotProject) IRobotCalcEngine)
(def-ro-property (results IRobotStructure) IRobotResultsServer)
(def-ro-property (labels IRobotStructure) IRobotLabelServer)
(def-ro-property (cases IRobotStructure) IRobotCaseServer)
(def-ro-property (records IRobotSimpleCase) IRobotLoadRecordMngr)
(def-ro-property (objects IRobotLoadRecord) IRobotSelection)
(def-ro-property (data IRobotLabel) IRobotNodeSuportData)
;(def-ro-property (data IRobotLabel) IRobotBarSectionData)
(def-rw-property (UX IRobotNodeSuportData) Integer)
(def-rw-property (UY IRobotNodeSuportData) Integer)
(def-rw-property (UZ IRobotNodeSuportData) Integer)
(def-rw-property (RX IRobotNodeSuportData) Integer)
(def-rw-property (RY IRobotNodeSuportData) Integer)
(def-rw-property (RZ IRobotNodeSuportData) Integer)
(def-rw-property (Gamma IRobotBar) Double) ;Angle of rotation

(def-rw-property (shape-type IRobotBarSectionData) Integer)
(def-rw-property (concrete IRobotBarSectionData) RobotBarSectionConcreteData)
(def-rw-property (MaterialName IRobotBarSectionData) String) 
(def-rw-property (start-node IRobotBarReleaseData) StartNode)
(def-rw-property (end-node IRobotBarReleaseData) EndNode)

(def-rw-property (CB71_Category IRobotMaterialData) Integer)
(def-rw-property (CB71_Humidity IRobotMaterialData) Double)
(def-rw-property (CB71_Nature IRobotMaterialData) Integer)
(def-rw-property (CB71_Retreat IRobotMaterialData) Double)
(def-rw-property (CS IRobotMaterialData) Double)
(def-rw-property (Default IRobotMaterialData) Boolean)
(def-rw-property (DumpCoef IRobotMaterialData) Double)
(def-rw-property (E IRobotMaterialData) Double)
(def-rw-property (E_5 IRobotMaterialData) Double)
(def-rw-property (E_Trans IRobotMaterialData) Double)
(def-rw-property (EC_Deformation IRobotMaterialData) Double)
(def-rw-property (GMean IRobotMaterialData) Double)
(def-rw-property (Kirchoff IRobotMaterialData) Double)
(def-rw-property (LX IRobotMaterialData) Double)
(def-rw-property (Name IRobotMaterialData) String)
(def-rw-property (NU IRobotMaterialData) Double)
(def-rw-property (Nuance IRobotMaterialData) String)
(def-rw-property (PN_Deformation IRobotMaterialData) Double)
(def-rw-property (PN_E_Additional IRobotMaterialData) Double)
(def-rw-property (PN_E_Trans IRobotMaterialData) Double)
(def-rw-property (RE IRobotMaterialData) Double)
(def-rw-property (RE_AxCompr IRobotMaterialData) Double)
(def-rw-property (RE_AxTens IRobotMaterialData) Double)
(def-rw-property (RE_Bending IRobotMaterialData) Double)
(def-rw-property (RE_Shear IRobotMaterialData) Double)
(def-rw-property (RE_TrCompr IRobotMaterialData) Double)
(def-rw-property (RE_TrTens IRobotMaterialData) Double)
(def-rw-property (RO IRobotMaterialData) Double)
(def-rw-property (RT IRobotMaterialData) Double)
(def-rw-property (SecondName IRobotMaterialData) Double)
(def-rw-property (Steel_Thermal IRobotMaterialData) Boolean)
(def-rw-property (Timber_Type IRobotMaterialData) IRobotMaterialTimberType)
(def-rw-property (Type IRobotMaterialData) IRobotMaterialType)


;;COM METHODS
(define-syntax (def-com stx)
  (syntax-case stx ()
    [(def (name com) (param-spec ...) rtype)
     (let ((known-com (translate com-obj #'com #f)))
       (with-syntax ([(param-spec ...)
                      (map (lambda (param-spec)
                             (syntax-case param-spec ()
                               ((param type)
                                (with-syntax ([type (translate com-type #'type #'type)])
                                  #'(param type)))
                               (opt (if (eq? (syntax->datum #'opt) '#:opt)
                                        param-spec
                                        (error "Finish this" param-spec)))))
                           (syntax->list #'(param-spec ...)))]
                     [rtype (translate com-type #'rtype #'rtype)])
         (if known-com
             (with-syntax ([known-com known-com])
               (syntax/loc stx
                 (def-com-method name known-com (param-spec ...) rtype)))
             (syntax/loc stx
               (def-com-method name #f (param-spec ...) rtype)))))]))


(def-com (calculate IRobotCalcEngine) () Boolean)
(def-com (new IRobotProject) ((type IRobotProjectType)) Void)
(def-com (close IRobotProject) () Void)
(def-com ((create-node Create) IRobotNodeServer) ((node-number Long) (x Double) (y Double) (z Double)) Void)
(def-com ((create-bar Create) IRobotBarServer) ((bar-number Long) (start-node Long) (end-node Long)) Void)
(def-com ((create-label Create) IRobotLabelServer) ((type Integer) (name String)) IRobotLabel)
(def-com (is-available IRobotLabelServer) ((type Integer) (name String)) Boolean)
(def-com (delete IRobotLabelServer) ((type Integer) (name String)) Void)
(def-com (store IRobotLabelServer) ((label IRobotLabel)) Void)
(def-com (get IRobotNodeServer) ((idx Integer)) IRobotNode)
(def-com (set-label IRobotNode) ((type Integer) (name String)) Void)
(def-com (set-value IRobotBarSectionConcreteData) ((type Integer) (value Double)) Void)
(def-com (CreateNonstd IRobotBarSectionData) ((rel_pos Double)) CreateNonstd)
(def-com (CalcNonstdGeometry IRobotBarSectionData) () Void)
(def-com (create-simple IRobotCaseServer) ((number Integer) (name String) (nature IRobotCaseNature) (analize-type IRobotCaseAnalizeType)) IRobotSimpleCase)
(def-com (SaveToDBase IRobotMaterialData) () Void)
(def-com (LoadFromDBase IRobotMaterialData) ((name String)) Boolean)
(def-com (add-one IRobotSelection) ((id Integer)) Void)

(define (new-project! type) ;I_PT_FRAME_2D, I_PT_SHELL, etc
  (close (project (application)))
  (new (project (application)) type))

(define (new-3d-project!)
  (new-project! I_PT_SHELL))

;;Labels
(define (new-label type name fn)
  (let ((labels (labels (structure (project (application))))))
    (when (is-available labels type name)
      (delete labels type name))
    (let* ((label (create-label labels type name))
           (data (data label)))
      (fn data)
      (store labels label)
      name)))

;;Node support
(define (create-node-support-label name ux uy uz rx ry rz)
  (new-label
   I_LT_NODE_SUPPORT
   name
   (lambda (support-data)
     (UX support-data (if ux 1 0))
     (UY support-data (if uy 1 0))
     (UZ support-data (if uz 1 0))
     (RX support-data (if rx 1 0))
     (RY support-data (if ry 1 0))
     (RZ support-data (if rz 1 0)))))

(define (set-node-support! node label)
  (set-label (get (nodes (structure (project (application)))) node) I_LT_NODE_SUPPORT label))

(define (add-support-node! p label)
  (let* ((node (add-node! p)))
    (set-node-support! node label)
    node))

;;Nodes
(define node-counter 0)
(define added-nodes (make-hasheq))

(define (add-node! p)
  (set! node-counter (+ node-counter 1))
  (create-node (nodes (structure (project (application)))) node-counter (cx p) (cy p) (cz p))
  (hash-set! added-nodes p node-counter)
  node-counter)

(define (current-nodes)
  (range (+ node-counter 1)))

;;Bars
(define bar-counter 0)

(define (add-bar! p0 p1)
  (set! bar-counter (+ bar-counter 1))
  (create-bar (bars (structure (project (application))))
              bar-counter
              (hash-ref added-nodes p0)
              (hash-ref added-nodes p1))
  bar-counter)

(define (current-bars)
  (range (+ bar-counter 1)))

(define (set-bar-rotation! bar angle)
  (Gamma (get (bars (structure (project (application)))) bar) angle))

;;Bar release

(define (create-bar-release-label name sux suy suz srx sry srz eux euy euz erx ery erz)
  (new-label
   I_LT_BAR_RELEASE
   name
   (lambda (data)
     (let ((start-node (start-node data))
           (end-node (end-node data)))
       (UX start-node sux)
       (UY start-node suy)
       (UZ start-node suz)
       (RX start-node srx)
       (RY start-node sry)
       (RZ start-node srz)
       (UX end-node eux)
       (UY end-node euy)
       (UZ end-node euz)
       (RX end-node erx)
       (RY end-node ery)
       (RZ end-node erz)))))

(define (set-bar-release! bar label)
  (set-label (get (bars (structure (project (application)))) bar) I_LT_BAR_RELEASE label))

;;Bar material

(define (create-bar-timber-material-label
         name _type
         _Timber_Type _Name _Nuance _E _NU _GMean _RO _LX _DumpCoef
         _RE_Bending _RE_AxTens _RE_TrTens _RE_AxCompr _RE_TrCompr _RE_Shear
         _E_5 _E_Trans)
  (new-label
   I_LT_BAR_MATERIAL
   name
   (lambda (data)
     (Type data _type)
     (Timber_Type data _Timber_Type)
     (Name data _Name)
     (Nuance data _Nuance)
     (E data _E)
     (NU data _NU)
     (GMean data _GMean)
     (RO data _RO)
     (LX data _LX)
     (DumpCoef data _DumpCoef)
     (RE_Bending data _RE_Bending)
     (RE_AxTens data _RE_AxTens)
     (RE_TrTens data _RE_TrTens)
     (RE_AxCompr data _RE_AxCompr)
     (RE_TrCompr data _RE_TrCompr)
     (RE_Shear data _RE_Shear)
     (E_5 data _E_5)
     (E_Trans data _E_Trans)
     (SaveToDBase data))))

(define (create-bar-material-label
         name _type
         _Name _Nuance _E _NU _Kirchoff _RO _LX _DumpCoef
         _RE _RT)
  (new-label
   I_LT_BAR_MATERIAL
   name
   (lambda (data)
     (Type data _type)
     (Name data _Name)
     (Nuance data _Nuance)
     (E data _E)
     (NU data _NU)
     (Kirchoff data _Kirchoff)
     (RO data _RO)
     (LX data _LX)
     (DumpCoef data _DumpCoef)
     (RE data _RE)
     (RT data _RT)
     (SaveToDBase data))))

;;Bar section

(define (create-bar-tube-section-label name material-name wood? specs)
  (new-label
   I_LT_BAR_SECTION
   name
   (lambda (data)
     (Type data I_BST_NS_TUBE)
     (shape-type data (if wood? I_BSST_WOOD_CIRC I_BSST_TUBE))
     (MaterialName data material-name)
     (for ((spec (in-list specs))
           (relative (division 0.0 1.0 (length specs))))
       (match spec
         ((list solid? diameter thickness)
          (let ((robotBarSectionNonstdData (CreateNonstd data relative)))
            (set-value robotBarSectionNonstdData I_BSNDV_BOX_H diameter)
            (unless solid?
              (set-value robotBarSectionNonstdData I_BSNDV_BOX_B thickness))))))
     (CalcNonstdGeometry data))))

(define (create-bar-rectangle-section-label name material-name wood? specs)
  (new-label
   I_LT_BAR_SECTION
   name
   (lambda (data)
     (Type data I_BST_NS_RECT)
     (shape-type data (if wood? I_BSST_FRTG I_BSST_RECT))
     (MaterialName data material-name)
     (for ((spec (in-list specs))
           (relative (division 0.0 1.0 (length specs))))
       (match spec
         ((list solid? width height thickness)
          (let ((robotBarSectionNonstdData (CreateNonstd data relative)))
            (set-value robotBarSectionNonstdData I_BSNDV_BOX_H width)
            (set-value robotBarSectionNonstdData I_BSNDV_BOX_B height)
            (unless solid?
              (set-value robotBarSectionNonstdData I_BSNDV_BOX_TF thickness)))))))))

(define (set-bar-section! bar label)
  (set-label (get (bars (structure (project (application)))) bar) I_LT_BAR_SECTION label))

(define (new-case number name nature analize-type setup process-results)
  (let* ((case (create-simple (cases (structure (project (application)))) number name nature analize-type))
         (records (records case)))
    (time (setup records))
    (time (calculate (calc-engine (project (application)))))
    (process-results (results (structure (project (application)))))))


(define (new-node-loads records nodes vec)
  (let* ((idx (new records I_LRT_NODE_FORCE))
         (record (get records idx))
         (objects (objects record)))
    (for ((node (in-list nodes)))
      (add-one objects node))
    (set-value record I_NFRV_FX (cx vec))
    (set-value record I_NFRV_FY (cy vec))
    (set-value record I_NFRV_FZ (cz vec))))