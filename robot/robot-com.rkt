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
(def-ro-property (selections IRobotStructure) IRobotSelection)
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
(def-ro-property (Nodes IRobotResultsServer) IRobotNodeResultServer)
(def-ro-property (Bars IRobotResultsServer) IRobotBarResultServer)
(def-ro-property (Displacements IRobotNodeResultServer) IRobotNodeDisplacementServer)
(def-ro-property (Stresses IRobotBarResultServer) IRobotBarStressServer)

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
(def-com ((set-selection-label SetLabel) IRobotBarServer) ((selection IRobotSelection) (type Integer) (name String)) Void) 
(def-com (set-value IRobotBarSectionConcreteData) ((type Integer) (value Double)) Void)
(def-com (CreateNonstd IRobotBarSectionData) ((rel_pos Double)) CreateNonstd)
(def-com (CalcNonstdGeometry IRobotBarSectionData) () Void)
(def-com (create-simple IRobotCaseServer) ((number Integer) (name String) (nature IRobotCaseNature) (analize-type IRobotCaseAnalizeType)) IRobotSimpleCase)
(def-com (SaveToDBase IRobotMaterialData) () Void)
(def-com (LoadFromDBase IRobotMaterialData) ((name String)) Boolean)
(def-com (add-one IRobotSelection) ((id Integer)) Void)
(def-com ((node-displacement Value) IRobotNodeDisplacementServer) ((node Integer) (case Integer)) IRobotDisplacementData)
(def-com ((bar-displacement Value) IRobotBarDisplacementServer) ((bar Integer) (pos Double) (case Integer)) IRobotDisplacementData)
(def-com ((bar-stress Value) IRobotBarStressServer) ((bar Integer) (case Integer) (pos Double)) IRobotBarStressData)
(def-com (from-text IRobotSelection) ((ids String)) Void)

;;Stress
(def-rw-property (Smax IRobotBarStressData) Double)
(def-rw-property (Smin IRobotBarStressData) Double)
(def-rw-property (Torsion IRobotBarStressData) Double)


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

;;Nodes
(struct truss-node-data
  ([id : Integer]
   [loc : Loc]
   [family : Any]
   [load : Any]))

(define node-counter (make-parameter 0))
(define added-nodes (make-parameter (make-hasheq)))
(define case-counter (make-parameter 0))
(define bar-counter (make-parameter 0))
(define added-bars (make-parameter (make-hasheq)))

(define (find-prox-node-data p)
  (for/or ([(k v) (in-hash (added-nodes))])
    (and (< (distance k p) reuse?)
         v)))

(define (add-node! p family [load #f] [reuse? #f])
  (or (and reuse?
           (find-prox-node-data p)) ;We should check that the families are the same. What about the loads?
      (begin
        (node-counter (+ (node-counter) 1))
        (let ((data (truss-node-data (node-counter) p family load)))
          (hash-set! (added-nodes) p data) ; Should we check for collisions here? (nodes at the same location)
          data))))

(define (current-nodes-ids)
  (range (+ (node-counter) 1)))

;;Bars
(struct truss-bar-data
  ([id : Integer]
   [node0 : truss-node-data]
   [node1 : truss-node-data]
   [rotation : Double]
   [family : Any]))

(define (add-bar! p0 p1 rotation family)
  (bar-counter (+ (bar-counter) 1))
  (let ((data (truss-bar-data (bar-counter)
                              (find-prox-node-data p0)
                              (find-prox-node-data p1)
                              rotation
                              family)))
    (hash-set! (added-bars) (bar-counter) data)
    data))

(define (current-bars-ids)
  (range (+ (bar-counter) 1)))


(require "../base/bim-families.rkt")

#;
(define (call-with-new-robot-analysis create-truss v process-results)
  (parameterize ((node-counter 0)
                 (added-nodes (make-hasheq))
                 (added-bars (make-hasheq))
                 (case-counter 0))
    (create-truss)
    (let ((nodes (nodes (structure (project (application)))))
          (bars (bars (structure (project (application))))))
      (for ((data (in-hash-values (added-nodes))))
        (let ((node-id (truss-node-data-id data))
              (p (truss-node-data-loc data))
              (node-family (truss-node-data-family data)))
          (create-node nodes node-id (cx p) (cy p) (cz p))
          (let ((support (truss-node-family-support node-family)))
            (when support
              (match support
                ((node-support name ux uy uz rx ry rz created?)
                 (unless created?
                   (create-node-support-label name ux uy uz rx ry rz)
                   (set-node-support-created?! support #t))
                 (set-label (get nodes node-id)
                            I_LT_NODE_SUPPORT
                            name)))))))
      (for ((data (in-hash-values (added-bars))))
        (let ((bar-id (truss-bar-data-id data))
              (node-id0 (truss-node-data-id (truss-bar-data-node0 data)))
              (node-id1 (truss-node-data-id (truss-bar-data-node1 data)))
              (rotation (truss-bar-data-rotation data))
              (bar-family (truss-bar-data-family data)))
          (create-bar bars bar-id node-id0 node-id1)
          (let ((bar (get bars bar-id)))
            (when (and rotation (> (abs rotation) 1e-16)) ;fix this
              (Gamma bar rotation))
            (when bar-family
              (let ((b (truss-bar-family-created? bar-family)))
                (unless (unbox b)
                  (match (truss-bar-family-material bar-family)
                    ((list name _type _Name _Nuance _E _NU _Kirchoff _RO _LX _DumpCoef _RE _RT)
                     (create-bar-material-label name _type _Name _Nuance _E _NU _Kirchoff _RO _LX _DumpCoef _RE _RT)))
                  (match (truss-bar-family-section bar-family)
                    ((list name material-name wood? specs)
                     (create-bar-tube-section-label name material-name wood? specs)))
                  (set-box! b #t)))
              (match (truss-bar-family-section bar-family)
                ((list name material-name wood? specs)      
                 (set-label bar I_LT_BAR_SECTION name)))))))
      (case-counter (+ (case-counter) 1))
      (new-case (case-counter)
                (format "Test-~A" (case-counter))
                I_CN_PERMANENT ; I_CN_EXPLOATATION I_CN_WIND I_CN_SNOW I_CN_TEMPERATURE I_CN_ACCIDENTAL I_CN_SEISMIC
                I_CAT_STATIC_LINEAR ;I_CAT_STATIC_NONLINEAR I_CAT_STATIC_FLAMBEMENT
                (lambda (records)
                  (new-node-loads records (current-nodes-ids) v))
                process-results))))

(define (call-with-new-robot-analysis create-truss v process-results)
  (parameterize ((node-counter 0)
                 (added-nodes (make-hasheq))
                 (added-bars (make-hasheq))
                 (case-counter 0))
    (create-truss)
    (let* ((structure (structure (project (application))))
           (nodes (nodes structure))
           (bars (bars structure))
           (node-loads (make-hasheq
                        (if v
                            (list (cons v (map truss-node-data-id (hash-values (added-nodes)))))
                            (list)))))
      (for ((data (in-hash-values (added-nodes))))
        (let ((node-id (truss-node-data-id data))
              (p (truss-node-data-loc data))
              (node-family (truss-node-data-family data))
              (node-load (truss-node-data-load data)))
          (create-node nodes node-id (cx p) (cy p) (cz p))
          (let ((support (truss-node-family-support node-family)))
            (when support
              (match support
                ((node-support name ux uy uz rx ry rz created?)
                 (unless created?
                   (create-node-support-label name ux uy uz rx ry rz)
                   (set-node-support-created?! support #t))
                 (set-label (get nodes node-id) I_LT_NODE_SUPPORT name)))))
          (when node-load
            (hash-update! node-loads node-load (lambda (ids) (cons node-id ids)) (list)))))
      (let ((family-bars (make-hasheq)))
        (for ((data (in-hash-values (added-bars))))
          (let ((bar-id (truss-bar-data-id data))
                (node-id0 (truss-node-data-id (truss-bar-data-node0 data)))
                (node-id1 (truss-node-data-id (truss-bar-data-node1 data)))
                (rotation (truss-bar-data-rotation data))
                (bar-family (truss-bar-data-family data)))
            (create-bar bars bar-id node-id0 node-id1)
            (when (and rotation (> (abs rotation) 1e-16)) ;fix this
              (Gamma (get bars bar-id) rotation))
            (hash-update! family-bars bar-family (lambda (bars) (cons bar-id bars)) (list))))
        (for (((bar-family bars-ids) (in-hash family-bars)))
          (let ((b (truss-bar-family-created? bar-family)))
            (unless (unbox b)
              (match (truss-bar-family-material bar-family)
                ((list name _type _Name _Nuance _E _NU _Kirchoff _RO _LX _DumpCoef _RE _RT)
                 (create-bar-material-label name _type _Name _Nuance _E _NU _Kirchoff _RO _LX _DumpCoef _RE _RT)))
              (match (truss-bar-family-section bar-family)
                ((list name material-name wood? specs)
                 (create-bar-tube-section-label name material-name wood? specs)))
              (set-box! b #t)))
          (let ((selection (get (selections structure) I_OT_BAR))
                (ids (open-output-string)))
            (for ((bar-id (in-list bars-ids)))
              (display bar-id ids) (display " " ids))
            (let ((str (get-output-string ids)))
              (from-text selection str))
            (match (truss-bar-family-section bar-family)
              ((list name material-name wood? specs)
               (set-selection-label bars selection I_LT_BAR_SECTION name))))))
      (case-counter (+ (case-counter) 1))
      (new-case (case-counter)
                (format "Test-~A" (case-counter))
                I_CN_PERMANENT ; I_CN_EXPLOATATION I_CN_WIND I_CN_SNOW I_CN_TEMPERATURE I_CN_ACCIDENTAL I_CN_SEISMIC
                I_CAT_STATIC_LINEAR ;I_CAT_STATIC_NONLINEAR I_CAT_STATIC_FLAMBEMENT
                (lambda (records)
                  (new-node-loads records node-loads))
                process-results))))

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

(define (new-node-loads records loads)
  (for (((vec ids) (in-hash loads)))
    (let* ((idx (new records I_LRT_NODE_FORCE))
           (record (get records idx))
           (objects (objects record)))
      (for ((node-id (in-list ids)))
        (add-one objects node-id))
      (set-value record I_NFRV_FX (cx vec))
      (set-value record I_NFRV_FY (cy vec))
      (set-value record I_NFRV_FZ (cz vec)))))
  
(define (node-displacement-vector results id case-id)
  (let ((d (node-displacement (Displacements (nodes results)) id case-id)))
    (vxyz (UX d)
          (UY d)
          (UZ d))))


(define (bar-displacement-vector results id case-id)
  (let ((d (node-displacement (Displacements (nodes results)) id case-id)))
    (vxyz (UX d)
          (UY d)
          (UZ d))))

(define (bar-max-stress results id case-id)
  (Smax (bar-stress (Stresses (bars results)) id case-id 0.0))) ;;The position should be changeable
  
