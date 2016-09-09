#lang typed/racket/base/no-check
(require (for-syntax racket/base racket/list))
(require racket/function racket/string racket/match racket/port racket/file)
(require (except-in math random-integer)
         "../base/typed-com.rkt"
         "../base/utils.rkt"
         "../base/coord.rkt"
         "ac-enums.rkt")

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

(define (ac-cm-color->color [color : AcCmColor]) : Color
  (rgb (cast (red color) Byte)
       (cast (green color) Byte)
       (cast (blue color) Byte)))

(define (color->ac-cm-color [color : Color]) : AcCmColor
  (let ((ac-cm-color
         (get-interface-object
          (string-append "AutoCAD.AcCmColor."
                         (substring (acadver) 0 2)))))
    (set-rgb! ac-cm-color (rgb-red color) (rgb-green color) (rgb-blue color))
    ac-cm-color))


;;Several types must be treated just as Com-Objects

(define-type 3DFace Com-Object)
(define-type 3DPolyline Com-Object)
(define-type 3DSolid Com-Object)
(define-type AcadMaterials Com-Object)
(define-type AcadSelectionSets Com-Object)
(define-type AcCmColor Com-Object)
(define-type All Com-Object)
(define-type Application Com-Object)
(define-type Arc Com-Object)
(define-type Block Com-Object)
(define-type Blocks Com-Object)
(define-type Circle Com-Object)
(define-type Database Com-Object)
(define-type Document Com-Object)
(define-type Ellipse Com-Object)
(define-type Id Com-Object)
(define-type LightweightPolyline Com-Object)
(define-type Line Com-Object)
(define-type ModelSpace Com-Object)
(define-type Object Com-Object)
(define-type Path Com-Object)
(define-type Point Com-Object)
(define-type PolyfaceMesh Com-Object)
(define-type PolygonMesh Com-Object)
(define-type Polyline Com-Object)
(define-type Profile Com-Object)
(define-type PViewport Com-Object)
(define-type Ray Com-Object)
(define-type Region Com-Object)
(define-type SelectionSet Com-Object)
(define-type Spline Com-Object)
(define-type Text Com-Object)
(define-type Viewport Com-Object)
(define-type Views Com-Object)

(define-type VarIdN (Variant (Vectorof Id)))

; initialization

(define (initialize) : Void
  (let* ((app
          (let ((clsid (progid->clsid "AutoCAD.Application")))
            (with-handlers ((exn?
                             (λ (e)
                               (display "Starting AutoCAD...")
                               (flush-output)
                               (begin0
                                 (com-create-instance clsid)
                                 (displayln "done!")))))
              (com-get-active-object clsid))))
         (doc (cast (com-get-property app "ActiveDocument") Com-Object))
         (mod (cast (com-get-property doc "ModelSpace") Com-Object))
         (utl (cast (com-get-property doc "Utility") Com-Object)))
    (set! application (lambda () app))
    (set! active-document (lambda () doc))
    (set! active-modelspace (lambda () mod))
    (set! utility (lambda () utl))
    (com-set-property! app "Visible" #t)
    (reset-ucs)
    (delobj 0)
    (osmode 0) 
    ;(nomutt 1)
    ;(cmdecho 0)
    ;(expert 5)
    
    ;(objectsnap)
    ;(surfaceassociativity 0)
    ;(surfacemodelingmode 1)
    ;(solidhist 0)
    
    ;(start-undo-mark)
    ;(undo-off)
    ))

(define (application) : Com-Object
  (initialize)
  (application))

(define (active-document) : Com-Object
  (initialize)
  (active-document))

(define (active-modelspace) : Com-Object
  (initialize)
  (active-modelspace))

(define (utility) : Com-Object
  (initialize)
  (utility))

(define-cached (autolisp-functions) : Com-Object
  (vl-load-com)
  (let ((lisp (get-interface-object "VL.Application.16")))
    (cast (com-get-property
           (cast (com-get-property lisp "ActiveDocument")
                 Com-Object)
           "Functions")
          Com-Object)))

(begin-for-syntax
  (require syntax/id-table)
  (define com-obj (make-immutable-free-id-table 
                   (list (cons #'Application #'(application))
                         (cons #'Document #'(active-document))
                         (cons #'ModelSpace #'(active-modelspace))
                         (cons #'Utility #'(utility)))))
  (define com-type (make-immutable-free-id-table
                    (map (lambda (id) (cons id id #;#'Com-Object))
                         (list #'3DFace
                               #'3DPolyline
                               #'3DSolid
                               #'PolygonMesh
                               #'AcCmColor
                               #'Arc
                               #'Circle
                               #'Ellipse
                               #'LightweightPolyline
                               #'Line
                               #'Mirrored
                               #'Path
                               #'Point
                               #'PolyfaceMesh
                               #'Polyline
                               #'Profile
                               #'PViewport
                               #'Ray
                               #'Id
                               #'Region
                               #'Spline
                               #'Text
                               #'Viewport
                               #'AcadMaterials
                               #'AcadSelectionSets
                               #'Object
                               #'Blocks
                               #'Database
                               #'Document
                               #'All))))
  (define (translate table stx default)
    (if (identifier? stx)
        (free-id-table-ref table stx default)
        stx)))

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
     [Shorts      VarIntegerN  Integers->VarShortN]
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
     [IdsOrVoid   Refs         ids-or-void->refs]
     [AcCmColor   Color        ac-cm-color->color]
     [Color       AcCmColor    color->ac-cm-color]))

#|
; object properties

(def-com-property center coord<-vector)
(def-com-property closed (boolean boolean))
(def-com-property control-points coords<-flat-vector)
(def-com-property elevation)
(def-com-property end-angle)
(def-com-property handle)
(def-com-property material (String String))
(def-com-property m-close (boolean boolean))
(def-com-property n-close (boolean boolean))
(def-com-property object-name)
(def-com-property object-normal coord<-vector)
(def-com-property position coord<-vector)
(def-com-property (circle-radius "Radius") Float)
(def-com-property start-angle)
(def-com-property custom-scale Float)

(provide ac-simple-mesh ac-quad-surface-mesh ac-cubic-surface-mesh ac-bezier-surface-mesh)
(define ac-simple-mesh 0)
(define ac-quad-surface-mesh 5)
(define ac-cubic-surface-mesh 6)
(define ac-bezier-surface-mesh 8)
(def-com-property type (Integer Integer))

(def-com-property visible (boolean boolean))

(define (convert-3dpolyline obj)
  (let ((type (object-name obj)))
    (cond ((string=? type "AcDb3dPolyline")
           (let ((cs (coordinates obj)))
             (begin0
               (for/list : (Listof Com-Object)
                 ((start (in-list cs))
                  (end (in-list (cdr cs))))
                 (add-line start end))
               (delete obj))))
          (else
           obj))))

(provide convert-3dpolylines)
(define (convert-3dpolylines objs)
  (apply append (map convert-3dpolyline objs)))

; viewport

;; viewport properties

(define (get-viewport-center viewport)
  (vector-double-flonum->xyz
   (com-get-property viewport "Center")))

(define (set-viewport-center! viewport center)
  (com-set-property! viewport "Center" (point center)))


(define (get-viewport-height viewport)
  (com-get-property viewport "Height"))

(define (set-viewport-height! viewport height)
  (com-set-property! viewport "Height" height))


(define (get-viewport-snap-on viewport)
  (com-get-property viewport "SnapOn"))

(define (set-viewport-snap-on! viewport snap-on?)
  (com-set-property! viewport "SnapOn" snap-on?))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;NEW VERSION



;;PROPERTIES

#|
(def-rw-property (Action SecurityParams) AcadSecurityParamsType)
(def-ro-property (Active Document) Boolean)
(def-rw-property (ActiveDimStyle Document) DimStyle)
(def-rw-property (ActiveDocument Application) Document)
(def-rw-property (ActiveLayer Document) Layer)
(def-rw-property (ActiveLayout Document) Layout)
(def-rw-property (ActiveLinetype Document) Linetype)
(ActiveMaterial property idh_activematerial.htm)
(def-rw-property (ActiveProfile PreferencesProfiles) String)
|#
(def-rw-property (active-p-viewport Document) PViewport)
(def-ro-property (active-selection-set Document) SelectionSet)
#|
(def-rw-property (ActiveSpace Document) acActiveSpace)
(def-rw-property (ActiveTextStyle Document) TextStyle)
(ActiveUCS property idh_activeucs.htm)
|#
(def-rw-property (active-viewport Document) Viewport)
#|(def-rw-property (ADCInsertUnitsDefaultSource PreferencesUser) AcInsertUnits)
(def-rw-property (ADCInsertUnitsDefaultTarget PreferencesUser) AcInsertUnits)
(def-ro-property (AdjustForBackground DwfUnderlay) Long)
(def-ro-property (AffectsGraphics FileDependency) Boolean)
(def-rw-property (Algorithm SecurityParams) AcadSecurityParamsConstants)
(Alignment property idh_alignment.htm)
(def-rw-property (AlignmentPointAcquisition PreferencesDrafting) acAlignmentPointAcquisition)
(def-rw-property (AlignSpace MLeaderStyle) Long)
(def-ro-property (AllowedValues DynamicBlockReferenceProperty) Variant)
(def-rw-property (AllowLongSymbolNames DatabasePreferences) Boolean)
(def-rw-property (AllowManualHeights Table) Boolean)
(def-rw-property (AllowManualPositions Table) Boolean)
(def-rw-property (AltFontFile PreferencesFiles) String)
(def-rw-property (AltRoundDistance DimAligned) Double)
(def-rw-property (AltSuppressLeadingZeros DimAligned) Boolean)
(def-rw-property (AltSuppressTrailingZeros DimAligned) Boolean)
(def-rw-property (AltSuppressZeroFeet DimAligned) Boolean)
(def-rw-property (AltSuppressZeroInches DimAligned) Boolean)
(def-rw-property (AltTabletMenuFile PreferencesFiles) String)
(def-rw-property (AltTextPrefix DimAligned) String)
(def-rw-property (AltTextSuffix DimAligned) String)
(def-rw-property (AltTolerancePrecision DimAligned) acDimPrecision)
(AltToleranceSuppressLeadingZeros property idh_alttolerancesuppressleadingzeros.htm)
(AltToleranceSuppressTrailingZeros property idh_alttolerancesuppresstrailingzeros.htm)
(AltToleranceSuppressZeroFeet property idh_alttolerancesuppresszerofeet.htm)
(AltToleranceSuppressZeroInches property idh_alttolerancesuppresszeroinches.htm)
(def-rw-property (AltUnits DimAligned) Boolean)
(def-rw-property (AltUnitsFormat DimAligned) acDimUnits)
(def-rw-property (AltUnitsPrecision DimAligned) acDimPrecision)
(def-rw-property (AltUnitsScale DimAligned) Double)
(def-ro-property (Angle Line) Double)
(def-rw-property (AngleFormat Dim3PointAngular) acAngleUnits)
(def-rw-property (AngleVertex Dim3PointAngular) Double)
(Annotation property idh_annotation.htm)
(def-rw-property (Annotative MLeaderStyle) Boolean)
(def-ro-property (Application All) Application)
(def-rw-property (ArcEndParam DimArcLength) Double)
(def-ro-property (ArcLength Arc) Double)
(def-rw-property (ArcPoint DimArcLength) Variant)
(def-rw-property (ArcSmoothness PViewport) Integer)
(def-rw-property (ArcStartParam DimArcLength) Double)
(def-rw-property (Area Arc) Double)
(def-rw-property (Arrowhead1Block Dim3PointAngular) String)
(def-rw-property (Arrowhead1Type Dim3PointAngular) acDimArrowheadType)
(def-rw-property (Arrowhead2Block Dim3PointAngular) String)
(def-rw-property (Arrowhead2Type Dim3PointAngular) acDimArrowheadType)
(def-rw-property (ArrowheadBlock DimRadial) String)
(def-rw-property (ArrowheadSize Dim3PointAngular) Double)
(def-rw-property (ArrowheadType DimRadial) acDimArrowheadType)
(def-rw-property (ArrowSize MLeaderStyle) Long)
(def-rw-property (ArrowSymbol MLeaderStyle) Long)
(def-ro-property (AssociativeHatch Hatch) Boolean)
(def-rw-property (AttachmentPoint MText) acAttachmentPoint)
(def-rw-property (Author SummaryInfo) String)
(def-rw-property (AutoAudit PreferencesOpenSave) Boolean)
(def-rw-property (AutomaticPlotLog PreferencesOutput) Boolean)
(def-rw-property (AutoSaveInterval PreferencesOpenSave) Integer)
(def-rw-property (AutoSavePath PreferencesFiles) String)
(def-rw-property (AutoSnapAperture PreferencesDrafting) Boolean)
(def-rw-property (AutoSnapApertureSize PreferencesDrafting) Long)
(def-rw-property (AutoSnapMagnet PreferencesDrafting) Boolean)
(def-rw-property (AutoSnapMarker PreferencesDrafting) Boolean)
(def-com (AutoSnapMarkerColor PreferencesDrafting) nil nil)
(def-rw-property (AutoSnapMarkerSize PreferencesDrafting) Long)
(def-rw-property (AutoSnapToolTip PreferencesDrafting) Boolean)
(AutoTrackingVecColor property idh_autotrackingveccolor.htm)
(def-rw-property (AutoTrackTooltip PreferencesDrafting) Boolean)
(def-ro-property (AxisDirection RevolvedSurface) Variant)
(AxisPosition property idh_axisposition.htm)
(def-rw-property (BackgroundFill MText) Boolean)
(BackgroundLinesColor property idh_backgroundlinescolor.htm)
(def-ro-property (BackgroundLinesHiddenLine SectionTypeSettings) Long)
(def-ro-property (BackgroundLinesLayer SectionTypeSettings) String)
(def-ro-property (BackgroundLinesLinetype SectionTypeSettings) String)
(BackgroundLinesLinetypeScale property idh_backgroundlineslinetypescale.htm)
(BackgroundLinesLineweight property idh_backgroundlineslineweight.htm)
(def-ro-property (BackgroundLinesPlotStyleName SectionTypeSettings) String)
(def-ro-property (BackgroundLinesVisible SectionTypeSettings) Long)
(def-rw-property (Backward Attribute) Boolean)
(def-ro-property (Bank SweptSurface) Long)
(def-rw-property (BasePoint Ray) Double)
(def-ro-property (BaseRadius Helix) Double)
(def-rw-property (BatchPlotProgress Plot) Boolean)
(def-rw-property (BeepOnError PreferencesSystem) Boolean)
(def-rw-property (BigFontFile TextStyle) String)
(def-rw-property (BitFlags TableStyle) Long)
(def-ro-property (Block Layout) Block)
(def-rw-property (Block MLeaderStyle) String)
(def-rw-property (BlockColor MLeaderStyle) AcCmColor)
(def-rw-property (BlockConnectionType MLeader) AcBlockConnectionType)
(Blocks property idh_blocks.htm)
(def-rw-property (BlockScale MLeaderStyle) Long)
(def-rw-property (BlockScaling Block) AcBlockScaling)
|#
(def-ro-property (blue AcCmColor) Long)
#|(def-ro-property (BookName AcCmColor) String)
(def-ro-property (BottomHeight Section) Long)
(def-rw-property (BreaksEnabled Table) Boolean)
(def-rw-property (BreakSize MLeaderStyle) Double)
(def-rw-property (BreakSpacing Table) Double)
(def-rw-property (Brightness Raster) Integer)
(def-rw-property (CanonicalMediaName Layout) String)
(def-ro-property (Caption Application) String)
(def-rw-property (CategoryName View) String)
|#
(def-rw-property (center Arc) VarDouble3)
#|
(def-rw-property (CenterMarkSize DimDiametric) Double)
(def-rw-property (CenterPlot Layout) Boolean)
(def-rw-property (CenterPoint DimArcLength) Variant)
(def-rw-property (CenterType DimDiametric) acDimCenterType)
(Centroid property idh_centroid.htm)
(def-rw-property (Check PopupMenuItem) Boolean)
(def-rw-property (ChordPoint DimRadialLarge) Variant)
(def-rw-property (Circumference Circle) Double)
(def-ro-property (Clipped PViewport) Boolean)
(def-rw-property (ClippingEnabled DwfUnderlay) Boolean)
|#
(def-rw-property (closed 3DPolyline) Boolean)
(def-rw-property (color All) AcCmColor)
#|
(def-rw-property (ColorBookPath PreferencesFiles) String)
(def-rw-property (ColorIndex AcCmColor) acColor)
(def-rw-property (ColorMethod AcCmColor) acColorMethod)
(def-ro-property (ColorName AcCmColor) String)
(def-rw-property (Columns MInsertBlock) Long)
(def-rw-property (Columns Table) Long)
(def-rw-property (ColumnSpacing MInsertBlock) Double)
(def-ro-property (ColumnWidth Table) Double)
(def-rw-property (Comment SecurityParams) String)
(def-rw-property (Comments Block) String)
(def-rw-property (Comments SummaryInfo) String)
(def-ro-property (ConfigFile PreferencesFiles) String)
(def-rw-property (ConfigName Layout) String)
(def-rw-property (Constant Attribute) Boolean)
(def-rw-property (ConstantWidth LightweightPolyline) Double)
(Constrain property idh_constrain.htm)
(def-rw-property (ContentBlockName MLeader) String)
(def-rw-property (ContentBlockType MLeader) AcPredefBlockType)
(def-rw-property (ContentType MLeaderStyle) AcMLeaderContentType)
(def-rw-property (ContinuousPlotLog PreferencesOutput) Boolean)
(def-rw-property (ContourlinesPerSurface DatabasePreferences) Integer)
(def-rw-property (Contrast DwfUnderlay) Integer)
(def-rw-property (ControlPoints Spline) Double)
|#
(def-rw-property (coordinates All) VarDouble3N)
(def-rw-property ((2d-coordinates Coordinates) All) VarDouble2N)
(def-rw-property ((point-coordinates Coordinates) All) VarDouble3)
(def-ro-property (count All) Integer)
#|(def-rw-property (CreateBackup PreferencesOpenSave) Boolean)
(CurrentSectionType property idh_currentsectiontype.htm)
(def-rw-property (CursorSize PreferencesDisplay) Integer)
(CurveTangencyLinesColor property idh_curvetangencylinescolor.htm)
(def-ro-property (CurveTangencyLinesLayer SectionTypeSettings) String)
(def-ro-property (CurveTangencyLinesLinetype SectionTypeSettings) String)
(CurveTangencyLinesLinetypeScale property idh_curvetangencylineslinetypescale.htm)
(CurveTangencyLinesLineweight property idh_curvetangencylineslineweight.htm)
(def-ro-property (CurveTangencyLinesPlotStyleName SectionTypeSettings) String)
(def-ro-property (CurveTangencyLinesVisible SectionTypeSettings) Long)
(def-rw-property (CustomDictionary PreferencesFiles) String)
(def-ro-property (CustomIconPath PreferencesFiles) String)
(def-rw-property (CustomScale PViewport) Double)
(def-ro-property (Database Document) Database)
(def-rw-property (DecimalSeparator Dim3PointAngular) String)
(def-rw-property (DefaultInternetURL PreferencesFiles) String)
(def-rw-property (DefaultOutputDevice PreferencesOutput) String)
(def-rw-property (DefaultPlotStyleForLayer PreferencesOutput) String)
(def-rw-property (DefaultPlotStyleForObjects PreferencesOutput) String)
(def-rw-property (DefaultPlotStyleTable PreferencesOutput) String)
(def-ro-property (DefaultPlotToFilePath PreferencesOutput) String)
(def-rw-property (DefaultPlotToFilePath PreferencesOutput) String)
(def-ro-property (Degree Spline) Integer)
(def-ro-property (Delta Line) Double)
(def-rw-property (DemandLoadArxApp PreferencesOpenSave) acARXDemandLoad)
(def-ro-property (Description DynamicBlockReferenceProperty) String)
(DestinationBlock property idh_destinationblock.htm)
(def-ro-property (DestinationFile SectionTypeSettings) String)
(def-rw-property (Diameter Circle) Double)
(Dictionaries property idh_dictionaries.htm)
(def-rw-property (DimensionLineColor Dim3PointAngular) acColor)
(def-rw-property (DimensionLineExtend DimAligned) Double)
(def-ro-property (DimensionLinetype DimAligned) String)
(def-rw-property (DimensionLineWeight Dim3PointAngular) acLineWeight)
(def-rw-property (DimLine1Suppress Dim3PointAngular) Boolean)
(def-rw-property (DimLine2Suppress Dim3PointAngular) Boolean)
(def-rw-property (DimLineInside Dim3PointAngular) Boolean)
(def-rw-property (DimLineSuppress DimRadial) Boolean)
(DimStyles property idh_dimstyles.htm)
|#
(def-rw-property (direction All) VarDouble3)
#|
(def-rw-property (DirectionVector Ray) Double)
(def-ro-property (Display Preferences) PreferencesDisplay)
(def-rw-property (DisplayGrips PreferencesSelection) Boolean)
(def-rw-property (DisplayGripsWithinBlocks PreferencesSelection) Boolean)
(def-rw-property (DisplayLayoutTabs PreferencesDisplay) Boolean)
(def-rw-property (DisplayLocked PViewport) Boolean)
(def-rw-property (DisplayOLEScale PreferencesSystem) Boolean)
(def-rw-property (DisplayScreenMenu PreferencesDisplay) Boolean)
(def-rw-property (DisplayScrollBars PreferencesDisplay) Boolean)
(def-rw-property (DisplaySilhouette DatabasePreferences) Boolean)
(def-rw-property (DockedVisibleLines PreferencesDisplay) Integer)
(def-ro-property (DockStatus Toolbar) acToolbarDockStatus)
(def-ro-property (Document Blocks) Document)
(Documents property idh_documents.htm)
(def-rw-property (DogLegged MLeader) Boolean)
(def-rw-property (DoglegLength MLeader) Double)
(def-ro-property (Drafting Preferences) PreferencesDrafting)
(def-rw-property (DrawingDirection MText) acDrawingDirection)
(def-rw-property (DrawLeaderOrderType MLeaderStyle) AcDrawLeaderOrderType)
(def-rw-property (DrawMLeaderOrderType MLeaderStyle) AcDrawLeaderOrderType)
(def-rw-property (DriversPath PreferencesFiles) String)
(def-ro-property (EffectiveName BlockRef) String)
|#
(def-rw-property (elevation All) Double)
#|
(def-rw-property (ElevationModelSpace Document) Double)
(def-rw-property (ElevationPaperSpace Document) Double)
(def-rw-property (Enable PopupMenuItem) Boolean)
(def-rw-property (EnableBlockRotation MLeaderStyle) Boolean)
(def-rw-property (EnableBlockScale MLeaderStyle) Boolean)
(def-rw-property (EnableDogleg MLeaderStyle) Boolean)
(def-rw-property (EnableFrameText MLeaderStyle) Boolean)
(def-rw-property (EnableLanding MLeaderStyle) Boolean)
(def-ro-property (EnableShadowDisplay ShadowDisplay) Long)
(def-rw-property (EnableStartupDialog PreferencesSystem) Boolean)
(def-rw-property (EndAngle Arc) Double)
(def-ro-property (EndDraftAngle LoftedSurface) Long)
(def-ro-property (EndDraftMagnitude LoftedSurface) Long)
(def-rw-property (EndParameter Ellipse) Double)
|#
(def-rw-property (end-point All) VarDouble3)
#|
(def-rw-property (EndSubMenuLevel PopupMenuItem) Integer)
(def-rw-property (EndTangent Spline) Double)
(def-ro-property (EnterpriseMenuFile PreferencesFiles) String)
(def-rw-property (EntityColor AcCmColor) Long)
(def-rw-property (Explodable Block) Boolean)
(def-rw-property (ExtensionLineColor Dim3PointAngular) acColor)
(def-rw-property (ExtensionLineExtend Dim3PointAngular) Double)
(def-rw-property (ExtensionLineOffset Dim3PointAngular) Double)
(def-rw-property (ExtensionLineWeight Dim3PointAngular) acLineWeight)
(def-rw-property (ExtLine1EndPoint Dim3PointAngular) Double)
(def-ro-property (ExtLine1Linetype DimAligned) String)
(def-rw-property (ExtLine1Point DimAligned) Double)
(def-rw-property (ExtLine1StartPoint DimAngular) Double)
(def-rw-property (ExtLine1Suppress Dim3PointAngular) Boolean)
(def-rw-property (ExtLine2EndPoint Dim3PointAngular) Double)
(def-ro-property (ExtLine2Linetype DimAligned) String)
(def-rw-property (ExtLine2Point DimAligned) Double)
(def-rw-property (ExtLine2StartPoint DimAngular) Double)
(def-rw-property (ExtLine2Suppress Dim3PointAngular) Boolean)
(def-ro-property (ExtLineFixedLen DimAligned) Double)
(def-ro-property (ExtLineFixedLenSuppress DimAligned) Long)
(def-rw-property (Fade DwfUnderlay) Integer)
(def-ro-property (Feature FileDependency) String)
(def-rw-property (FieldLength Attribute) Integer)
(def-ro-property (File DwfUnderlay) String)
(def-ro-property (FileName FileDependency) String)
(def-ro-property (Files Preferences) PreferencesFiles)
(def-ro-property (FileSize FileDependency) Long)
(def-ro-property (FingerprintGUID FileDependency) String)
(def-rw-property (FirstSegmentAngleConstraint MLeaderStyle) Long)
(def-rw-property (Fit Dim3PointAngular) acDimFit)
(def-rw-property (FitPoints Spline) Double)
(def-rw-property (FitTolerance Spline) Double)
(def-rw-property (FloatingRows Toolbar) Integer)
(def-rw-property (FlowDirection Table) AcTableDirection)
(def-ro-property (Flyout ToolbarItem) Toolbar)
(def-rw-property (FontFile TextStyle) String)
(def-rw-property (FontFileMap PreferencesFiles) String)
(def-rw-property (ForceLineInside Dim3PointAngular) Boolean)
(ForegroundLinesColor property idh_foregroundlinescolor.htm)
(def-ro-property (ForegroundLinesEdgeTransparency SectionTypeSettings) Long)
(def-ro-property (ForegroundLinesFaceTransparency SectionTypeSettings) Long)
(def-ro-property (ForegroundLinesHiddenLine SectionTypeSettings) Long)
(def-ro-property (ForegroundLinesLayer SectionTypeSettings) String)
(def-ro-property (ForegroundLinesLinetype SectionTypeSettings) String)
(ForegroundLinesLinetypeScale property idh_foregroundlineslinetypescale.htm)
(ForegroundLinesLineweight property idh_foregroundlineslineweight.htm)
(def-ro-property (ForegroundLinesPlotStyleName SectionTypeSettings) String)
(def-ro-property (ForegroundLinesVisible SectionTypeSettings) Long)
(def-ro-property (FoundPath FileDependency) String)
(def-rw-property (FractionFormat Dim3PointAngular) acDimFractionType)
(def-rw-property (Freeze Layer) Boolean)
(def-rw-property (FullCRCValidation PreferencesOpenSave) Boolean)
(def-ro-property (FullFileName FileDependency) String)
(def-ro-property (FullName Application) String)
(def-rw-property (FullScreenTrackingVector PreferencesDrafting) Boolean)
(GenerationOptions property idh_generationoptions.htm)
(GradientAngle property idh_gradientangle.htm)
(def-rw-property (GradientCentered Hatch) Boolean)
(GradientColor1 property idh_gradientcolor1.htm)
(GradientColor2 property idh_gradientcolor2.htm)
(def-rw-property (GradientName Hatch) String)
(GraphicsWinLayoutBackgrndColor property idh_graphicswinlayoutbackgrndcolor.htm)
(GraphicsWinModelBackgrndColor property idh_graphicswinmodelbackgrndcolor.htm)
|#
(def-ro-property (green AcCmColor) Long)
#|(def-rw-property (GridOn Viewport) Boolean)
(def-rw-property (GripColorSelected PreferencesSelection) acColor)
(def-rw-property (GripColorUnselected PreferencesSelection) acColor)
(def-rw-property (GripSize PreferencesSelection) Long)
(Groups property idh_groups.htm)
|#
(def-ro-property (handle Blocks) String)
#|
(def-ro-property (HasAttributes BlockRef) Boolean)
(def-ro-property (HasExtensionDictionary All) Boolean)
(def-rw-property (HasLeader DimArcLength) Boolean)
(def-ro-property (HasSheetView PViewport) Boolean)
(def-ro-property (HasSubSelection Table) Boolean)
(def-rw-property (HasVpAssociation View) Boolean)
(def-rw-property (HatchObjectType Hatch) AcHatchObjectType)
(def-rw-property (HatchStyle Hatch) acHatchStyle)
(def-rw-property (HeaderSuppressed Table) Boolean)
|#
(def-rw-property (height All) Double)
#|
(def-rw-property (HelpFilePath PreferencesFiles) String)
(def-rw-property (HelpString PopupMenuItem) String)
(History property idh_history.htm)
(def-rw-property (HistoryLines PreferencesDisplay) Integer)
(def-rw-property (HorizontalTextPosition Dim3PointAngular) acDimHorizontalJustification)
(def-rw-property (HorzCellMargin Table) Double)
(def-ro-property (HWND Document) Long_PTR)
(def-ro-property (HWND32 Document) Long)
(def-rw-property (HyperlinkBase SummaryInfo) String)
(def-rw-property (HyperlinkDisplayCursor PreferencesUser) Boolean)
(def-rw-property (HyperlinkDisplayTooltip PreferencesUser) Boolean)
(def-ro-property (Hyperlinks All) Hyperlinks)
(def-rw-property (ImageFile Raster) String)
(def-rw-property (ImageFrameHighlight PreferencesDisplay) Boolean)
(def-rw-property (ImageHeight Raster) Double)
(def-rw-property (ImageVisibility Raster) Boolean)
(def-rw-property (ImageWidth Raster) Double)
(def-rw-property (IncrementalSavePercent PreferencesOpenSave) Integer)
(def-ro-property (Index PopupMenuItem) Integer)
(def-ro-property (Index FileDependency) Long)
(IndicatorFillColor property idh_indicatorfillcolor.htm)
(def-ro-property (IndicatorTransparency Section) Long)
(def-rw-property (InsertionPoint Attribute) Double)
|#
(def-ro-property (insertion-point All) VarDouble3)
#|
(def-ro-property (InsUnits BlockRef) String)
(def-ro-property (InsUnitsFactor BlockRef) Double)
(IntersectionBoundaryColor property idh_intersectionboundarycolor.htm)
(def-ro-property (IntersectionBoundaryDivisionLines SectionTypeSettings) Long)
(def-ro-property (IntersectionBoundaryLayer SectionTypeSettings) String)
(def-ro-property (IntersectionBoundaryLinetype SectionTypeSettings) String)
(IntersectionBoundaryLinetypeScale property idh_intersectionboundarylinetypescale.htm)
(IntersectionBoundaryLineweight property idh_intersectionboundarylineweight.htm)
(def-ro-property (IntersectionBoundaryPlotStyleName SectionTypeSettings) String)
(IntersectionFillColor property idh_intersectionfillcolor.htm)
(def-ro-property (IntersectionFillFaceTransparency SectionTypeSettings) Long)
(IntersectionFillHatchAngle property idh_intersectionfillhatchangle.htm)
(def-ro-property (IntersectionFillHatchPatternName SectionTypeSettings) String)
(IntersectionFillHatchPatternType property idh_intersectionfillhatchpatterntype.htm)
(IntersectionFillHatchScale property idh_intersectionfillhatchscale.htm)
(def-ro-property (IntersectionFillHatchSpacing SectionTypeSettings) Double)
(def-ro-property (IntersectionFillLayer SectionTypeSettings) String)
(def-ro-property (IntersectionFillLinetype SectionTypeSettings) String)
(IntersectionFillLinetypeScale property idh_intersectionfilllinetypescale.htm)
(IntersectionFillLineweight property idh_intersectionfilllineweight.htm)
(def-ro-property (IntersectionFillPlotStyleName SectionTypeSettings) String)
(def-ro-property (IntersectionFillVisible SectionTypeSettings) Long)
(def-rw-property (Invisible Attribute) Boolean)
(def-ro-property (IsCloned IDPair) Boolean)
(def-ro-property (IsDynamicBlock Block) Boolean)
(def-ro-property (IsLayout Block) Boolean)
(def-ro-property (IsModified FileDependency) Boolean)
(ISOPenWidth property idh_isopenwidth.htm)
(def-ro-property (IsOwnerXlated IDPair) Boolean)
(def-rw-property (IsPartial DimArcLength) Boolean)
(def-ro-property (IsPeriodic Spline) Boolean)
(def-ro-property (IsPlanar Spline) Boolean)
(def-ro-property (IsPrimary IDPair) Boolean)
(def-ro-property (IsQuiescent AcadState) Boolean)
(def-ro-property (IsRational Spline) Boolean)
(def-rw-property (Issuer SecurityParams) String)
(def-ro-property (IsXRef Block) Boolean)
(def-ro-property (ItemName DwfUnderlay) String)
(def-rw-property (JogAngle DimRadialLarge) ACAD_ANGLE)
(def-rw-property (JogLocation DimRadialLarge) Variant)
(Justification property idh_justification.htm)
(def-ro-property (Key IDPair) Long_PTR)
(def-ro-property (Key32 IDPair) Long)
(def-rw-property (KeyboardAccelerator PreferencesUser) acKeyboardAccelerator)
(def-rw-property (KeyboardPriority PreferencesUser) acKeyboardPriority)
(def-rw-property (KeyLength SecurityParams) Long)
(def-rw-property (Keywords SummaryInfo) String)
(def-rw-property (Knots Spline) Double)
(def-rw-property (Label PopupMenuItem) String)
(def-rw-property (LabelBlockId PViewport) Long_PTR)
(def-rw-property (LabelBlockId32 PViewport) Long_PTR)
(def-rw-property (LandingGap MLeader) Double)
(def-ro-property (LargeButtons Toolbar) Boolean)
(def-rw-property (LastHeight TextStyle) Double)
(def-rw-property (LastSavedBy SummaryInfo) String)
|#
(def-rw-property (layer All) String)
#|
(def-rw-property (LayerOn Layer) Boolean)
|#
(def-ro-property (layers Document) Id)
#|
(def-rw-property (LayerState View) String)
(def-rw-property (Layout ModelSpace) Layout)
(def-rw-property (LayoutCreateViewport PreferencesDisplay) Boolean)
(LayoutCrosshairColor property idh_layoutcrosshaircolor.htm)
(def-rw-property (LayoutDisplayMargins PreferencesDisplay) Boolean)
(def-rw-property (LayoutDisplayPaper PreferencesDisplay) Boolean)
(def-rw-property (LayoutDisplayPaperShadow PreferencesDisplay) Boolean)
(def-ro-property (LayoutID View) Long_PTR)
(def-ro-property (LayoutID32 View) Long_PTR)
(def-ro-property (Layouts Document) Layouts)
(def-rw-property (LayoutShowPlotSetup PreferencesDisplay) Boolean)
(def-rw-property (Leader1Point DimArcLength) Variant)
(def-rw-property (Leader2Point DimArcLength) Variant)
(def-ro-property (LeaderLength DimDiametric) Double)
(def-rw-property (LeaderLineColor MLeader) AcadAcCmColor)
(def-rw-property (LeaderLinetype MLeader) ACAD_LTYPE)
(def-rw-property (LeaderLineTypeId MLeaderStyle) ACAD_LTYPE)
(def-rw-property (LeaderLineWeight MLeader) ACAD_LWEIGHT)
(def-rw-property (LeaderType MLeader) AcMLeaderType)
(def-rw-property (Left Toolbar) Integer)
(def-ro-property (Length Line) Double)
|#
(def-rw-property (lens-length PViewport) Double)
#|
(def-rw-property (Limits Document) Double)
(def-rw-property (LinearScaleFactor DimAligned) Double)
(def-rw-property (LineSpacingDistance MText) Double)
(def-rw-property (LineSpacingFactor MText) Double)
(def-rw-property (LineSpacingStyle MText) acLineSpacingStyle)
(def-rw-property (Linetype All) String)
(def-rw-property (LinetypeGeneration LightweightPolyline) Boolean)
(Linetypes property idh_linetypes.htm)
(def-rw-property (LinetypeScale All) Double)
(def-rw-property (Lineweight All) acLineWeight)
(def-rw-property (LineweightDisplay DatabasePreferences) Boolean)
(def-ro-property (LiveSectionEnabled Section) Long)
(def-rw-property (LoadAcadLspInAllDocuments PreferencesSystem) Boolean)
(def-ro-property (LocaleID Application) Long)
(def-rw-property (Lock Layer) Boolean)
(def-rw-property (LockAspectRatio OLE) Boolean)
(def-rw-property (LockPosition Attribute) Boolean)
(def-rw-property (LogFileOn PreferencesOpenSave) Boolean)
(def-rw-property (LogFilePath PreferencesFiles) String)
(LowerLeftCorner property idh_lowerleftcorner.htm)
(def-rw-property (Macro PopupMenuItem) String)
(def-rw-property (MainDictionary PreferencesFiles) String)
|#
(def-rw-property (major-axis Ellipse) Double)
(def-rw-property (major-radius Ellipse) Double)
#|
(def-ro-property (Mask LayerStateManager) enum)
|#
(def-rw-property (material All) String)
#|
(def-rw-property (Material Table) String)
|#
(def-ro-property (materials Document) AcadMaterials)
#|
(def-rw-property (MaxActiveViewports DatabasePreferences) Integer)
(def-rw-property (MaxAutoCADWindow PreferencesDisplay) Boolean)
(def-rw-property (MaxLeaderSegmentsPoints MLeaderStyle) Integer)
|#
(def-rw-property (m-close PolygonMesh) Boolean)
#|
(def-rw-property (Mdensity PolygonMesh) Integer)
(def-ro-property (Measurement Dim3PointAngular) Double)
(def-ro-property (MenuBar Application) MenuBar)
(MenuFile property idh_menufile.htm)
(def-ro-property (MenuFileName MenuGroup) String)
(MenuGroups property idh_menugroups.htm)
(Menus property idh_menus.htm)
(def-ro-property (MinimumTableHeight Table) Double)
(def-ro-property (MinimumTableWidth Table) Double)
|#
(def-ro-property (minor-axis Ellipse) Double)
(def-rw-property (minor-radius Ellipse) Double)
#|
(def-ro-property (MLineScale MLine) Double)
(def-rw-property (Mode Attribute) acAttributeMode)
(ModelCrosshairColor property idh_modelcrosshaircolor.htm)
|#
(def-rw-property (modelspace Document) ModelSpace)
#|
(ModelType property idh_modeltype.htm)
(def-rw-property (ModelView PViewport) View)
(def-ro-property (MomentOfInertia 3DSolid) Double)
(def-ro-property (Monochrome DwfUnderlay) Long)
(def-ro-property (MRUNumber PreferencesOpenSave) Long)
(def-rw-property (MSpace Document) Boolean)
(def-rw-property (MTextAttribute Attribute) Boolean)
(def-rw-property (MTextAttribute Attribute) Boolean)
(def-rw-property (MTextAttribute Attribute) Boolean)
(def-ro-property (MTextAttributeContent Attribute) String)
(def-ro-property (MTextAttributeContent Attribute) String)
(def-ro-property (MTextAttributeContent Attribute) String)
(def-ro-property (MTextBoundaryWidth Attribute) Double)
(def-ro-property (MTextBoundaryWidth Attribute) Double)
(def-rw-property (MTextDrawingDirection Attribute) AcDrawingDirection)
(def-rw-property (MTextDrawingDirection Attribute) AcDrawingDirection)
(def-ro-property (MVertexCount PolygonMesh) Integer)
|#
(def-rw-property (name Application) String)
#|
(def-ro-property (NameNoMnemonic PopupMenu) String)
|#
(def-rw-property (n-close PolygonMesh) Boolean)
#|
(def-rw-property (NDensity PolygonMesh) Integer)
|#
(def-rw-property (normal All) VarDouble3)
#|
(def-ro-property (NumberOfControlPoints Spline) Integer)
(def-rw-property (NumberOfCopies Plot) Long)
(def-ro-property (NumberOfFaces PolyFaceMesh) Long)
(def-ro-property (NumberOfFitPoints Spline) Integer)
(NumberOfLoops property idh_numberofloops.htm)
(def-ro-property (NumberOfVertices PolyFaceMesh) Long)
(def-com (NumCellStyles TableStyle) nil Long)
(def-ro-property (NumCrossSections LoftedSurface) Long)
(def-ro-property (NumGuidePaths LoftedSurface) Long)
(NumVertices property idh_numvertices.htm)
(def-ro-property (NVertexCount PolygonMesh) Integer)
(def-ro-property (ObjectID Blocks) Long_PTR)
(def-ro-property (ObjectID32 Blocks) Long)
|#
(def-ro-property (object-name Com-Object) String)
#|
(def-rw-property (ObjectSnapMode Document) Boolean)
(def-rw-property (ObjectSortByPlotting DatabasePreferences) Boolean)
(def-rw-property (ObjectSortByPSOutput DatabasePreferences) Boolean)
(def-rw-property (ObjectSortByRedraws DatabasePreferences) Boolean)
(def-rw-property (ObjectSortByRegens DatabasePreferences) Boolean)
(def-rw-property (ObjectSortBySelection DatabasePreferences) Boolean)
(def-rw-property (ObjectSortBySnap DatabasePreferences) Boolean)
(def-rw-property (ObliqueAngle Attribute) Double)
(def-rw-property (OleItemType OLE) AcOleType)
(def-rw-property (OLELaunch DatabasePreferences) Boolean)
(def-rw-property (OlePlotQuality OLE) AcOlePlotQuality)
(OLEQuality property idh_olequality.htm)
(OleSourceApp property idh_ole_sourceapp.htm)
(def-ro-property (OnMenuBar PopupMenu) Boolean)
(def-ro-property (OpenSave Preferences) PreferencesOpenSave)
(def-rw-property (Origin Block) Double)
(def-rw-property (OrthoOn Viewport) Boolean)
(def-ro-property (Output Preferences) PreferencesOutput)
(def-rw-property (OverrideCenter DimRadialLarge) Variant)
(def-ro-property (OwnerID Blocks) Long_PTR)
(def-ro-property (OwnerID TableStyle) Long_PTR)
(def-ro-property (OwnerID32 Blocks) Long)
(def-rw-property (PageSetupOverridesTemplateFile PreferencesFiles) String)
(PaperSpace property idh_paperspace.htm)
(def-rw-property (PaperUnits Layout) acPlotPaperUnits)
(def-ro-property (Parent MenuBar) the)
(def-rw-property (Password SecurityParams) String)
(def-ro-property (Path Application) String)
(def-rw-property (PatternAngle Hatch) Double)
(def-rw-property (PatternDouble Hatch) Boolean)
(def-rw-property (PatternName Hatch) String)
(def-rw-property (PatternScale Hatch) Double)
(def-rw-property (PatternSpace Hatch) Double)
(def-ro-property (PatternType Hatch) acPatternType)
(def-ro-property (Perimeter Region) Double)
(def-rw-property (PickAdd PreferencesSelection) Boolean)
(def-rw-property (PickAuto PreferencesSelection) Boolean)
(def-rw-property (PickBoxSize PreferencesSelection) Long)
(def-rw-property (PickDrag PreferencesSelection) Boolean)
(def-rw-property (PickFirst PreferencesSelection) Boolean)
(PickfirstSelectionSet property idh_pickfirstselectionset.htm)
(def-rw-property (PickGroup PreferencesSelection) Boolean)
(def-ro-property (Plot Document) Plot)
(PlotConfigurations property idh_plotconfigurations.htm)
(def-rw-property (PlotHidden Layout) Boolean)
(def-rw-property (PlotLegacy PreferencesOutput) Boolean)
(def-rw-property (PlotLogFilePath PreferencesFiles) String)
(PlotOrigin property idh_plotorigin.htm)
(def-rw-property (PlotPolicy PreferencesOutput) acPlotPolicy)
(def-rw-property (PlotRotation Layout) acPlotRotation)
(def-rw-property (PlotStyleName All) String)
(def-rw-property (Plottable Layer) Boolean)
(def-rw-property (PlotType Layout) acPlotType)
(def-rw-property (PlotViewportBorders Layout) Boolean)
(def-rw-property (PlotViewportsFirst Layout) Boolean)
(def-rw-property (PlotWithLineweights Layout) Boolean)
(def-rw-property (PlotWithPlotStyles Layout) Boolean)
(def-rw-property (PolarTrackingVector PreferencesDrafting) Boolean)
(Position property idh_position.htm)
(def-rw-property (PostScriptPrologFile PreferencesFiles) String)
(def-ro-property (Preferences Application) DatabasePreferences)
(def-rw-property (Preset Attribute) Boolean)
(def-rw-property (PrimaryUnitsPrecision DimAligned) acDimPrecision)
(def-ro-property (PrincipalDirections 3DSolid) Double)
(def-ro-property (PrincipalMoments 3DSolid) Double)
(def-rw-property (PrinterConfigPath PreferencesFiles) String)
(def-rw-property (PrinterDescPath PreferencesFiles) String)
(def-rw-property (PrinterPaperSizeAlert PreferencesOutput) Boolean)
(def-rw-property (PrinterSpoolAlert PreferencesOutput) acPrinterSpoolAlert)
(def-rw-property (PrinterStyleSheetPath PreferencesFiles) String)
(def-rw-property (PrintFile PreferencesFiles) String)
(def-rw-property (PrintSpoolerPath PreferencesFiles) String)
(def-rw-property (PrintSpoolExecutable PreferencesFiles) String)
(def-ro-property (ProductOfInertia 3DSolid) Double)
(ProfileRotation property idh_profilerotation.htm)
(def-ro-property (Profiles Preferences) PreferencesProfiles)
(def-rw-property (PromptString Attribute) String)
(def-ro-property (PropertyName DynamicBlockReferenceProperty) String)
(def-rw-property (ProviderName SecurityParams) String)
(def-rw-property (ProviderType SecurityParams) Long)
(def-rw-property (ProxyImage PreferencesOpenSave) acProxyImage)
(def-rw-property (QNewTemplateFile PreferencesFiles) String)
(def-rw-property (QuietErrorMode Plot) Boolean)
(def-ro-property (RadiiOfGyration 3DSolid) Double)
|#
(def-rw-property (radius Arc) Double)
(def-rw-property (radius-ratio Ellipse) Double)
#|
(def-ro-property (RoOnly Document) Boolean)
|#
(def-ro-property (red AcCmColor) Long)
#|
(def-ro-property (ReferenceCount FileDependency) Long)
(def-rw-property (RegenerateTableSuppressed Table) Boolean)
(RegisteredApplications property idh_registeredapplications.htm)
(def-rw-property (RemoveHiddenLines PViewport) Boolean)
(def-rw-property (RenderSmoothness DatabasePreferences) Double)
(def-rw-property (RepeatBottomLabels Table) Boolean)
(def-rw-property (RepeatTopLabels Table) Boolean)
(def-rw-property (RevisionNumber SummaryInfo) String)
(RevolutionAngle property idh_revolutionangle.htm)
(Rotation property idh_ole_rotation.htm)
(def-rw-property (Rotation Attribute) Double)
(def-rw-property (RoundDistance DimAligned) Double)
(def-rw-property (RowHeight Table) Double)
(def-rw-property (Rows MInsertBlock) Long)
(def-rw-property (Rows Table) Long)
(def-rw-property (RowSpacing MInsertBlock) Double)
(def-rw-property (SaveAsType PreferencesOpenSave) acSaveAsType)
(def-ro-property (Saved Document) Boolean)
(def-rw-property (SavePreviewThumbnail PreferencesOpenSave) Boolean)
(def-ro-property (scale SweptSurface) Double)
(def-rw-property (ScaleFactor Attribute) Double)
(def-rw-property (ScaleHeight OLE) Double)
(def-rw-property (ScaleLineweights Layout) Boolean)
(def-rw-property (ScaleWidth OLE) Double)
(def-rw-property (SCMCommandMode PreferencesUser) AcDrawingAreaSCMCommand)
(def-rw-property (SCMDefaultMode PreferencesUser) AcDrawingAreaSCMDefault)
(def-rw-property (SCMEditMode PreferencesUser) AcDrawingAreaSCMEdit)
(def-rw-property (SCMTimeMode PreferencesUser) Boolean)
(def-rw-property (SCMTimeValue PreferencesUser) Integer)
(def-rw-property (SecondPoint Ray) Double)
(def-rw-property (SecondSegmentAngleConstraint MLeaderStyle) Long)
(def-ro-property (SectionManager Database) AcadSectionManager)
(def-rw-property (SegmentPerPolyline DatabasePreferences) Integer)
(def-ro-property (Selection Preferences) PreferencesSelection)
|#
(def-rw-property (selection-sets Document) AcadSelectionSets)
#|
(def-rw-property (SerialNumber SecurityParams) String)
(def-rw-property (ShadePlot PViewport) AcShadePlot)
(ShadowDisplay property idh_shadowdisplay.htm)
(def-rw-property (SheetView PViewport) View)
(def-ro-property (ShortCutMenu PopupMenu) Boolean)
(def-rw-property (ShortCutMenuDisplay PreferencesUser) Boolean)
(def-ro-property (Show DynamicBlockReferenceProperty) Boolean)
(def-ro-property (ShowHistory 3dSolid) Long)
(def-rw-property (ShowPlotStyles Layout) Boolean)
(def-rw-property (ShowProxyDialogBox PreferencesOpenSave) Boolean)
(def-rw-property (ShowRasterImage PreferencesDisplay) Boolean)
(def-rw-property (ShowRotation Raster) Boolean)
(def-rw-property (ShowWarningMessages PreferencesSystem) Boolean)
(def-rw-property (SingleDocumentMode PreferencesSystem) Boolean)
(SnapBasePoint property idh_snapbasepoint.htm)
(def-rw-property (SnapOn Viewport) Boolean)
(def-rw-property (SnapRotationAngle Viewport) Double)
(def-rw-property (SolidFill DatabasePreferences) Boolean)
(def-ro-property (SolidType 3dSolid) String)
(SourceObjects property idh_sourceobjects.htm)
(StandardScale property idh_standardscale.htm)
(def-rw-property (StartAngle Arc) Double)
(def-ro-property (StartDraftAngle LoftedSurface) Long)
(def-ro-property (StartDraftMagnitude LoftedSurface) Long)
(def-rw-property (StartParameter Ellipse) Double)
|#
(def-rw-property (start-point All) VarDouble3)
#|(State property idh_state.htm)
(def-com (StatusID Application) ((VportObj Viewport)) Boolean)
(def-rw-property (StoreSQLIndex PreferencesSystem) Boolean)
(def-rw-property (StyleName Attribute) String)
(def-rw-property (StyleName Table) String)
(def-rw-property (StyleSheet Layout) String)
(def-rw-property (Subject SecurityParams) String)
(def-rw-property (Subject SummaryInfo) String)
(def-ro-property (SubMenu PopupMenuItem) PopupMenu)
(SummaryInfo property idh_summaryinfo_interface.htm)
(def-rw-property (SupportPath PreferencesFiles) String)
(def-rw-property (SuppressLeadingZeros Dim3PointAngular) Boolean)
(def-rw-property (SuppressTrailingZeros Dim3PointAngular) Boolean)
(def-rw-property (SuppressZeroFeet DimAligned) Boolean)
(def-rw-property (SuppressZeroInches DimAligned) Boolean)
(def-ro-property (SurfaceNormals LoftedSurface) Long)
(def-ro-property (SurfaceType Surface) String)
(SymbolPosition property idh_symbolposition.htm)
(def-ro-property (System Preferences) PreferencesSystem)
(def-rw-property (TableBreakFlowDirection Table) AcTableFlowDirection)
(def-rw-property (TableBreakHeight Table) Double)
(def-rw-property (TablesRoOnly PreferencesSystem) Boolean)
(def-ro-property (TableStyleOverrides Table) Variant)
(def-rw-property (TabOrder Layout) Long)
(def-rw-property (TagString Attribute) String)
(TaperAngle property idh_taperangle.htm)
|#
(def-rw-property (target All) VarDouble3)
#|
(def-rw-property (TempFileExtension PreferencesOpenSave) String)
(def-rw-property (TempFilePath PreferencesFiles) String)
(def-rw-property (TemplateDwgPath PreferencesFiles) String)
(def-rw-property (TempXRefPath PreferencesFiles) String)
(def-rw-property (TextAlignmentPoint Attribute) Double)
(def-rw-property (TextAlignmentType MLeaderStyle) AcTextAlignmentType)
(def-rw-property (TextAngleType MLeaderStyle) AcTextAngleType)
(def-rw-property (TextAttachmentType MLeader) AcTextAttachmentType)
(def-rw-property (TextBackgroundFill MLeader) Boolean)
(def-rw-property (TextColor Dim3PointAngular) acColor)
(def-rw-property (TextDirection MLeader) AcDrawingDirection)
(def-rw-property (TextEditor PreferencesFiles) String)
(def-ro-property (TextFill Dimension) Long)
(TextFillColor property idh_textfillcolor.htm)
(def-rw-property (TextFont PreferencesDisplay) String)
(def-rw-property (TextFontSize PreferencesDisplay) Integer)
(def-rw-property (TextFontStyle PreferencesDisplay) acTextFontStyle)
(def-rw-property (TextFrameDisplay DatabasePreferences) Boolean)
(def-rw-property (TextGap Dim3PointAngular) Double)
(def-rw-property (TextGenerationFlag Attribute) acTextGenerationFlag)
(def-rw-property (TextHeight Dim3PointAngular) Double)
(def-rw-property (TextInside Dim3PointAngular) Boolean)
(def-rw-property (TextInsideAlign Dim3PointAngular) Boolean)
(def-rw-property (TextJustify MLeader) AcAttachmentPoint)
(def-rw-property (TextLeftAttachmentType MLeader) AcTextAttachmentType)
(def-rw-property (TextLineSpacingDistance MLeader) Double)
(def-rw-property (TextLineSpacingFactor MLeader) Double)
(def-rw-property (TextLineSpacingStyle MLeader) AcLineSpacingStyle)
(def-rw-property (TextMovement Dim3PointAngular) acDimTextMovement)
(def-rw-property (TextOutsideAlign Dim3PointAngular) Boolean)
(def-rw-property (TextOverride Dim3PointAngular) String)
(def-rw-property (TextPosition Dim3PointAngular) Double)
(def-rw-property (TextPrecision DimAngular) acDimPrecision)
(def-rw-property (TextPrefix Dim3PointAngular) String)
(def-rw-property (TextRightAttachmentType MLeader) AcTextAttachmentType)
(def-rw-property (TextRotation Dim3PointAngular) Double)
|#
(def-rw-property (text-string All) String)
#|
(def-rw-property (TextStyle Dim3PointAngular) String)
(def-rw-property (TextStyleName MLeader) String)
(TextStyles property idh_textstyles.htm)
(def-rw-property (TextSuffix Dim3PointAngular) String)
(def-rw-property (TextureMapPath PreferencesFiles) String)
(def-rw-property (TextWidth MLeader) Double)
(TextWinBackgrndColor property idh_textwinbackgrndcolor.htm)
(TextWinTextColor property idh_textwintextcolor.htm)
(def-rw-property (Thickness Arc) Double)
(def-rw-property (TimeServer SecurityParams) String)
(def-ro-property (TimeStamp FileDependency) Long)
(def-rw-property (Title SummaryInfo) String)
(def-rw-property (TitleSuppressed Table) Boolean)
(def-rw-property (ToleranceDisplay Dim3PointAngular) acDimToleranceMethod)
(def-rw-property (ToleranceHeightScale Dim3PointAngular) Double)
(def-rw-property (ToleranceJustification Dim3PointAngular) acDimToleranceJustify)
(def-rw-property (ToleranceLowerLimit Dim3PointAngular) Double)
(def-rw-property (TolerancePrecision Dim3PointAngular) acDimPrecision)
(def-rw-property (ToleranceSuppressLeadingZeros Dim3PointAngular) Boolean)
(ToleranceSuppressTrailingZeros property idh_tolerancesuppresstrailingzeros.htm)
(def-rw-property (ToleranceSuppressZeroFeet DimAligned) Boolean)
(def-rw-property (ToleranceSuppressZeroInches DimAligned) Boolean)
(def-rw-property (ToleranceUpperLimit Dim3PointAngular) Double)
(Toolbars property idh_toolbars.htm)
(def-rw-property (ToolPalettePath PreferencesFiles) String)
(def-rw-property (Top Toolbar) Integer)
(def-ro-property (TopHeight Section) Double)
(def-ro-property (TopRadius Helix) Double)
(def-ro-property (TotalAngle Arc) Double)
(def-ro-property (TotalLength Helix) Double)
(def-rw-property (TranslateIDs XRecord) Boolean)
(def-rw-property (Transparency Raster) Boolean)
|#
(def-rw-property (true-color All) AcCmColor)
#|(def-rw-property (TrueColorImages PreferencesDisplay) Boolean)
(def-ro-property (TurnHeight Helix) Double)
(Turns property idh_turns.htm)
(def-ro-property (TurnSlope Helix) ACAD_ANGLE)
(Twist property idh_helix_twist.htm)
(Twist property idh_sweptsurface_twist.htm)
(def-rw-property (TwistAngle PViewport) Double)
(Type property idh_type.htm)
(def-rw-property (UCSIconAtOrigin PViewport) Boolean)
(def-rw-property (UCSIconOn PViewport) Boolean)
(def-rw-property (UCSPerViewport PViewport) Boolean)
(def-ro-property (UIsolineDensity Surface) Long)
(def-ro-property (UnderlayLayerOverrideApplied DwfUnderlay) Long)
(def-ro-property (UnderlayName DwfUnderlay) String)
(UnderlayVisibility property idh_underlayvisibility.htm)
(def-rw-property (Units Block) AcInsertUnits)
(def-rw-property (UnitsFormat DimAligned) acDimLUnits)
(def-ro-property (UnitsType DynamicBlockReferenceProperty) AcDynamicBlockReferencePropertyUnitsType)
(UpperRightCorner property idh_upperrightcorner.htm)
(def-rw-property (UpsideDown Attribute) Boolean)
(def-rw-property (URL Hyperlink) String)
(def-rw-property (URLDescription Hyperlink) String)
(def-rw-property (URLNamedLocation Hyperlink) String)
(def-ro-property (Used Layer) Boolean)
(def-rw-property (UseLastPlotSettings PreferencesOutput) Boolean)
(def-ro-property (User Preferences) PreferencesUser)
(def-rw-property (UseStandardScale Layout) Boolean)
(def-ro-property (Utility Document) Utility)
(def-ro-property (Value DynamicBlockReferenceProperty) DynamicBlockReferenceProperty)
(def-ro-property (Value32 DynamicBlockReferenceProperty) DynamicBlockReferenceProperty)
(def-ro-property (VBE Application) VBE)
(def-rw-property (Verify Attribute) Boolean)
|#
(def-ro-property (version Application) String)
#|
(def-ro-property (VersionGUID FileDependency) String)
(def-rw-property (VertCellMargin Table) Double)
(VerticalDirection property idh_verticaldirection.htm)
(def-rw-property (VerticalTextPosition Dim3PointAngular) acDimVerticalJustification)
(Vertices property idh_vertices.htm)
(ViewingDirection property idh_viewingdirection.htm)
(def-rw-property (ViewportDefault Layer) Boolean)
(def-rw-property (ViewportOn PViewport) Boolean)
(Viewports property idh_viewports.htm)
|#
(def-ro-property (views Document) Views)
#|(def-rw-property (ViewToPlot Layout) String)
(def-rw-property (VisibilityEdge1 3DFace) Boolean)
(def-rw-property (VisibilityEdge2 3DFace) Boolean)
(def-rw-property (VisibilityEdge3 3DFace) Boolean)
(def-rw-property (VisibilityEdge4 3DFace) Boolean)
|#
;;HACK is this working? It doesn't seem like it
(def-rw-property (visible All) Boolean)
#|
(def-ro-property (VIsolineDensity Surface) Long)
(def-ro-property (Volume 3DSolid) Double)
(def-rw-property (Weights Spline) Double)
|#
(def-rw-property (width Application) Double)
#|
(def-rw-property (WindowLeft Application) Integer)
|#
(def-rw-property (window-state Application) AcWindowState)
#|
(def-ro-property (WindowTitle Document) String)
(def-rw-property (WindowTop Application) Integer)
(def-rw-property (WorkspacePath PreferencesFiles) String)
(XEffectiveScaleFactor property idh_xeffectivescalefactor.htm)
(def-ro-property (XRefDatabase Block) Database)
(def-rw-property (XRefDemandLoad PreferencesOpenSave) acXRefDemandLoad)
(def-rw-property (XRefEdit DatabasePreferences) Boolean)
(def-rw-property (XRefFadeIntensity PreferencesDisplay) Long)
(def-rw-property (XRefLayerVisibility DatabasePreferences) Boolean)
(def-rw-property (XScaleFactor BlockRef) Double)
(def-rw-property (XVector UCS) Double)
(YEffectiveScaleFactor property idh_yeffectivescalefactor.htm)
(def-rw-property (YScaleFactor BlockRef) Double)
(def-rw-property (YVector UCS) Double)
(ZEffectiveScaleFactor property idh_zeffectivescalefactor.htm)
(def-rw-property (ZScaleFactor BlockRef) Double)
|#

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

(def-com (activate Document) () Void)
(def-com (add All) ((Name String)) Id)
(def-com ((add-3d-face Add3DFace) ModelSpace) ((Point1 VarDouble3) (Point2 VarDouble3) (Point3 VarDouble3) (Point4 VarDouble3)) 3DFace)
(def-com ((add-3d-mesh Add3dMesh) ModelSpace) ((M Integer) (N Integer) (PointsMatrix VarDouble3N)) PolygonMesh)
(def-com ((add-3d-poly Add3DPoly) ModelSpace) ((PointsArray VarDouble3N)) 3DPolyline)
(def-com (add-arc ModelSpace) ((Center VarDouble3) (Radius Double) (StartAngle Double) (EndAngle Double)) Arc)
;(def-com (AddAttribute ModelSpace) ((Height Double) (Mode AcAttributeMode) (Prompt String) (InsertionPoint VarDouble3) (Tag String) (Value String)) Attribute)
(def-com (add-box ModelSpace) ((Origin VarDouble3) (Length Double) (Width Double) (Height Double)) 3DSolid)
(def-com (add-circle ModelSpace) ((Center VarDouble3) (Radius Double)) Circle)
(def-com (add-cone ModelSpace) ((Center VarDouble3) (BaseRadius Double) (Height Double)) 3DSolid)
#|
(def-com (AddCustomInfo SummaryInfo) ((key String) (Value String)) Void)
(def-com (AddCustomObject ModelSpace) ((ClassName String)) Custom)
|#
(def-com (add-cylinder ModelSpace) ((Center VarDouble3) (Radius Double) (Height Double)) 3DSolid)
#|
(def-com (AddDim3PointAngular ModelSpace) ((AngleVertex VarDouble3) (FirstEndPoint VarDouble3) (SecondEndPoint VarDouble3) (TextPoint VarDouble3)) Dim3PointAngular)
(def-com (AddDimAligned ModelSpace) ((ExtLine1Point VarDouble3) (ExtLine2Point VarDouble3) (TextPosition VarDouble3)) DimAligned)
(def-com (AddDimAngular ModelSpace) ((AngleVertex VarDouble3) (FirstEndPoint VarDouble3) (SecondEndPoint VarDouble3) (TextPoint VarDouble3)) DimAngular)
(def-com (AddDimArc ModelSpace) ((ArcCenter VarDouble3) (FirstEndPoint VarDouble3) (SecondEndPoint VarDouble3) (ArcPoint VarDouble3)) DimArcLength)
(def-com (AddDimDiametric ModelSpace) ((ChordPoint VarDouble3) (FarChordPoint VarDouble3) (LeaderLength Double)) DimDiametric)
(def-com (AddDimOrdinate ModelSpace) ((DefinitionPoint VarDouble3) (LeaderEndPoint VarDouble3) (UseXAxis Integer)) DimOrdinate)
(def-com (AddDimRadial ModelSpace) ((Center VarDouble3) (ChordPoint VarDouble3) (LeaderLength Double)) DimRadial)
(def-com (AddDimRadialLarge ModelSpace) ((Center VarDouble3) (ChordPoint VarDouble3) (OverrideCenter VarDouble3) (JogPoint VarDouble3) (JogAngle VarDouble3)) DimRadialLarge)
(def-com (AddDimRotated ModelSpace) ((XLine1Point VarDouble3) (XLine2Point VarDouble3) (DimLineLocation VarDouble3) (RotationAngle Double)) DimRotated)
|#
(def-com (add-ellipse ModelSpace) ((Center VarDouble3) (MajorAxis VarDouble3) (RadiusRatio Double)) Ellipse)
(def-com (add-elliptical-cone ModelSpace) ((Center VarDouble3) (MajorRadius Double) (MinorRadius Double) (Height Double)) 3DSolid)
(def-com (add-elliptical-cylinder ModelSpace) ((Center VarDouble3) (MajorRadius Double) (MinorRadius Double) (Height Double)) 3DSolid)
(def-com (add-extruded-solid ModelSpace) ((Section Profile) (Height Double) (TaperAngle Double)) 3DSolid)
(def-com (add-extruded-solid-along-path ModelSpace) ((Section Profile) (Curve Path)) 3DSolid)
#|
(def-com (AddFitPoint Spline) ((Index Integer) (FitPoint VarDouble3)) Void)
(def-com (AddHatch ModelSpace) ((PatternType AcPatternType) (PatternName String) (Associativity Boolean) (HatchObjectType OPT-HatchObjectType)) Hatch)
|#
(def-com (add-items AcadSelectionSet) ([items VarIdN]) Void)
#|
(def-com (AddLeader ModelSpace) ((PointsArray VarDouble3N) (Annotation Object) (Type AcLeaderType)) Leader)
|#
(def-com ((add-lightweight-polyline AddLightWeightPolyline) ModelSpace) ((VerticesList VarDouble3N)) LightweightPolyline)
(def-com (add-line ModelSpace) ((StartPoint VarDouble3) (EndPoint VarDouble3)) Line)
#|
;(AddMenuItem method idh_addmenuitem.htm)
(def-com (AddMInsertBlock ModelSpace) ((InsertionPoint VarDouble3) (Name String) (XScale Double) (YScale Double) (ZScale Double) (Rotation Double) (NumRows Long) (NumColumns Long) (RowSpacing Long) (ColumnSpacing Long) (Password Variant)) MInsertBlock)
(def-com (AddMLeader ModelSpace) ((pointsArray VarDouble3) (leaderLineIndex Long)) MLeader)
(def-com (AddMLine ModelSpace) ((VertexList VarDouble3N)) MLine)
(def-com (AddMtext ModelSpace) ((InsertionPoint VarDouble3) (Width Double) (Text String)) MText)
(def-com (AddObject Dictionary) ((Keyword String) (ObjectName String)) Object)
|#
(def-com (add-point ModelSpace) ((Pt VarDouble3)) Point)
;;AML FIX THE ANY
(def-com (add-polyface-mesh ModelSpace) ((VerticesList VarDouble3N) (FaceList VarIntegerN)) PolyfaceMesh)
(def-com (add-polyline ModelSpace) ((VerticesList VarDouble3N)) Polyline)
#|
(def-com (AddPViewport PaperSpace) ((Center VarDouble3) (Width Double) (Height Double)) PViewport)
(def-com (AddRaster ModelSpace) ((ImageFileName String) (InsertionPoint VarDouble3) (ScaleFactor Double) (RotationAngle Double)) Raster)
|#
(def-com (add-ray ModelSpace) ((Point1 VarDouble3) (Point2 VarDouble3)) Ray)
(def-com (add-region ModelSpace) ((ObjectList VarIdN)) VarIdN)
(def-com (add-revolved-solid ModelSpace) ((Profile Region) (AxisPoint VarDouble3) (AxisDir VarDouble3) (Angle Double)) 3DSolid)

(def-com (add-section Block) ((FromPoint VarDouble3) (ToPoint VarDouble3) (PlaneVector VarDouble3)) Com-Object) ;;Is there a better return type?
;(AddSeparator method idh_addseparator.htm)
#|(def-com (AddShape ModelSpace) ((Name String) (InsertionPoint VarDouble3) (ScaleFactor Double) (Rotation Double)) Shape)
(def-com (AddSolid ModelSpace) ((Point1 VarDouble3) (Point2 VarDouble3) (Point3 VarDouble3) (Point4 VarDouble3)) Solid)
|#
(def-com (add-sphere ModelSpace) ((Center VarDouble3) (Radius Double)) 3DSolid)
(def-com (add-spline ModelSpace) ((PointsArray VarDouble3N) (StartTangent VarDouble3) (EndTangent VarDouble3)) Spline)
#|
;(AddSubMenu method idh_addsubmenu.htm)
(def-com (AddTable ModelSpace) ((InsertionPoint Integer) (NumRows Long) (NumColumns Long) (RowHeight Double) (ColWidth Double)) Table)
|#
(def-com (add-text ModelSpace) ((TextString String) (InsertionPoint VarDouble3) (Height Double)) Text)
#|
(def-com (AddTolerance ModelSpace) ((Text String) (InsertionPoint VarDouble3) (Direction VarDouble3)) Tolerance)
(def-com (AddToolbarButton Toolbar) ((Index Variant) (Name String) (HelpString String) (Macro String) (FlyoutButton Variant)) ToolbarItem)
|#
(def-com (add-torus ModelSpace) ((Center VarDouble3) (TorusRadius Double) (TubeRadius Double)) 3DSolid)
#|
(def-com (AddTrace ModelSpace) ((PointsArray VarDouble3N)) Trace)
(def-com (AddVertex LightweightPolyline) ((Index Integer) (Point VarDouble3)) Void)
|#
(def-com (add-wedge ModelSpace) ((Center VarDouble3) (Length Double) (Width Double) (Height Double)) 3DSolid)
#|
(def-com (AddXLine ModelSpace) ((Point1 VarDouble3) (Point2 VarDouble3)) XLine)
(def-com (AddXRecord Dictionary) ((Keyword String)) XRecord)
(def-com (AngleFromXAxis Utility) ((Point1 VarDouble3) (Point2 VarDouble3)) Double)
(def-com (AngleToReal Utility) ((Angle String) (Unit AcAngleUnits)) Double)
(def-com (AngleToString Utility) ((Angle Double) (Unit AcAngleUnits) (Precision Integer)) String)
(def-com (AppendInnerLoop Hatch) ((Loop Variant)) Void)
(def-com (AppendItems Group) ((Objects VarIdN)) Void)
(def-com (AppendOuterLoop Hatch) ((Loop Object)) Void)
(def-com (AppendVertex 3DPolyline) ((Point VarDouble3)) Void)
(def-com (ArrayPolar All) ((NumberOfObjects Integer) (AngleToFill Double) (CenterPoint VarDouble3)) Variant)
(def-com (ArrayRectangular All) ((NumberOfRows Integer) (NumberOfColumns Integer) (NumberOfLevels Integer) (DistBetweenRows Double) (DistBetweenColumns Double) (DistBetweenLevels Double)) Variant)
(def-com (AttachExternalReference ModelSpace) ((PathName String) (Name String) (InsertionPoint VarDouble3) (XScale Double) (YScale Double) (ZScale Double) (Rotation Double) (bOverlay Boolean) (Password Variant)) ExternalReference)
(def-com (AttachToolbarToFlyout ToolbarItem) ((MenuGroupName String) (ToolbarName String)) Void)
(def-com (AuditInfo Document) ((FixError Boolean)) Void)
(def-com (Bind Block) ((bPrefixName Boolean)) Void)
(def-com (Block SortentsTable) () The)
|#
;;Note Boolean -> acadBoolean to avoid name clashes
(def-com ((acad-boolean Boolean) 3DSolid) ((Operation AcBooleanType) (Obj Object)) Void)
(def-com (check-interference 3DSolid) ((CreateInterferenceSolid Boolean)) 3DSolid)
(def-com (clear SelectionSet) () Void)
#|
(def-com (ClearSubSelection Table) () Void)
(def-com (ClearTableStyleOverrides Table) ((flag Long)) Void)
(def-com (ClipBoundary Raster) ((PointsArray (Variant (Arrayof (Array Double Double))))) Void)
|#
;;Can't use all options(def-com (Close Document) ((SaveChanges Variant) (FileName String)) Void)
(def-com (close Document) ((SaveChanges Boolean)) Void)
#|
(def-com (ConvertToAnonymousBlock BlockRef) () Void)
(def-com (ConvertToStaticBlock BlockRef) ((newBlockName String)) Void)
|#
(def-com (copy All) () Object)

;(CopyFrom Method idh_copyfrom.htm)
(def-com (copy-objects Database) ((Objects VarIdN) #:opt (Owner Document) #;(IDPairs VarIdN)) VarIdN)
#|(def-com (CopyProfile PreferencesProfiles) ((oldProfileName String) (newProfileName String)) Void)
(def-com (CreateCellStyle TableStyle) ((StringCellStyle String)) Void)
(def-com (CreateCellStyleFromStyle TableStyle) ((StringCellStyle String) (StringSourceCellStyle String)) Void)
(def-com (CreateContent Table) ((nRow Long) (nCol Long) (nIndex Long)) Long)
(def-com (CreateEntry FileDependencies) ((Feature String) (FullFileName String) (AffectsGraphics Boolean) (noIncrement Boolean)) Void)
(def-com (CreateJog Section) ((varPt Variant)) Void)
;(CreateTypedArray Method idh_createtypedarray.htm)
|#
(def-com (delete All) () Void)

(provide safe-delete)
(define (safe-delete [obj : Com-Object]) : Boolean
  (with-handlers ((exn:fail? (lambda ([e : exn]) ;;HACK: Typed Racket doesn't provide Exn
                               (if (regexp-match? "Object was erased" (exn-message e))
                                   #f
                                   (raise e)))))
    (delete obj)
    #t))

#|
(def-com (DeleteCellContent Table) ((row Long) (col Long)) Void)
(def-com (DeleteCellStyle TableStyle) ((StringCellStyle String)) Void)
(def-com (DeleteColumns Table) ((col Long) (cols Long)) Void)
(def-com (DeleteConfiguration Viewports) ((Name String)) Void)
(def-com (DeleteContent Table) ((nRow Long) (nCol Long)) Void)
(def-com (DeleteFitPoint Spline) ((Index Integer)) Void)
(def-com (DeleteProfile PreferencesProfiles) ((ProfileName String)) Void)
(def-com (DeleteRows Table) ((row Long) (Rows Long)) Void)
(def-com (Detach Block) () Void)
(def-com (Display PViewport) ((Status Boolean)) Void)
(def-com (DisplayPlotPreview Plot) ((Preview AcPreviewMode)) Void)
(def-com (DistanceToReal Utility) ((Distance String) (Unit AcUnits)) Double)
(def-com (Dock Toolbar) ((Side AcToolbarDockStatus)) Void)
(def-com (ElevateOrder Spline) ((Order Integer)) Void)
(def-com (EnableMergeAll Table) ((nRow Integer) (nCol Integer) (bEnable Boolean)) Void)
|#
(def-com (end-undo-mark Document) () Void)
#|
(def-com (Erase SelectionSet) () Void)
|#
(def-com (eval Application) ((Expression String)) Void)
#|
(def-com (Evaluate Hatch) () Void)
|#
(def-com (explode All) () VarIdN)
#|
;(Export method idh_export.htm)
(def-com (ExportProfile PreferencesProfiles) ((Profile String) (RegFile String)) Void)
(def-com (FieldCode Text) () String)
(def-com (Float Toolbar) ((Top Integer) (Left Integer) (NumberFloatRows Integer)) Void)
(def-com (FormatValue Table) ((row Integer) (col Integer) (nOption AcFormatOption)) String)
(def-com (GenerateLayout Table) () Void)
;(GenerateSectionGeometry method idh_generatesectiongeometry.htm)
(def-com (GenerateUsageData Layers) () Void)
(def-com (GetAcadState Application) () AcadState)
;(GetAlignment method idh_ts_getalignment.htm)
(def-com (GetAlignment2 Table) ((bstrCellStyle String)) AcCellAlignment)
;(GetAllProfileNames method idh_getallprofilenames.htm)
;(GetAngle method idh_getangle.htm)
(def-com (GetAttachmentPoint Table) ((row Long) (col Long)) Void)
(def-com (GetAttributes BlockRef) () Variant)
(def-com (GetAutoScale Table) ((row Long) (col Long)) Boolean)
(def-com (GetAutoScale2 Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Boolean)
(GetBackgroundColor method idh_ts_getbkcolor.htm)
(GetBackgroundColorNone method idh_ts_getbkcolornone.htm)
(def-com (GetBitmaps ToolbarItem) ((SmallIconName String) (LargeIconName String)) Void)
(def-com (GetBlockAttributeValue MLeader) ((attdefID Long) (value String)) Void)
(GetBlockAttributeValue method idh_table_getblockattributevalue.htm)
(def-com (GetBlockAttributeValue2 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (blkId Long_PTR)) String)
(def-com (GetBlockAttributeValue32 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (blkId Long)) String)
(def-com (GetBlockAttributeValue32 MLeader) ((attdefId Long)) String)
(def-com (GetBlockRotation Table) ((row Long) (col Long)) Double)
(def-com (GetBlockScale Table) ((row Long) (col Long)) Double)
(GetBlockTableRecordId method idh_table_getblocktablerecordid.htm)
(def-com (GetBlockTableRecordId2 Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Long_PTR)
(def-com (GetBlockTableRecordId32 Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Long)
|#
(def-com (get-bounding-box Shape) ([min-point (Boxof VarDouble3)] [max-point (Boxof VarDouble3)]) Void)

(provide bounding-box)
(define (bounding-box [shape : Com-Object]) : (List Loc Loc)
  (let ((min-point (box #(0.0 0.0 0.0)))
        (max-point (box #(0.0 0.0 0.0))))
    (get-bounding-box shape min-point max-point)
    (list (vector-double-flonum->loc (cast (unbox min-point) VarDouble3))
          (vector-double-flonum->loc (cast (unbox max-point) VarDouble3)))))

#|
(def-com (GetBreakHeight Table) ((nIndex Integer)) Double)
(def-com (GetBulge LightweightPolyline) ((Index Integer)) Double)
(def-com (GetCanonicalMediaNames Layout) () Variant)
(def-com (GetCellAlignment Table) ((row Long) (col Long)) AcCellAlignment)
(def-com (GetCellBackgroundColor Table) ((row Long) (col Long)) AcCmColor)
(def-com (GetCellBackgroundColorNone Table) ((row Long) (col Long)) Boolean)
(def-com (GetCellClass TableStyle) ((StringCellStyle String)) Integer)
(def-com (GetCellContentColor Table) ((row Long) (col Long)) AcCmColor)
(def-com (GetCellContentColor2 Table) ((nRow Integer) (nCol Integer) (nContent Integer)) AcadAcCmColor)
(GetCellDataType method idh_getcelldatatype.htm)
(GetCellExtents method idh_table_getcellextents.htm)
(def-com (GetCellFormat Table) ((row Integer) (col Integer)) String)
(GetCellGridColor method idh_table_getcellgridcolor.htm)
(GetCellGridLineWeight method idh_table_getcellgridlineweight.htm)
(GetCellGridVisibility method idh_table_getcellgridvisibility.htm)
(def-com (GetCellState Table) ((nRow Integer) (nCol Integer)) AcCellState)
(def-com (GetCellStyle Table) ((nRow Integer) (nCol Integer)) String)
(def-com (GetCellStyleOverrides Table) ((row Long) (col Long)) Void)
(def-com (GetCellTextHeight Table) ((row Long) (col Long)) Double)
(def-com (GetCellTextStyle Table) ((row Long) (col Long)) String)
(def-com (GetCellType Table) ((row Long) (col Long)) AcCellType)
(def-com (GetCellValue Table) ((row Integer) (col Integer)) VARIANT)
(GetColor method idh_ts_getcolor.htm)
(def-com (GetColumnName Table) ((nIndex Integer)) String)
(def-com (GetColumnWidth Table) ((col Long)) Double)
(def-com (GetConstantAttributes BlockRef) () Variant)
(GetContentColor method idh_table_getcontentcolor.htm)
(def-com (GetContentLayout Table) ((nRow Integer) (nCol Integer)) AcCellContentLayout)
(def-com (GetContentType Table) ((nRow Integer) (nCol Integer)) AcCellContentType)
(def-com (GetControlPoint Spline) ((Index Integer)) Variant)
(GetCorner method idh_getcorner.htm)
(def-com (GetCustomByIndex SummaryInfo) ((Index Long) (pKey String) (pValue String)) String)
(def-com (GetCustomByKey SummaryInfo) ((key String) (pValue String)) String)
(def-com (GetCustomData Table) ((nRow Integer) (nCol Integer) (szKey String)) VARIANT)
(def-com (GetCustomScale Layout) ((Numerator Double) (Denominator Double)) Void)
(def-com (GetDataFormat Table) ((nRow Integer) (nCol Integer) (nContent Integer)) String)
(def-com (GetDataLink Table) ((nRow Integer) (nCol Integer)) Long_PTR)
(def-com (GetDataLink32 Table) ((nRow Integer) (nCol Integer) (pacDbObjId Long)) Void)
(GetDataType method idh_getdatatype.htm)
(GetDataType2 method idh_getdatatype2.htm)
(GetDistance method idh_getdistance.htm)
(def-com (GetDynamicBlockProperties BlockRef) () Void)
|#
(def-com ((native-get-entity GetEntity) Utility) ((entity (Boxof Com-Object)) (point (Boxof VarDouble3)) #:opt (str String)) Void)

(provide get-entity)
(define (get-entity [str : String "Select shape"]) : Com-Object
  (define point (box (vector 0.0 0.0 0.0)))
  (define entity (box (utility)))
  ;(com-invoke (utility) "GetEntity" entity point str)
  (native-get-entity entity point str)
  (cast (unbox entity) Com-Object))

#|
(def-com (GetExtensionDictionary All) () Dictionary)
(def-com (GetFieldId Table) ((row Long) (col Long)) Long)
(def-com (GetFieldId2 Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Long_PTR)
(def-com (GetFieldId232 Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Long)
(GetFieldId32 method idh_table_getfieldid32.htm)
(def-com (GetFitPoint Spline) ((Index Integer)) Variant)
(GetFont method idh_getfont.htm)
(def-com (GetFormat TableStyle) ((rowType AcRowType)) Void)
(def-com (GetFormat Table) ((rowType AcRowType)) String)
(def-com (GetFormat2 TableStyle) ((StringCellStyle String) (pbstrFormat String)) Void)
(def-com (GetFormula Table) ((nRow Integer) (nCol Integer) (nContent Integer)) String)
(GetFullDrawOrder method idh_getfulldraworder.htm)
(GetGridColor method idh_ts_getgridcolor.htm)
(def-com (GetGridColor2 Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType)) AcCmColor)
(def-com (GetGridDoubleLineSpacing Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType)) Double)
(def-com (GetGridLineStyle Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType)) AcGridLineStyle)
(def-com (GetGridLinetype Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType)) Long_PTR)
(def-com (GetGridLinetype32 Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType)) Long)
(GetGridLineWeight method idh_ts_getgridlineweight.htm)
(def-com (GetGridLineWeight2 Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType)) ACAD_LWEIGHT)
(def-com (GetGridSpacing Viewport) ((XSpacing Double) (YSpacing Double)) Void)
(GetGridVisibility method idh_ts_getgridvisibility.htm)
(def-com (GetGridVisibility2 Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType)) Boolean)
(def-com (GetHasDataLink Table) ((nRow Integer) (nCol Integer)) Boolean)
(def-com (GetHasFormula Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Boolean)
(def-com (GetInput Utility) () String)
|#
(def-com (get-integer Utility) (#:opt (str String)) Integer)
(def-com (get-interface-object Application) ((ProgID String)) Com-Object)
#|(GetInvisibleEdge method idh_getinvisibleedge.htm)
(def-com (GetIsCellStyleInUse TableStyle) ((pszCellStyle String)) Boolean)
(def-com (GetIsMergeAllEnabled TableStyle) ((StringCellStyle String)) Boolean)
(GetKeyword Method idh_getkeyword.htm)
(def-com (GetLiveSection SectionManager) () Void)
(def-com (GetLocaleMediaName Layout) ((Name String)) String)
(def-com (GetLoopAt Hatch) ((Index Integer) (Loop VarIdN)) Void)
(def-com (GetMargin Table) ((nRow Integer) (nCol Integer) (nMargins AcCellMargin)) Double)
(def-com (GetMinimumColumnWidth Table) ((col Long)) Double)
(def-com (GetMinimumRowHeight Table) ((row Long)) Double)
(GetName method idh_getname.htm)
(def-com (GetObject Dictionary) ((Name String)) Object)
(GetOrientation method idh_getorientation.htm)
(def-com (GetOverride Table) ((nRow Integer) (nCol Integer) (nContent Integer)) AcCellProperty)
(GetPaperMargins method idh_getpapermargins.htm)
(def-com (GetPaperSize Layout) ((Width Double) (Height Double)) Void)
(def-com (GetPlotDeviceNames Layout) () Variant)
(def-com (GetPlotStyleTableNames Layout) () Variant)
|#
(def-com (get-point Utility) ((point VarDouble3) #:opt (str String)) VarDouble3)
#|
(def-com (GetProjectFilePath PreferencesFiles) ((ProjectName String)) String)
|#
(def-com (get-real Utility) (#:opt (str String)) Real)
#|
(GetRelativeDrawOrder method idh_getrelativedraworder.htm)
(GetRemoteFile method idh_getremotefile.htm)
(def-com (GetRotation Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Double)
(def-com (GetRotation TableStyle) ((StringCellStyle String)) Double)
(def-com (GetRowHeight Table) ((row Long)) Double)
(def-com (GetRowType Table) ((row Long)) AcRowType)
(def-com (GetScale Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Double)
(def-com (GetSectionTypeSettings SectionSettings) ((secType AcSectionType)) Void)
(def-com (GetSettings Section) ((pUnk IUnknown)) Void)
(def-com (GetSnapSpacing Viewport) ((XSpacing Double) (YSpacing Double)) Void)
(GetString method idh_getstring.htm)
(GetSubEntity method idh_getsubentity.htm)
(def-com (GetSubSelection Table) ((rowMin Long) (rowMax Long) (colMin Long) (colMax Long)) Void)
(def-com (GetText Table) ((row Long) (col Long)) String)
|#
;;;Is this correct?(def-com (get-text-height Text) () Real)
#|
(def-com (GetTextHeight2 Table) ((nRow Integer) (nCol Integer) (nContent Integer)) Double)
(def-com (GetTextRotation Table) ((row Long) (col Long)) AcRotationAngle)
(def-com (GetTextString Table) ((nRow Integer) (nCol Integer) (nContent Integer)) String)
(def-com (GetTextStyle Table) ((rowTypes Long)) String)
(def-com (GetTextStyle2 Table) ((nRow Integer) (nCol Integer) (nContent Integer)) String)
(def-com (GetUCSMatrix UCS) () Variant)
(def-com (GetUniqueCellStyleName TableStyle) ((pszBaseName String)) String)
(def-com (GetUniqueSectionName SectionManager) ((BaseName String)) Void)
(def-com (GetValue Table) ((nRow Integer) (nCol Integer) (nContent Integer)) VARIANT)
|#
(def-com (get-variable Document) ((Name String)) Any)
#|
(def-com (GetWeight Spline) ((Index Integer)) Double)
(def-com (GetWidth LightweightPolyline) ((Index Integer) (StartWidth Double) (EndWidth Double)) Void)
(GetWindowToPlot method idh_getwindowtoplot.htm)
|#
(def-com (GetXdata All) ((AppName String) (XDataType (Boxof Any)) (XDataValue (Boxof Any))) Void)

(provide get-x-data)
(define (get-x-data [obj : Com-Object] [app-name : String]) : (Values Any Any)
  (let ((b0 (box (cast #f Any)))
        (b1 (box (cast #f Any))))
    ;(com-invoke obj "GetXData" app-name b0 b1)
    (GetXdata obj app-name b0 b1)
    (values (unbox b0) (unbox b1))))

#|
(GetXRecordData method idh_getxrecorddata.htm)
(def-com (HandleToObject Document) ((Handle String)) Object)
|#
(def-com (highlight All) ((on? Boolean)) Void)
#|
(HitTest method idh_hittest.htm)
(HitTest method idh_table_hittest.htm)
(Import method idh_import.htm)
(def-com (ImportProfile PreferencesProfiles) ((Profile String) (RegFile String) (IncludePathInfo Boolean)) Void)
(def-com (IndexOf FileDependencies) ((Feature String) (FullFileName String)) Long)
(InitializeUserInput method idh_initializeuserinput.htm)
(def-com (InsertBlock ModelSpace) ((InsertionPoint VarDouble3) (Name String) (Xscale Double) (Yscale Double) (Zscale Double) (Rotation Double) (Password Variant)) BlockRef)
(def-com (InsertColumns Table) ((col Long) (Width Double) (cols Long)) Void)
(def-com (InsertColumnsAndInherit Table) ((col Integer) (nInheritFrom Integer) (nNumCols Integer)) Void)
(def-com (InsertInMenuBar PopupMenu) ((Index Variant)) Void)
(def-com (InsertLoopAt Hatch) ((Index Integer) (LoopType AcLoopType) (Loop Variant)) Void)
(InsertMenuInMenuBar method idh_insertmenuinmenubar.htm)
(def-com (InsertRows Table) ((row Long) (Height Double) (Rows Long)) Void)
(def-com (InsertRowsAndInherit Table) ((nIndex Integer) (nInheritFrom Integer) (nNumRows Integer)) Void)
(IntersectWith method idh_intersectwith.htm)
(def-com (IsContentEditable Table) ((nRow Integer) (nCol Integer)) Boolean)
(def-com (IsEmpty Table) ((nRow Long) (nCol Long)) Boolean)
(def-com (IsFormatEditable Table) ((nRow Integer) (nCol Integer)) Boolean)
(def-com (IsMergeAllEnabled Table) ((nRow Integer) (nCol Integer)) Boolean)
(def-com (IsMergedCell Table) ((row Long) (col Long) (minRow Long) (maxRow Long) (minCol Long) (maxCol Long)) Boolean)
(def-com (IsRemoteFile Utility) ((LocalFile String) (URL String)) Boolean)
(def-com (IsURL Utility) ((LocalFile String)) Boolean)
|#
(def-com (item All) ((Index Any)) Object)
#|
(LaunchBrowserDialog method idh_launchbrowserdialog.htm)
(def-com (LayerPropertyOverrides PViewport) () Boolean)
(def-com (ListARX Application) () Variant)
(Load method idh_load.htm)
|#
(def-com ((load-arx LoadARX) Application) ((Name String)) Void)
#|
(def-com (LoadDVB Application) ((Name String)) Void)
(def-com (LoadShapeFile Document) ((FullName String)) Void)
(def-com (MergeCells Table) ((minRow Long) (maxRow Long) (minCol Long) (maxCol Long)) Void)
(def-com (Mirror All) ((Point1 VarDouble3) (Point2 VarDouble3)) Com-Object)
|#
(def-com ((mirror3d Mirror3d) All) ((Point1 VarDouble3) (Point2 VarDouble3) (Point3 VarDouble3)) Com-Object)
(def-com (move All) ((Point1 VarDouble3) (Point2 VarDouble3)) Void)
#|
(MoveAbove method idh_moveabove.htm)
(MoveBelow method idh_movebelow.htm)
(def-com (MoveContent Table) ((nRow Long) (nCol Long) (nFromIndex Long) (nToindex Long)) Void)
(MoveToBottom method idh_movetobottom.htm)
(MoveToTop method idh_movetotop.htm)
(def-com (New Document) ((TemplateFileName String)) Document)
(def-com (NumCustomInfo SummaryInfo) () Long)
(def-com (ObjectIDToObject Document) ((ID Long_ptr)) Object)
(def-com (ObjectIDToObject32 Document) ((ID Long)) Object)
(def-com (Offset Arc) ((Distance Double)) Variant)
|#
(def-com (open All) ((name String)) Void)
#|
(def-com (OverwritePropChanged MLeaderStyle) () Boolean)
(def-com (PlotToDevice Plot) ((plotConfig String)) Boolean)
(def-com (PlotToFile Plot) ((plotFile String) (plotConfig String)) Boolean)
(def-com (PolarPoint Utility) ((Point VarDouble3) (Angle Double) (Distance Double)) Variant)
(def-com (Prompt Utility) ((Message String)) Void)
(def-com (PurgeAll Document) () Void)
(PurgeFitData method idh_purgefitdata.htm)
(def-com (PutRemoteFile Utility) ((URL String) (LocalFile String)) Void)
(def-com (Quit Application) () Void)
(def-com (RealToString Utility) ((Value Double) (Unit AcUnits) (Precision Integer)) String)
(RecomputeTableBlock method idh_table_recomputetableblock.htm)
(def-com (RefreshPlotDeviceInfo Layout) () Void)
|#
(def-com (regen Document) ((WhichViewports AcRegenType)) Void)
#|
(def-com (Reload Block) () Void)
(def-com (Remove Dictionary) ((Name String)) Void)
(def-com (RemoveAllOverrides Table) ((nRow Integer) (nCol Integer)) Void)
(def-com (RemoveCustomByIndex SummaryInfo) ((Index Long)) Void)
(def-com (RemoveCustomByKey SummaryInfo) ((key String)) Void)
(def-com (RemoveDataLink Table) ((nRow Integer) (nCol Integer)) Void)
(RemoveEntry method idh_removeentry.htm)
(def-com (RemoveFromMenuBar PopupMenu) () Void)
(def-com (RemoveItems Group) ((Objects Variant)) Void)
(def-com (RemoveMenuFromMenuBar PopupMenus) ((Index Variant)) Void)
(def-com (RemoveVertex Section) ((nIndex Integer)) Void)
(def-com (Rename Dictionary) ((OldName String) (NewName String)) Void)
(def-com (RenameCellStyle TableStyle) ((StringOldName String) (StringNewName String)) Void)
(def-com (RenameProfile PreferencesProfiles) ((origProfileName String) (newProfileName String)) Void)
(def-com (Replace Dictionary) ((Name String) (NewObject Object)) Void)
(def-com (ReselectSubRegion Table) () Void)
(def-com (ResetBlock BlockRef) () Void)
(def-com (ResetCellValue Table) ((row Integer) (col Integer)) Void)
(def-com (ResetProfile PreferencesProfiles) ((Profile String)) Void)
(def-com (Restore LayerStateManager) ((Name String)) Void)
(def-com (Reverse Spline) () Void)
|#
(def-com (rotate All) ((BasePoint VarDouble3) (RotationAngle Double)) Void)
(def-com ((rotate3d Rotate3d) All) ((Point1 VarDouble3) (Point2 VarDouble3) (RotationAngle Double)) Void)
#|
(RunMacro method idh_runmacro.htm)
(Save method idh_save.htm)
(SaveAs method idh_saveas.htm)
|#
(def-com (scale-entity All) ((BasePoint VarDouble3) (ScaleFactor Double)) Void)
(def-com (section-solid 3DSolid) ((Point1 VarDouble3) (Point2 VarDouble3) (Point3 VarDouble3)) Region)
#|
(def-com (Select SelectionSet) ((Mode AcSelect) (Point1 VarDouble3N) (Point2 VarDouble3) (FilterType Integer) (FilterData Variant)) Void)
(Select method idh_table_select.htm)
(def-com (SelectAtPoint SelectionSet) ((Point VarDouble3) (FilterType Integer) (FilterData Variant)) Void)
(def-com (SelectByPolygon SelectionSet) ((Mode AcSelect) (PointsList VarDouble3N) (FilterType Integer) (FilterData Variant)) Void)
(def-com (SelectOnScreen SelectionSet) ((FilterType Integer) (FilterData Variant)) Void)
(SelectSubRegion method idh_table_selectsubregion.htm)
|#

(def-com (send-command Document) ((Command String)) Void)

#|
(def-com (SendModelessOperationEnded Utility) ((Context String)) Void)
(def-com (SendModelessOperationStart Utility) ((Context String)) Void)
(SetAlignment method idh_ts_setalignment.htm)
(def-com (SetAlignment2 Table) ((bstrCellStyle String) (cellAlignment AcCellAlignment)) Void)
(SetAutoScale method idh_table_setautoscale.htm)
(def-com (SetAutoScale2 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (bAutoFit VARIANT_BOOL)) Void)
(SetBackgroundColor method idh_ts_setbkcolor.htm)
(SetBackgroundColorNone method idh_ts_setbkcolornone.htm)
(def-com (SetBitmaps ToolbarItem) ((SmallIconName String) (LargeIconName String)) Void)
(def-com (SetBlockAttributeValue MLeader) ((attdefID Long) (value String)) Void)
(SetBlockAttributeValue method idh_table_setblockattributevalue.htm)
(def-com (SetBlockAttributeValue2 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (blkId Long_PTR) (value String)) Void)
(def-com (SetBlockAttributeValue32 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (blkId Long) (value String)) Void)
(def-com (SetBlockAttributeValue32 MLeader) ((attdefID Long) (value String)) Void)
(def-com (SetBlockRotation Table) ((row Long) (col Long) (blkRotation Double)) Void)
(def-com (SetBlockScale Table) ((row Long) (col Long) (blkScale Double)) Void)
(SetBlockTableRecordId method idh_table_setblocktablerecordid.htm)
(def-com (SetBlockTableRecordId2 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (blkId Long_PTR) (autoFit VARIANT_BOOL)) Void)
(def-com (SetBlockTableRecordId32 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (blkId Long) (autoFit VARIANT_BOOL)) Void)
(def-com (SetBreakHeight Table) ((nIndex Integer) (height Double)) Void)
(def-com (SetBulge LightweightPolyline) ((Index Integer) (Value Double)) Void)
(SetCellAlignment method idh_table_setcellalignment.htm)
(SetCellBackgroundColor method idh_table_setcellbkcolor.htm)
(SetCellBackgroundColorNone method idh_table_setcellbkcolornone.htm)
(SetCellClass method idh_ts_setcellclass.htm)
(SetCellContentColor method idh_table_setcellcontentcolor.htm)
(SetCellContentColor2 method idh_setcellcontentcolor2.htm)
(def-com (SetCellDataType Table) ((row Integer) (col Integer) (pDataType AcValueDataType) (pUnitType AcValueUnitType)) Void)
(def-com (SetCellFormat Table) ((row Integer) (col Integer) (pFormat String)) Void)
(SetCellGridColor method idh_table_setcellgridcolor.htm)
(SetCellGridLineWeight method idh_table_setcellgridlineweight.htm)
(SetCellGridVisibility method idh_table_setcellgridvisibility.htm)
(def-com (SetCellState Table) ((nRow Integer) (nCol Integer) (nLock AcCellState)) Void)
(def-com (SetCellStyle Table) ((nRow Integer) (nCol Integer) (szCellStyle String)) Void)
(def-com (SetCellTextHeight Table) ((row Long) (col Long) (TextHeight Double)) Void)
(def-com (SetCellTextStyle Table) ((row Long) (col Long) (bstrName String)) Void)
(SetCellType method idh_table_setcelltype.htm)
(SetCellValue method idh_setcellvalue.htm)
(SetColor method idh_ts_setcolor.htm)
(def-com (SetColorBookColor AcCmColor) ((ColorName String) (ColorBook String)) Void)
(def-com (SetColumnName Table) ((nIndex Integer) (name String)) Void)
(def-com (SetColumnWidth Table) ((col Long) (Width Double)) Void)
(SetContentColor method idh_table_setcontentcolor.htm)
(def-com (SetContentLayout Table) ((nRow Integer) (nCol Integer) (nLayout AcCellContentLayout)) Void)
(SetControlPoint method idh_setcontrolpoint.htm)
(def-com (SetCustomByIndex SummaryInfo) ((Index Long) (key String) (Value String)) Void)
(def-com (SetCustomByKey SummaryInfo) ((key String) (pValue String)) Void)
(def-com (SetCustomData Table) ((nRow Integer) (nCol Integer) (szKey String) (data VARIANT)) Void)
(def-com (SetCustomScale Layout) ((Numerator Double) (Denominator Double)) Void)
(SetDatabase method idh_setdatabase.htm)
(def-com (SetDataFormat Table) ((nRow Integer) (nCol Integer) (nContent Integer) (szFormat String)) Void)
(def-com (SetDataLink Table) ((nRow Integer) (nCol Integer) (idDataLink Long_PTR) (bUpdate VARIANT_BOOL)) Void)
(def-com (SetDataLink32 Table) ((nRow Integer) (nCol Integer) (idDataLink Long) (bUpdate VARIANT_BOOL)) Void)
(def-com (SetDataType TableStyle) ((rowTypes Integer) (nDataType AcValueDataType) (nUnitType AcValueUnitType)) Void)
(def-com (SetDataType2 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (dataType AcValueDataType) (unitType AcValueUnitType)) Void)
(SetFieldId method idh_table_setfieldid.htm)
(def-com (SetFieldId2 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (acDbObjectId Long_PTR) (nflag AcCellOption)) Void)
(def-com (SetFieldId232 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (acDbObjectId Long) (nflag AcCellOption)) Void)
(SetFieldId32 method idh_table_setfieldid32.htm)
(SetFitPoint method idh_setfitpoint.htm)
(def-com (SetFont TextStyle) ((Typeface String) (Bold Boolean) (Italic Boolean) (CharSet Long) (PitchAndFamily Long)) Void)
(def-com (SetFormat TableStyle) ((rowTypes Long) (val String)) Void)
(def-com (SetFormat Table) ((rowType AcRowType) (pFormat String)) Void)
(def-com (SetFormat2 TableStyle) ((StringCellStyle String) (pVal String)) Void)
(def-com (SetFormula Table) ((nRow Integer) (nCol Integer) (nContent Integer) (pszFormula String)) Void)
(SetGridColor method idh_ts_setgridcolor.htm)
(SetGridColor2 method idh_setgridcolor2.htm)
(def-com (SetGridDoubleLineSpacing Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType) (fSpacing Double)) Void)
(def-com (SetGridLineStyle Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType) (nLineStyle AcGridLineStyle)) Void)
(def-com (SetGridLinetype Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType) (idLinetype Long_PTR)) Void)
(def-com (SetGridLinetype32 Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType) (idLinetype Long)) Void)
(SetGridLineWeight method idh_ts_setgridlineweight.htm)
(def-com (SetGridLineWeight2 Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType) (lineWeight ACAD_LWEIGHT)) Void)
(def-com (SetGridSpacing Viewport) ((XSpacing Double) (YSpacing Double)) Void)
(SetGridVisibility method idh_ts_setgridvisibility.htm)
(def-com (SetGridVisibility2 Table) ((nRow Integer) (nCol Integer) (nGridLineType AcGridLineType) (bVisible VARIANT_BOOL)) Void)
(def-com (SetInvisibleEdge 3DFace) ((Index Integer) (State Boolean)) Void)
(def-com (SetLayoutsToPlot Plot) ((layoutList Variant)) Void)
(def-com (SetMargin Table) ((nRow Integer) (nCol Integer) (nMargins AcCellMargin) (pValue Double)) Void)
(def-com (SetNames AcCmColor) ((ColorName String) (BookName String)) Void)
(def-com (SetOverride Table) ((nRow Integer) (nCol Integer) (nContent Integer) (nProp AcCellProperty)) Void)
(def-com (SetPattern Hatch) ((PatternType AcPatternType) (PatternName String)) Void)
(def-com (SetProjectFilePath PreferencesFiles) ((ProjectName String) (ProjectFilePath String)) Void)
(SetRelativeDrawOrder method idh_setrelativedraworder.htm)
|#
(def-com ((set-rgb! SetRGB) AcCmColor) ((Red Long) (Green Long) (Blue Long)) Void)
#|
(def-com (SetRotation Table) ((nRow Integer) (nCol Integer) (nContent Integer) (value Double)) Void)
(def-com (SetRotation TableStyle) ((StringCellStyle String) (rotation Double)) Void)
(def-com (SetRowHeight Table) ((row Long) (Height Double)) Void)
(def-com (SetScale Table) ((nRow Integer) (nCol Integer) (nContent Integer) (scale Double)) Void)
(def-com (SetSnapSpacing Viewport) ((XSpacing Double) (YSpacing Double)) Void)
(def-com (SetSubSelection Table) ((rowMin Long) (rowMax Long) (colMin Long) (colMax Long)) Void)
(def-com (SetText Table) ((row Long) (col Long) (pStr String)) Void)
(def-com (SetTextHeight Table) ((rowTypes Long) (TextHeight Double)) Void)
(def-com (SetTextHeight2 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (height Double)) Void)
(SetTextRotation method idh_table_settextrotation.htm)
(def-com (SetTextString Table) ((nRow Integer) (nCol Integer) (nContent Integer) (text String)) Void)
(def-com (SetTextStyle Table) ((rowTypes Long) (bstrName String)) Void)
(def-com (SetTextStyle2 Table) ((nRow Integer) (nCol Integer) (nContent Integer) (StringStyleName String)) Void)
(def-com (SetToolTip Table) ((nRow Integer) (nCol Integer) (tip String)) Void)
(def-com (SetValue Table) ((nRow Integer) (nCol Integer) (nContent Integer) (acValue VARIANT)) Void)
(def-com (SetValueFromText Table) ((nRow Integer) (nCol Integer) (nContent Integer) (szText String) (nOption AcParseOption)) Void)
|#
(def-com (set-variable Document) ((Name String) (Value Any)) Void)
#|
(def-com (SetView Viewport) ((View View)) Void)
(def-com (SetWeight Spline) ((Index Integer) (Weight Double)) Void)
(def-com (SetWidth LightweightPolyline) ((SegmentIndex Integer) (StartWidth Double) (EndWidth Double)) Void)
(SetWindowToPlot method idh_setwindowtoplot.htm)
(SetXdata method idh_setxdata.htm)
(SetXRecordData method idh_setxrecorddata.htm)
(def-com (SliceSolid 3DSolid) ((Point1 VarDouble3) (Point2 VarDouble3) (Point3 VarDouble3) (Negative Boolean)) 3DSolid)
(def-com (Split Viewport) ((NumWins AcViewportSplitType)) Void)
(def-com (StartBatchMode Plot) ((entryCount Long)) Void)]
|#
(def-com (start-undo-mark Document) () Void)
#|(SwapOrder method idh_swapobjects.htm)
(def-com (SyncModelView PViewport) () Void)
|#
(def-com (transform-by All) ((TransformationMatrix VarDouble4x4)) Void)
#|
(def-com (TranslateCoordinates Utility) ((OriginalPoint VarDouble3) (From AcCoordinateSystem) (To AcCoordinateSystem) (Disp Integer) (OCSNormal VarDouble3)) Variant)
(def-com (Unload Block) () Void)
(def-com (UnloadARX Application) ((Name String)) Void)
(def-com (UnloadDVB Application) ((Name String)) Void)
(def-com (UnmergeCells Table) ((minRow Long) (maxRow Long) (minCol Long) (maxCol Long)) Void)
|#
(def-com (update All) () Void)
#|
(def-com (UpdateAllDataLinks Table) ((nDir AcDataLinkUpdateDirection) (nOption AcDataLinkUpdateDirectOption)) Void)
(def-com (UpdateDataLink Table) ((nRow Integer) (nCol Integer) (nDir AcDataLinkUpdateDirection) (nOption AcDataLinkUpdateDirectOption)) Void)
(def-com (UpdateEntry FileDependencies) ((Index Long)) Void)
(def-com (UpdateMTextAttribute Attribute) () Void)
(def-com (WBlock Document) ((FileName String) (SelectionSet SelectionSet)) Void)
|#
(def-com (zoom-all Application) () Void)
(def-com (zoom-center Application) ((Center VarDouble3) (Magnify Double)) Void)
(def-com (zoom-extents Application) () Void)
(def-com (zoom-pick-window Application) () Void)
(def-com (zoom-previous Application) () Void)
;;(ZoomScaled method idh_zoomscaled.htm)
(def-com (zoom-window Application) ((LowerLeft VarDouble3) (UpperRight VarDouble3)) Void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide regen-active-viewport)
(define (regen-active-viewport)
  (regen acActiveViewport))


(provide Double) ;???

;;array of shorts
(define (Integers->VarShortN [v : Integers]) : VarIntegerN ;;HACK: improve this when the new ffi/com is out
  #;
  (for/vector ([e (in-list v)])
    (type-describe e 'short-int))
  (type-describe
   (list->vector v)
   `(variant (array ,(length v) short-int))))



(define (Com-Objects->VarIdN [c : Com-Objects]) : VarIdN
  (display "Converting from") (displayln c)
  (list->vector c))

(define (VarIdN->Com-Objects [c : VarIdN]) : Com-Objects
  (display "Converting to") (displayln c)
  (vector->list c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;To access AutoLISP defined functions

(define autolisp-function-table : (HashTable Symbol Com-Object)
  (make-hasheq))

(define (get-autolisp-function [name : Symbol]) : Com-Object
  (hash-ref! autolisp-function-table
             name
             (lambda () : Com-Object
               (cast
                (com-get-property*
                 (autolisp-functions)
                 "Item"
                 (string-upcase (symbol->string name)))
                Com-Object))))

(define-syntax def-autolisp
  (syntax-rules ()
    [(def-autolisp ((name al-name) param ...) type)
     (def-com-method (name funcall) (get-autolisp-function 'al-name) (param ...) type)]
    [(def-autolisp (name param ...) type)
     (def-autolisp ((name name) param ...) type)]))

(def-autolisp ((al-handent handent) [id String]) Com-Object)
(def-autolisp ((al-eval eval) [e Com-Object]) Any)
(def-autolisp ((al-read read) [s String]) Com-Object)


(define-type AutoLISP-Type (U Real String))

;;Defines an AutoLISP function
(provide defun)
(define-syntax (defun stx)
  (syntax-case stx ()
    ((def name params body ...)
     (with-syntax ([name-str (string-upcase (symbol->string (syntax-e #'name)))]
                   [real-params
                    (takef (syntax->list #'params)
                           (lambda (stx)
                             (not (eq? (syntax->datum stx) '/))))]
                   [str (format "~S"
                                (syntax->datum
                                 #'(defun name params
                                     (vl-princ-to-string
                                      (progn
                                       body ...)))))])
       (syntax/loc stx
         (define name : (-> AutoLISP-Type * Any)
           (lambda args
             (al-eval (al-read str))
             (let ((ref (cast (com-get-property* (autolisp-functions) "Item" name-str) Com-Object)))
               (define new-func : (-> AutoLISP-Type * Any)
                 (lambda args
                   (with-input-from-string
                    (cast (apply com-invoke ref "funcall" args) String)
                    read)))
               (set! name new-func)
               (apply name args)))))))))

#|
;These functions are not safe as the Lisp object might have been moved by the Lisp GC
(def-autolisp ((al-nth nth) [i Integer] [l Com-Object]) Any)
(def-autolisp ((al-length length) [l Com-Object]) Integer)
(define (loc<-al-com [c : Com-Object]) : Loc
  (xyz (cast (al-nth 0 c) Real)
       (cast (al-nth 1 c) Real)
       (cast (al-nth 2 c) Real)
       world-cs))

(define (vec<-al-com [c : Com-Object]) : Vec
  (vxyz (cast (al-nth 0 c) Real)
        (cast (al-nth 1 c) Real)
        (cast (al-nth 2 c) Real)
        world-cs))

(define (list<-al-com [c : Com-Object]) : (Listof Any)
  (let ((n (al-length c)))
    (for/list : (Listof Any) ((i (in-range n)))
      (al-nth i c))))

;(def-autolisp (entget [c Com-Object]) Com-Object)
;(def-autolisp (assoc [k Com-Object] [l Com-Object]) Com-Object)
|#
(def-autolisp (vlax-curve-getStartParam [c Com-Object]) Real)
(def-autolisp (vlax-curve-getEndParam [c Com-Object]) Real)
(def-autolisp (vlax-curve-getDistAtParam [c Com-Object] [t Real]) Real)
(def-autolisp (vlax-curve-getParamAtDist [c Com-Object] [length Real]) Real)
(def-autolisp (vlax-curve-getClosestPointTo [c Com-Object] [p (List Real Real Real)]) Real)
#|
;These functions are not safe as the Lisp object might have been moved by the Lisp GC
(def-autolisp (vlax-curve-getStartPoint [c Com-Object]) Com-Object)
(def-autolisp (vlax-curve-getEndPoint [c Com-Object]) Com-Object)
(def-autolisp (vlax-curve-getFirstDeriv [c Com-Object] [t Real]) Com-Object)
(def-autolisp (vlax-curve-getSecondDeriv [c Com-Object] [t Real]) Com-Object)
(def-autolisp (vlax-curve-getPointAtParam [c Com-Object] [t Real]) Com-Object)
|#
(defun safe-vlax-curve-getStartPoint (c) (vlax-curve-getStartPoint (handent c)))
(defun safe-vlax-curve-getEndPoint (c) (vlax-curve-getEndPoint (handent c)))
(defun safe-vlax-curve-getFirstDeriv (c r) (vlax-curve-getFirstDeriv (handent c) r))
(defun safe-vlax-curve-getSecondDeriv (c r) (vlax-curve-getSecondDeriv (handent c) r))
(defun safe-vlax-curve-getPointAtParam (c r) (vlax-curve-getPointAtParam (handent c) r))
(defun safe-vlax-curve-getClosestPointTo (c x y z) (vlax-curve-getClosestPointTo (handent c) (list x y z)))

(provide curve-start-point
         curve-end-point
         curve-start-param
         curve-end-param
         curve-closest-point
         curve-length
         curve-tangent-at
         curve-normal-at
         curve-point-at
         curve-frame-at
         curve-frame-at-length)

(define (loc<-list [l : Any]) : Loc
  (let ((l (cast l (List Real Real Real))))
    (xyz (list-ref l 0)
         (list-ref l 1)
         (list-ref l 2)
         world-cs)))

(define (vec<-list [l : Any]) : Vec
  (let ((l (cast l (List Real Real Real))))
    (vxyz (list-ref l 0)
          (list-ref l 1)
          (list-ref l 2)
          world-cs)))

(define (curve-start-point [c : Com-Object]) : Loc
  (loc<-list (safe-vlax-curve-getStartPoint (handle c))))

(define (curve-end-point [c : Com-Object]) : Loc
  (loc<-list (safe-vlax-curve-getEndPoint (handle c))))

(define (curve-start-param [c : Com-Object]) : Real
  (vlax-curve-getStartParam (al-handent (handle c))))

(define (curve-end-param [c : Com-Object]) : Real
  (vlax-curve-getEndParam (al-handent (handle c))))

(define (curve-length [c : Com-Object]) : Real
  (vlax-curve-getDistAtParam (al-handent (handle c))
                             (vlax-curve-getEndParam (al-handent (handle c)))))

(define (curve-frame-at-length [c : Com-Object] [d : Real]) : Loc
  (curve-frame-at c (vlax-curve-getParamAtDist (al-handent (handle c)) d)))

(define (curve-tangent-at [c : Com-Object] [t : Real]) : Vec
  (vec<-list (safe-vlax-curve-getFirstDeriv (handle c) t)))

(define (curve-normal-at [c : Com-Object] [t : Real]) : Vec
  (vec<-list (safe-vlax-curve-getSecondDeriv (handle c) t)))

(define (curve-point-at [c : Com-Object] [t : Real]) : Loc
  (loc<-list (safe-vlax-curve-getPointAtParam (handle c) t)))

(define (curve-closest-point [c : Com-Object] [p : Loc]) : Loc
  (let ((p (loc-in-world p)))
    (loc<-list (safe-vlax-curve-getClosestPointTo (handle c) (cx p) (cy p) (cz p)))))

(define (xvec-for [t : Vec] [n : Vec])
  (define (~zero? [x : Real]) : Boolean
    (< (abs x) 1e-14))
  (if (~zero? (vlength n)) ;unusable normal
      (vpol 1 (+ (sph-phi t) pi/2))
      n))

(define (curve-frame-at [c : Com-Object] [t : Real]) : Loc
  (let* ((o (curve-point-at c t))
         (tv (curve-tangent-at c t))
         (nv (xvec-for tv (curve-normal-at c t))))
    (loc-from-o-vx-vy
     o
     nv
     (v*v nv (v*r tv -1)))))


;;Helpers
(provide add-closed-line)
(define (add-closed-line [cs : Locs]) : Com-Object
  (let ((com (add-3d-poly (append cs (list (car cs))))))
    (closed com #t)
    com))

(provide add-region-from-curves)
(define (add-region-from-curves [curves : Com-Objects]) : Com-Object
  (begin0
    (car (add-region curves))
    (for ((c (in-list curves))) (delete c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (last-item [obj : Com-Object]) : Com-Object
  (item obj (- (count obj) 1)))

(define (created-items [obj : Com-Object] [prev : Integer]) : (Listof Com-Object)
  (let ((curr (count obj)))
    (cond ((< curr prev)
           (error 'created-items "Items were eliminated"))
          ((= curr prev)
           (error 'created-items "No new items"))
          (else
           (for/list : (Listof Com-Object)
             ((i (in-range prev curr 1)))
             (item obj i))))))

(define (created-item [obj : Com-Object] [prev : Integer]) : Com-Object
  (let ((curr (count obj)))
    (cond ((< curr prev)
           (error 'created-item "Items were eliminated"))
          ((= curr prev)
           (error 'created-item "No new item"))
          ((> curr (+ prev 1))
           (error 'created-item "More than one new items"))
          (else
           (item obj prev)))))


(provide all-objects)
(define (all-objects [modelspace : Com-Object (active-modelspace)])
  (for/list : (Listof Com-Object)
    ((i (in-range 0 (count modelspace) 1)))
    (item modelspace i)))

;; Commands

(define-syntax (def-cmd stx)
  (syntax-case stx ()
    ((_ (name in ...) body ...)
     (syntax/loc stx
       (begin
         (provide name)
         (define (name in ...) : Void
           (send-command (string-append body ...))))))))

(define-syntax (def-new-shape-cmd stx)
  (syntax-case stx ()
    ((_ (name in ...) body ...)
     (syntax/loc stx
       (begin
         (provide name)
         (define (name in ...) : Com-Object
           (let ((prev-count (count (active-modelspace))))
             (send-command (string-append body ...))
             (created-item (active-modelspace) prev-count))))))))

(define-syntax (def-new-shapes-cmd stx)
  (syntax-case stx ()
    ((_ (name in ...) body ...)
     (syntax/loc stx
       (begin
         (provide name)
         (define (name in ...) : (Listof Com-Object)
           (let ((prev-count (count (active-modelspace))))
             (send-command (string-append body ...))
             (created-items (active-modelspace) prev-count))))))))

;;The most fundamental one:

(def-new-shape-cmd (new-shape-from [str : String])
  str)

(def-new-shapes-cmd (new-shapes-from [str : String])
  str)

(def-cmd (erase-all) "_.erase _all" vbCr)

(define (loc-string [c : Loc]) : String
  (let ((c (loc-in-world c)))
    (format "~A,~A,~A" (cx c) (cy c) (cz c))))

(define (vec-string [v : Vec]) : String
  (let ((v (vec-in-world v)))
    (format "~A,~A,~A" (cx v) (cy v) (cz v))))

(def-cmd (dview-zoom-command [center : Loc] [target : Loc] [lens : Real] [distance : Real])
  (format "_.dview  _z ~A _po ~A ~A _d ~A" lens (loc-string target) (loc-string center) distance)
  vbCr)

(def-new-shape-cmd (add-cone-frustum [c : Loc] [base-radius : Real] [top-radius : Real] [height : Real])
  "_.cone "
  (loc-string c)
  (format " ~A _T ~A ~A " base-radius top-radius height))

(def-cmd (reset-ucs) "_.ucs _W ")
(def-cmd (view-top) "_.-view _top ")
(def-cmd (view-wireframe) "_.vscurrent _2dwireframe ")
(def-cmd (view-conceptual) "_.vscurrent _conceptual ")

(def-cmd (vl-load-com) "(vl-load-com)\n")

;(def-cmd zoom-extents () "_.zoom _e ")

;;To refer to com objects in commands

(define (handent-string [handle : String]) : String
  (string-append "(handent \"" handle "\")"))

(define (handent [object : Com-Object]) : String
  (handent-string (handle object)))

(define (handents [objects : (Listof Com-Object)]) : String
  (string-join (map handent objects) " "))

(def-cmd (union-command [objects : (Listof Com-Object)])
  "_.union " (handents objects) vbCr)

(def-cmd (subtraction-command [objects0 : (Listof Com-Object)] [objects1 : (Listof Com-Object)])
  (format "_.subtract ~A~A~A~A"
          (handents objects0)
          vbCr;" "
          (handents objects1)
          vbCr))

(def-cmd (intersection-command [objects0 : (Listof Com-Object)] [objects1 : (Listof Com-Object)])
   (format "_.intersect ~A~A~A~A"
           (handents objects0)
           vbCr;" "
           (handents objects1)
           vbCr))

(def-cmd (join-command [objects : (Listof Com-Object)])
   "._join " (handents objects) vbCr)

(define-syntax (def-autocad-variable stx)
  (syntax-case stx ()
    [(def (name var) type)
     (with-syntax ([upgraded-type (upgrade-type #'type)])
       (syntax/loc stx
         (begin
           (provide name)
           (define name
             (case-lambda
               (()
                (convert-to-type type upgraded-type (cast (get-variable var) type)))
               (([arg : upgraded-type])
                (set-variable var (convert-to-type upgraded-type type arg))))))))]
    [(def name type)
     (with-syntax ([str (string-upcase (symbol->string (syntax-e #'name)))])
       (syntax/loc stx
         (def (name str) type)))]
    [(def name)
     (syntax/loc stx
       (def name Any))]))

(def-autocad-variable delobj)
(def-autocad-variable facetersmoothlev Integer)
(def-autocad-variable smoothmeshconvert Integer)
(def-autocad-variable loftnormals Integer)
(def-autocad-variable loftang1)
(def-autocad-variable loftang2)
(def-autocad-variable loftmag1)
(def-autocad-variable loftmag2)
(def-autocad-variable loftparam Integer)
(def-autocad-variable nomutt)
(def-autocad-variable perspective)
(def-autocad-variable skystatus Skystatus)
(def-autocad-variable sunstatus)
(def-autocad-variable cmdecho)
(def-autocad-variable lenslength Real)
(def-autocad-variable acadver String)
(def-autocad-variable clayer String)
(def-autocad-variable osmode)
(def-autocad-variable expert)
(def-autocad-variable filedia)
(def-autocad-variable vsmin)
(def-autocad-variable vsmax)
(def-autocad-variable viewsize Real)
(def-autocad-variable viewctr VarDouble3)
(def-autocad-variable undoctl Integer)

(define-syntax (with-autocad-variable stx)
  (syntax-case stx ()
    ((with (name new-value) body ...)
     (syntax/loc stx
       (let ((previous (name))
             (new new-value))
         (unless (= previous new)
           (name new))
         (begin0
           (begin body ...)
           (unless (= previous new)
             (name previous))))))))

;; skystatus values
(provide skystatus:off skystatus:background skystatus:background-and-illumination)
(define-type Skystatus (U 0 1 2))
(define skystatus:off : Skystatus 0)
(define skystatus:background : Skystatus 1)
(define skystatus:background-and-illumination : Skystatus 2)


;;; target

(define target-name "TARGET")

(define (get-target-variable) : Loc
  (vector-double-flonum->loc
   (cast (get-variable target-name) VarDouble3)))

;;; view ctr

(define view-ctr-name "VIEWCTR")

(define (get-viewctr-variable)
  (vector-double-flonum->loc
   (cast (get-variable view-ctr-name) VarDouble3)))

;;; view dir

(define view-dir-name "VIEWDIR")

(define (get-viewdir-variable)
  (vector-double-flonum->loc
   (cast (get-variable view-dir-name) VarDouble3)))

;;Turn undo on or off

(provide undo-off)
(define (undo-off)
  (unless (even? (undoctl))
    (send-command #;(active-document) (format "_.undo _C _None\n"))))

(provide undo-on)
(define (undo-on)
  (when (even? (undoctl))
    (send-command #;(active-document) (format "_.undo _All\n"))))

;;Predicates:

(define object-geometry-types : (HashTable String Symbol) (make-hash))
(define-syntax define-object-predicate
  (syntax-rules ()
    ((_ name str type)
     (begin
       (provide name)
       (define (name [id : Com-Object])
         (string=? (object-name id) str))
       (hash-set! object-geometry-types str 'type)))))

(define-object-predicate 2d-polyline? "AcDb2dPolyline" open-or-closed-line)
(define-object-predicate 3d-face? "AcDbFace" surface)
(define-object-predicate nurb-surface? "AcDbNurbSurface" surface)
(define-object-predicate 3d-polyline? "AcDb3dPolyline" open-or-closed-line)
(define-object-predicate 3d-solid? "AcDb3dSolid" solid)
(define-object-predicate arc? "AcDbArc" arc)
(define-object-predicate circle? "AcDbCircle" circle)
(define-object-predicate ellipse? "AcDbEllipse" ellipse)
(define-object-predicate lightweight-polyline? "AcDbPolyline" open-or-closed-line)
(define-object-predicate line? "AcDbLine" line)
(define-object-predicate point? "AcDbPoint" point)
(define-object-predicate region? "AcDbRegion" surface)
(define-object-predicate spline? "AcDbSpline" open-or-closed-spline)
(define-object-predicate surface? "AcDbSurface" surface)
(define-object-predicate extruded-surface? "AcDbExtrudedSurface" surface)
(define-object-predicate lofted-surface? "AcDbLoftedSurface" surface)
(define-object-predicate revolved-surface? "AcDbRevolvedSurface" surface)
(define-object-predicate text? "AcDbText" text)
(define-object-predicate mtext? "AcDbMText" text)
(define-object-predicate surface-grid? "AcDbPolygonMesh" surface)
(define-object-predicate block-reference? "AcDbBlockReference" block-reference)

(provide object-geometry)
(define (object-geometry [obj : Com-Object])
  (let ((type (hash-ref object-geometry-types (object-name obj))))
    (cond ((eq? type 'open-or-closed-line)
           (if (closed obj) 'closed-line 'line))
          ((eq? type 'open-or-closed-spline)
           (if (closed obj) 'closed-spline 'spline))
          (else
           type))))

(provide curve?)
(define (curve? [obj : Com-Object]) : Boolean
  (and (member (object-name obj)
               '("AcDb3dPolyline"
                 "AcDb2dPolyline"
                 "AcDbArc"
                 "AcDbCircle"
                 "AcDbEllipse"
                 "AcDbPolyline"
                 "AcDbLine"
                 "AcDbSpline"))
       #t))

(provide acceptable-surface?)
(define (acceptable-surface? [obj : Com-Object]) : Boolean
  (and (member (object-name obj) ;;HACK Convert to boolean
                '("AcDbSurface" "AcDbFace" "AcDbNurbSurface"))
       #t))

(provide loftable-surface?)
(define (loftable-surface? [obj : Com-Object]) : Boolean
  (and (member (object-name obj) ;;HACK Convert to boolean
                '("AcDbSurface"
                  "AcDbFace"
                  "AcDbNurbSurface"
                  "AcDbRegion"
                  "AcDbLoftedSurface"
                  "AcDbExtrudedSurface"
                  "AcDbSweptSurface"))
       #t))

(provide as-surface)
(define (as-surface [obj : Com-Object]) : Com-Object
  (let ((name (object-name obj)))
    (cond ((string=? name "AcDbRegion")
           (begin0
             (conv-to-surface-command obj)
             (delete obj)))
          ((string=? name "AcDbPolygonMesh")
           (let ((s (mesh-smooth-command obj 0)))
             (begin0
               (conv-to-surface-command s 1)
               (delete obj)
               (delete s))))
          (else
           obj))))

(define (convert-3dpolyline [obj : Com-Object]) : (Listof Com-Object)
  (let ((type (object-name obj)))
    (cond ((string=? type "AcDb3dPolyline")
           (let ((cs (coordinates obj)))
             (begin0
               (for/list : (Listof Com-Object)
                 ((start : Loc (in-list cs))
                  (end : Loc (in-list (cdr cs))))
                 (add-line start end))
               (delete obj))))
          (else
           (list obj)))))

(provide convert-3dpolylines)
(define (convert-3dpolylines [objs : (Listof Com-Object)])
  (apply append (map convert-3dpolyline objs)))

;;Layers

(provide add-layer)
(define (add-layer [name : String])
  (add (layers (active-document)) name))

(provide get-layer)
(define (get-layer [name : String])
  (item (layers (active-document)) name))

(provide add-material)
(define (add-material [name : String])
  (add (materials (active-document)) name))


;;HACK Optimize this
(provide transform)
(define (transform [obj : Com-Object] [p : Loc]) : Com-Object
  (transform-by obj p)
  obj)

(provide boolean-union boolean-intersection boolean-subtraction)

(define (boolean-union [obj0 : Com-Object] [obj1 : Com-Object])
  (acad-boolean obj0 acUnion obj1)
  obj0)

(define (boolean-intersection [obj0 : Com-Object] [obj1 : Com-Object])
  (acad-boolean obj0 acIntersection obj1)
  obj0)

(define (boolean-subtraction [obj0 : Com-Object] [obj1 : Com-Object])
  (acad-boolean obj0 acSubtraction obj1)
  obj0)

;;Really poor's man approach to detect erased objects
(define (erased-object? [obj : Com-Object]) : Boolean
  (with-handlers ((exn:fail? (lambda ([e : exn]) ;;HACK: Typed Racket doesn't provide Exn
                               (regexp-match? "Object was erased" (exn-message e)))))
    (object-name obj)
    #f))

(provide join-curves)
(define (join-curves [curves : Com-Objects]) : Com-Object
  ;;Sometimes, AutoCAD reuses the first shape,
  ;;someother times it creates a new shape
  (join-command curves)
  (if (erased-object? (car curves))
      (item (active-modelspace) (- (count (active-modelspace)) 1))
      (car curves)))
#|
;; spline (to avoid tangents)

(def-new-shape-cmd spline-command (points v0 v1)
  (format "_.spline _mo _Fit ~A ~A~A~A~A"
          (point-string (car points))
          (if v0 (format "_T ~A " v0) "")
          (string-join (map point-string (cdr points)) " ")
          vbCr
          (if v1 (format "_T ~A " v1) "")))

; loft
|#

(provide irregular-pyramid)
(define (irregular-pyramid [cbs : Locs] [ct : Loc])
  (let ([pts0 (map loc-in-world cbs)]
        [pt1 (loc-in-world ct)])
    (let ((pts1 (append (cdr pts0) (list (car pts0)))))
      (let ((faces
             (append
              ;;sides
              (for/list : (Listof Ref)
                ([pt00 : Loc (in-list pts0)]
                 [pt01 : Loc (in-list pts1)])
                (add-3d-face pt00 pt01 pt1 pt1))
              ;;base
              (let ((pt (car pts0)))
                (for/list : (Listof Ref)
                  ([pt00 : Loc (in-list (cdr pts0))]
                   [pt01 : Loc (in-list (cdr pts1))])
                  (add-3d-face pt pt00 pt01 pt01))))))
        (let ((regions (add-region faces)))
          (for-each delete faces)
          (begin0
            (surfsculp-command regions)
            (for-each delete regions)))))))


(provide irregular-pyramid-frustum)
(define (irregular-pyramid-frustum [pts0 : Locs] [pts1 : Locs]) : Ref
  (let ([pts0 (map loc-in-world pts0)]
        [pts1 (map loc-in-world pts1)])
    (let ([pts0r (append (cdr pts0) (list (car pts0)))]
          [pts1r (append (cdr pts1) (list (car pts1)))])
      (let ((faces
             (append
              ;;sides
              (for/list : (Listof Ref)
                ([pt00 : Loc (in-list pts0)]
                 [pt01 : Loc (in-list pts0r)]
                 [pt10 : Loc (in-list pts1)]
                 [pt11 : Loc (in-list pts1r)])
                (add-3d-face pt00 pt01 pt11 pt10))
              ;;top
              (let ((pt (car pts0)))
                (for/list : (Listof Ref)
                  ([pt00 : Loc (in-list (cdr pts0))]
                   [pt01 : Loc (in-list (cdr pts0r))])
                  (add-3d-face pt pt00 pt01 pt01)))
              ;;bottom
              (let ((pt (car pts1)))
                (for/list : (Listof Ref)
                  ([pt00 : Loc (in-list (cdr pts1))]
                   [pt01 : Loc (in-list (cdr pts1r))])
                  (add-3d-face pt pt00 pt01 pt01))))))
        (let ((regions (add-region faces)))
          (for-each delete faces)
          (begin0
            (surfsculp-command regions)
            (for-each delete regions)))))))


(provide loftparam-no-twist
         loftparam-align-direction
         loftparam-simplify
         loftparam-close)

(define loftparam-no-twist 1)
(define loftparam-align-direction 2)
(define loftparam-simplify 4)
(define loftparam-close 8)

;;Added bulge option to avoid chaning behavior
(provide loft-objects-string)
(define (loft-objects-string [objects : Com-Objects] [solid? : Boolean]) : String
  (format
   "._loft _mo ~A ~A~A_bulge 0 0 _cross\n"
   (if solid? "_so" "_su")
   (handents objects)
   vbCr))

(provide loft-objects-path-string)
(define (loft-objects-path-string [objects : Com-Objects] [path : Com-Object] [solid? : Boolean])
  (format
   "._loft _mo ~A ~A~A_path ~A\n"
   (if solid? "_so" "_su")
   (handents objects)
   vbCr;" "
   (handent path)))

(provide loft-objects-guides-string)
(define (loft-objects-guides-string [objects : Com-Objects] [guides : Com-Objects] [solid? : Boolean])
  (format
   "._loft _mo ~A ~A~A_guides ~A~A"
   (if solid? "_so" "_su")
   (handents objects)
   vbCr;" "
   (handents guides)
   vbCr))

(provide loft-to-point-string)
(define (loft-to-point-string [object : Com-Object] [point : Loc] [solid? : Boolean]) : String
  (format
   "._loft _mo ~A ~A _po ~A~A"
   (if solid? "_so" "_su")
   (handent object)
   (loc-string point)
   vbCr))

(provide loft-command)
(define (loft-command [loft-string : String]
                      [ruled? : Boolean]
                      [closed? : Boolean])
  : Com-Object
  (let ((normals : AcLoftedSurfaceNormalType (if ruled? acRuled acSmooth))
        (previous-param (loftparam))
        (previous-normals (loftnormals)))
    (let ((param
           (if closed?
               (bitwise-ior previous-param loftparam-close)
               (bitwise-and previous-param (bitwise-not loftparam-close)))))
      (unless (= previous-param param)
        (loftparam param))
      (unless (= previous-normals normals)
        (loftnormals normals))
      (let ((prev (count (active-modelspace))))
        (send-command loft-string)
        (unless (= previous-param param)
          (loftparam previous-param))
        (unless (= previous-normals normals)
          (loftnormals previous-normals))
        (singleton-or-union
         (created-items (active-modelspace) prev))))))

;;Sweep
#;
(def-new-shapes-cmd (sweep-command [section : Com-Object] [perpendicular? : Boolean] [path : Com-Object] [solid? : Boolean] [rotation : Real] [scale : Real])
  ;;Stupid autocad bug
  (format "._sweep _mo ~A ~A  _A ~A _B 0,0 ~A~A~A "
          (if solid? "_so" "_su")
          (handent section)
          (if perpendicular? "_Yes" "_No")
          (if (= scale 1) "" (format "_S ~A " scale))
          (if (= rotation 0) "" (format "_T ~A " (radians->degrees rotation)))
          (handent path)))

(def-new-shapes-cmd (sweep-command [section : Com-Object] [perpendicular? : Boolean] [path : Com-Object] [solid? : Boolean] [start : Loc] [rotation : Real] [scale : Real])
  ;;Stupid autocad bug
  (format "._sweep _mo ~A ~A  _A ~A _B ~A ~A~A~A "
          (if solid? "_so" "_su")
          (handent section)
          (if perpendicular? "_Yes" "_No")
          (loc-string start)
          (if (= scale 1) "" (format "_S ~A " scale))
          (if (= rotation 0) "" (format "_T ~A " (radians->degrees rotation)))
          (handent path)))

;;Extrude

(def-new-shapes-cmd (extrude-command-length [object : Com-Objects] [length : Real] [solid? : Boolean])
  (format "._extrude _mo ~A ~A  ~A "
          (if solid? "_so" "_su")
          (handents object)
          length))

(def-new-shapes-cmd (extrude-command-direction [object : Com-Objects] [start-point : Loc] [end-point : Loc] [solid? : Boolean])
  (format "._extrude _mo ~A ~A  _d ~A ~A "
          (if solid? "_so" "_su")
          (handents object)
          (loc-string start-point)
          (loc-string end-point)))

;;Revolve

(provide revolve-string)
(define (revolve-string [object : Com-Object] [axis-p0 : Loc] [axis-p1 : Loc] [a0 : Real] [a1 : Real] [solid? : Boolean])
  (format "_.revolve _mo ~A ~A~A~A ~A _start ~A ~A\n"
          (if solid? "_so" "_su")
          (handent object)
          vbCr
          (loc-string axis-p0)
          (loc-string axis-p1)
          (radians->degrees a0) (radians->degrees (- a1 a0))))

(provide revolve-command)
(define (revolve-command [object : Com-Object] [axis-p0 : Loc] [axis-p1 : Loc] [fi : Real] [d-fi : Real] [solid? : Boolean])
  (singleton-or-union (new-shapes-from (revolve-string object axis-p0 axis-p1 fi d-fi solid?))))

;;Convtosurface

(def-new-shape-cmd (mesh-smooth-command [object : Com-Object] [smooth-level : Integer 0])
  (with-autocad-variable (facetersmoothlev smooth-level)
    (format "._meshsmooth ~A~A"
            (handent object)
            vbCr)))

(def-new-shape-cmd (conv-to-surface-command [object : Com-Object] [smooth-level : Integer 0])
  (with-autocad-variable (smoothmeshconvert (if (> smooth-level 0) 0 2))
    (format "._convtosurface ~A~A"
            (handent object)
            vbCr)))

(def-new-shape-cmd (conv-to-solid-command [object : Com-Object] [smooth-level : Integer 0])
  (with-autocad-variable (smoothmeshconvert (if (> smooth-level 0) 0 2))
    (format "._convtosolid ~A~A"
            (handent object)
            vbCr)))

(def-new-shape-cmd (surfsculp-command [objects : Com-Objects])
  (format "._surfsculpt ~A~A"
            (handents objects)
            vbCr))

;;Thicken

(def-new-shape-cmd (thicken-command [object : Com-Object] [length : Real])
  (format "._thicken ~A~A~A\n"
          (handent object)
          vbCr
          length))

#|
;;Helix
(def-new-shape-cmd conic-helix (p0 r0 p1 r1 turns)
  (format "._helix ~A ~A ~A _Turns ~A _Axis ~A\n" 
          (point-string p0)
          r0
          r1 
          turns 
          (point-string p1)))


;;Solid
(def-new-shape-cmd interior-solid (surfaces)
  (format "_.surfsculp ~A~A"
          (handents surfaces)
          vbCr))

;;Offset
(def-new-shape-cmd offset-surface (surface distance)
  (format "_.surfoffset ~A~A~A\n"
          (handent surface)
          vbCr
          distance))

;;Section
|#
(provide slice-command)
#;(define (slice-command object p0 p1 p2 in)
  (send-command
   (format "_.slice ~A~A_3P ~A ~A ~A ~A\n"
           (handent object)
           vbCr
           (point-string p0)
           (point-string p1)
           (point-string p2)
           (point-string in)))
  object)

(define (slice-command [shape : Com-Object] [p : Loc] [n : Vec]) : Com-Object
  (send-command
   (format "_.slice ~A~A_zaxis ~A ~A ~A\n"
           (handent shape)
           vbCr
           (loc-string p)
           (loc-string (p+v p n))
           (loc-string (p-v p n))))
  shape)


(provide clear-selection-command)
(define (clear-selection-command)
  (send-command "(sssetfirst nil)\n"))

(provide select-shape-command)
(define (select-shape-command [s : Com-Object]) : Void
  (send-command (format "(sssetfirst nil (ssadd ~A))\n" (handent s))))

(provide select-shapes-command)
(define (select-shapes-command [ss : Com-Objects]) : Void
  (send-command
   (format "((lambda (/ ss) (setq ss (ssadd)) (foreach s (list ~A) (ssadd s ss)) (sssetfirst nil ss)))\n"
           (handents ss))))

#|
;(provide slice-with-surface)
(def-new-shapes-cmd slice-with-surface (solid surface)
  (format "_.slice ~A~A_Surface ~A _Both\n"
          (handent solid)
          vbCr
          (handent surface)))

(provide fillet-command)
(define (fillet-command object0 object1 radius)
  (new-shape-from
   (format "_.fillet _Radius ~A ~A ~A\n"
           radius
           (handent object0)
           (handent object1))))

(provide xedges-command)
(define (xedges-command objects)
  (new-shapes-from
   (format "_.xedges ~A\n"
           (handents objects))))

(provide flatshot-command)
(define (flatshot-command objects)
  (new-shape-from
   (format "_.flatshot 0,0,0 1 1 0\n"
           #;#;#;
           (handents objects)
           vbCr
           vbCr)))

;; (define (gen-section pt1 pt2 com)
;;   (let ((plane (xyz 0 0 1 world-cs)))
;;     (let ((s (add-section pt1 pt2 plane)))
;;       (com-set-property! s "TopHeight" 3)
;;       (com-set-property! s "BottomHeight" 1)
;;       (com-set-property! s "State" acSectionStatePlane
;; Set ss = .Settings
;; End With

;; With ss
;; .CurrentSectionType = acSectionType2dSection
;; End With

;; Dim acSectionTypeSettings As AcadSectionTypeSettings
;; Set acSectionTypeSettings = ss.GetSectionTypeSettings(acSectionType2dSection)
;; With acSectionTypeSettings
;; .ForegroundLinesVisible = True
;; .BackgroundLinesHiddenLine = True
;; .IntersectionFillHatchPatternName = "ANSI31"
;; 'and other settings
;; End With

;; sec.GenerateSectionGeometry x3DSolid, BoundaryObjs, FillObjs, BakcGroundObjs, ForegroundObjs, CurveTangencyObjs

;; End Sub


|#
;;Utils

(provide closed-lines-points)
(define (closed-lines-points [ls : Com-Objects]) : Locs
  (let loop ([chain : Locs (list (end-point (car ls)) (start-point (car ls)))]
             [ls : Com-Objects (cdr ls)])
    (if (null? ls)
      (reverse chain)
      (let ((end (car chain)))
        (let ((next (findf (lambda ([l1 : Com-Object]) (=c? end (start-point l1))) ls)))
          (if next
              (loop (cons (end-point next) chain) (remq next ls))
              (error 'closed-lines-points "Missing line segment")))))))
#|#|
(defun lib:pline_clockwise ( lw  / LST MAXP MINP)
  (if (= (type lw) 'ENAME)
      (setq lw (vlax-ename->vla-object lw)))  
  (vla-GetBoundingBox lw 'MinP 'MaxP)
  (setq
   minp(vlax-safearray->list minp)
   MaxP(vlax-safearray->list MaxP)
   lst(mapcar(function(lambda(x)
                        (vlax-curve-getParamAtPoint lw
                                                    (vlax-curve-getClosestPointTo lw x))))
             (list minp (list(car minp)(cadr MaxP))
                   MaxP (list(car MaxP)(cadr minp)))))
  (if(or
      (<=(car lst)(cadr lst)(caddr lst)(cadddr lst))
      (<=(cadr lst)(caddr lst)(cadddr lst)(car lst))
      (<=(caddr lst)(cadddr lst)(car lst)(cadr lst))
      (<=(cadddr lst)(car lst)(cadr lst)(caddr lst))) t nil))

(defun C:OFF40 ( )
(vl-load-com)
(if  
(and
  (setq en (car(entsel)))
  (wcmatch (cdr(assoc 0 (entget en))) "*POLYLINE")
  (or (initget 7) t)
  (setq d (getdist "\nOffset distanse: "))
  (setq en (vlax-ename->vla-object en))
  (vlax-write-enabled-p en)
  (vlax-method-applicable-p en 'Offset)
  (if (lib:pline_clockwise en)
    d
    (setq d (- 0 d))  ;_ Plus or minus To change a sign
    )
  (setq i 1)
  (repeat 40
    (vl-catch-all-apply
      '(lambda()
         (vla-offset en (* i d))
         (setq i (1+ i))
         )
      )
    )
  )
(princ " Offset OK")
(princ "Not a polyline or on locket layer")
)
  (princ)
  )
|#

(provide 2dpoly<-3dpoly)
(define (2dpoly<-3dpoly 3dpoly)
  (let ((pts (coordinates 3dpoly))
        (closed? (closed 3dpoly)))
    ;;Just for testing, ignore the fact that the
    ;;3dpoly might not be contained in the XY plane
    (let ((2dpoly (add-polyline (if closed? (append pts (list (car pts))) pts))))
      (when closed? 
        (closed 2dpoly #t))
      2dpoly)))
;    (let ((p0 (car pts))
;          (p1 (cadr pts))
;          (p2 (last pts)))
;      (let ((base (loc-on-points p0 p1 p2 p0)))
;      
;    
;(defun c:p3p2 (/ eg cl en e nc fp pt)
; (princ "\n[Change 3D Polyline to 2D Polyline.]")
; (setq e  (entsel "  Select a 3DPoly: ")          ; get the header data
;       en (car e)
;       cl (if(=(cdr(assoc 70(entget en)))1)1)             ; closed flag 1=yes
;       p1 (cdr(assoc 10(entget(entnext en))))
;       p2 (cdr(assoc 10(entget(entnext(entnext en)))))
; )
; (command "ucs" "3" p1 p2 "")
; (setq fp (trans p1 0 1))                                  ; save first point
;;       en (entnext en))                                   ; leave header
; (command "pline")
; (command fp)                                             ; id first vertex
; (while (/=(cdr(assoc 0(setq eg(entget(setq en(entnext en))))))"SEQEND")
;  (setq pt (trans(cdr(assoc 10 eg))0 1))
;  (if pt (command pt))
;  (if (and (not pt) cl)(command fp))
; );endwhile
; (command "")
; (command "ucs" "w")
; (princ)
;)
;;

|#

(provide singleton-or-union)
(define (singleton-or-union [lst : (Listof Com-Object)]) : Com-Object
  (cond ((null? lst)
         (error 'singleton-or-union "Empty list"))
        ((null? (cdr lst))
         (car lst))
        (else
         (let ((s (copy (car lst))))
           (union-command (cons s (cdr lst)))
           (car lst)))))

;;Render

(def-cmd (render-command [width : Integer] [height : Integer] [filename : Path-String])
  ;;Rendering changed a lot in AutoCAD 2016
  (if (regexp-match? #rx"^20" (version (application)))
      (format "._-render ~A _R ~A ~A _yes ~A\n" "H" width height filename)
      (format "._-render ~A _R ~A ~A _yes ~A\n" "P" width height filename)))

(provide save-screen-png)
(define (save-screen-png [filename : String]) : Void
  (let ((filename (string-append filename ".png")))
    (let ((prev-filedia (filedia)))
      (filedia 0)
      (when (file-exists? filename)
        (delete-file filename))
      (send-command
       (format "_.pngout ~A ~A"
               filename
               vbCr))
      (filedia prev-filedia)))
  (void))

(provide save-screen-eps)
(define (save-screen-eps [filename : String]) : Void
  (let ((filename (string-append filename ".eps")))
    (let ((prev-filedia (filedia)))
      (filedia 0)
      (when (file-exists? filename)
        (delete-file filename))
      (send-command
       (format "_.PSOUT ~A ~A _D _N _M 1 1=1 5000,5000\n" filename vbCr))
      (filedia prev-filedia)))
  (void))


(define (save-eps [filename : String]) : Void
  (let ((filename (string-append filename ".eps")))
    (let ((prev-filedia (filedia)))
      (filedia 0)
      (when (file-exists? filename)
        (delete-file filename))
;;      (send-command (format "_select _all ~A\n" vbCr))
      (send-command 
       (format "_.PSOUT ~A ~A _D _N _M 1 1=1 5000,5000\n" filename vbCr))
      (filedia prev-filedia)))
  (void))

(provide 2d-view)
(define (2d-view [center : (Option Loc) #f] [magnification : Real 1]) : (Values Loc Real)
  (if center
      (begin
        (zoom-center center magnification)
        (values center magnification))
      (values (viewctr)
              (viewsize))))

(provide window-size)
(define (window-size [new-width : (Option Real) #f] [new-height : (Option Real) #f]) : (Values Real Real)
  (if (and new-width new-height)
      (begin
        (width (application) new-width)
        (height (application) new-height)
        (values new-width new-height))
      (values
       (width (application))
       (height (application)))))

(provide window-status)
(define (window-status [state : (Option AcWindowState) #f]) : AcWindowState
  (if state
      (begin
        (window-state (application) state)
        state)
      (window-state (application))))

;;get-view
(defun alisp-get-view (/ params)
  (setq params (tblsearch "VIEW" "foo"))
  (setq params (cddr params))
  (list (mapcar '+ (cdr (assoc 12 params)) (cdr (assoc 11 params)))
        (cdr (assoc 12 params))
        (cdr (assoc 42 params))))

(provide get-view)
(define (get-view) : (Values Loc Loc Real)
  (send-command (format "-VIEW _S foo\n"))
  (begin0
    (match (alisp-get-view)
      [(list (list xa ya za) (list xb yb zb) l)
       (values (xyz (cast xa Real) (cast ya Real) (cast za Real) world-cs)
               (xyz (cast xb Real) (cast yb Real) (cast zb Real) world-cs)
               (cast l Real))])
    (send-command (format "-VIEW _D foo\n"))))


;;set-view
(defun alisp-set-view (cx cy cz tx ty tz d lens)
  (command "_.vscurrent" "_Conceptual")
  (setvar "PERSPECTIVE" 1)
  (command "_.dview" "" "_z" lens "_po" (list tx ty tz) (list cx cy cz) "_d" d "")
  (setvar "SKYSTATUS" 2))

(provide set-view)
(define (set-view [camera : Loc] [target : Loc] [lens : Real])
  (alisp-set-view (cx camera) (cy camera) (cz camera)
                  (cx target) (cy target) (cz target)
                  (distance camera target)
                  lens))

(provide open-dbx-doc)
(define (open-dbx-doc [filename : String])
  (let ((dbx-doc
         (get-interface-object
          (string-append "ObjectDBX.AxDbDocument."
                         (substring (acadver) 0 2)))))
    (open dbx-doc filename)
    dbx-doc))

(provide insert-objects-from)
(define (insert-objects-from [filename : String] [predicate : (Option (-> Com-Object Any)) #f])
  (let ((doc (open-dbx-doc filename)))
    (let ((objs (all-objects (modelspace doc))))
      (let ((filtered-objs (if predicate (filter predicate objs) objs)))
        (unless (null? filtered-objs)
          (copy-objects doc filtered-objs (active-modelspace)))))))
