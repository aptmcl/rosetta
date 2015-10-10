#lang typed/racket

(require "../base/enums.rkt")
                                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ENUMS

(define-enum AcBoolean
  (acFalse 0)
  (acTrue 1))

(define-enum AcOnOff
  (acOff 0)
  (acOn 1))

(define-enum AcEntityName
  (ac3dFace 1)
  (ac3dPolyline 2)
  (ac3dSolid 3)
  (acArc 4)
  (acAttribute 5)
  (acAttributeReference 6)
  (acBlockReference 7)
  (acCircle 8)
  (acDimAligned 9)
  (acDimAngular 10)
  (acDimDiametric 12)
  (acDimOrdinate 13)
  (acDimRadial 14)
  (acDimRotated 15)
  (acEllipse 16)
  (acHatch 17)
  (acLeader 18)
  (acLine 19)
  (acMtext 21)
  (acPoint 22)
  (acPolyline 23)
  (acPolylineLight 24)
  (acPolymesh 25)
  (acRaster 26)
  (acRay 27)
  (acRegion 28)
  (acShape 29)
  (acSolid 30)
  (acSpline 31)
  (acText 32)
  (acTolerance 33)
  (acTrace 34)
  (acPViewport 35)
  (acXline 36)
  (acGroup 37)
  (acMInsertBlock 38)
  (acPolyfaceMesh 39)
  (acMLine 40)
  (acDim3PointAngular 41)
  (acExternalReference 42)
  (acTable 43)
  (acDimArcLength 44)
  (acDimRadialLarge 45)
  (acDwfUnderlay 46)
  (acDgnUnderlay 47)
  (acMLeader 48)
  (acSubDMesh 49)
  (acPdfUnderlay 50)
  (acNurbSurface 51))

(define-enum AcActiveSpace
  (acPaperSpace 0)
  (acModelSpace 1))

(define-enum AcKeyboardAccelerator
  (acPreferenceClassic 0)
  (acPreferenceCustom 1))

(define-enum AcPlotOrientation
  (acPlotOrientationPortrait 0)
  (acPlotOrientationLandscape 1))

(define-enum AcColor
  (acByBlock 0)
  (acRed 1)
  (acYellow 2)
  (acGreen 3)
  (acCyan 4)
  (acBlue 5)
  (acMagenta 6)
  (acWhite 7)
  (acByLayer 256))

(define-enum AcAttachmentPoint
  (acAttachmentPointTopLeft 1)
  (acAttachmentPointTopCenter 2)
  (acAttachmentPointTopRight 3)
  (acAttachmentPointMiddleLeft 4)
  (acAttachmentPointMiddleCenter 5)
  (acAttachmentPointMiddleRight 6)
  (acAttachmentPointBottomLeft 7)
  (acAttachmentPointBottomCenter 8)
  (acAttachmentPointBottomRight 9))

(define-enum AcDrawingDirection
  (acLeftToRight 1)
  (acRightToLeft 2)
  (acTopToBottom 3)
  (acBottomToTop 4)
  (acByStyle 5))

(define-enum AcLeaderType
  (acLineNoArrow 0)
  (acSplineNoArrow 1)
  (acLineWithArrow 2)
  (acSplineWithArrow 3))

(define-enum AcAttributeMode
  (acAttributeModeNormal 0)
  (acAttributeModeInvisible 1)
  (acAttributeModeConstant 2)
  (acAttributeModeVerify 4)
  (acAttributeModePreset 8)
  (acAttributeModeLockPosition 16)
  (acAttributeModeMultipleLine 32))

(define-enum AcHorizontalAlignment
  (acHorizontalAlignmentLeft 0)
  (acHorizontalAlignmentCenter 1)
  (acHorizontalAlignmentRight 2)
  (acHorizontalAlignmentAligned 3)
  (acHorizontalAlignmentMiddle 4)
  (acHorizontalAlignmentFit 5))

(define-enum AcVerticalAlignment
  (acVerticalAlignmentBaseline 0)
  (acVerticalAlignmentBottom 1)
  (acVerticalAlignmentMiddle 2)
  (acVerticalAlignmentTop 3))

(define-enum AcTextGenerationFlag
  (acTextFlagBackward 2)
  (acTextFlagUpsideDown 4))

(define-enum AcSelect
  (acSelectionSetWindow 0)
  (acSelectionSetCrossing 1)
  (acSelectionSetFence 2)
  (acSelectionSetPrevious 3)
  (acSelectionSetLast 4)
  (acSelectionSetAll 5)
  (acSelectionSetWindowPolygon 6)
  (acSelectionSetCrossingPolygon 7))

(define-enum AcPatternType
  (acHatchPatternTypeUserDefined 0)
  (acHatchPatternTypePreDefined 1)
  (acHatchPatternTypeCustomDefined 2))

(define-enum AcLoopType
  (acHatchLoopTypeDefault 0)
  (acHatchLoopTypeExternal 1)
  (acHatchLoopTypePolyline 2)
  (acHatchLoopTypeDerived 4)
  (acHatchLoopTypeTextbox 8))

(define-enum AcHatchStyle
  (acHatchStyleNormal 0)
  (acHatchStyleOuter 1)
  (acHatchStyleIgnore 2))

(define-enum AcPolylineType
  (acSimplePoly 0)
  (acFitCurvePoly 1)
  (acQuadSplinePoly 2)
  (acCubicSplinePoly 3))

(define-enum Ac3DPolylineType
  (acSimple3DPoly 0)
  (acQuadSpline3DPoly 1)
  (acCubicSpline3DPoly 2))

(define-enum AcViewportSplitType
  (acViewport2Horizontal 0)
  (acViewport2Vertical 1)
  (acViewport3Left 2)
  (acViewport3Right 3)
  (acViewport3Horizontal 4)
  (acViewport3Vertical 5)
  (acViewport3Above 6)
  (acViewport3Below 7)
  (acViewport4 8))

(define-enum AcRegenType
  (acActiveViewport 0)
  (acAllViewports 1))

(define-enum AcBooleanType
  (acUnion 0)
  (acIntersection 1)
  (acSubtraction 2))

(define-enum AcExtendOption
  (acExtendNone 0)
  (acExtendThisEntity 1)
  (acExtendOtherEntity 2)
  (acExtendBoth 3))

(define-enum AcAngleUnits
  (acDegrees 0)
  (acDegreeMinuteSeconds 1)
  (acGrads 2)
  (acRadians 3))

(define-enum AcUnits
  (acDefaultUnits -1)
  (acScientific 1)
  (acDecimal 2)
  (acEngineering 3)
  (acArchitectural 4)
  (acFractional 5))

(define-enum AcCoordinateSystem
  (acWorld 0)
  (acUCS 1)
  (acDisplayDCS 2)
  (acPaperSpaceDCS 3)
  (acOCS 4))

(define-enum AcMeasurementUnits
  (acEnglish 0)
  (acMetric 1))

(define-enum AcXRefDemandLoad
  (acDemandLoadDisabled 0)
  (acDemandLoadEnabled 1)
  (acDemandLoadEnabledWithCopy 2))

(define-enum AcPreviewMode
  (acPartialPreview 0)
  (acFullPreview 1))

(define-enum AcPolymeshType
  (acSimpleMesh 0)
  (acQuadSurfaceMesh 5)
  (acCubicSurfaceMesh 6)
  (acBezierSurfaceMesh 8))

(define-enum AcZoomScaleType
  (acZoomScaledAbsolute 0)
  (acZoomScaledRelative 1)
  (acZoomScaledRelativePSpace 2))

(define-enum AcDragDisplayMode
  (acDragDoNotDisplay 0)
  (acDragDisplayOnRequest 1)
  (acDragDisplayAutomatically 2))

(define-enum AcARXDemandLoad
  (acDemanLoadDisable 0)
  (acDemandLoadOnObjectDetect 1)
  (acDemandLoadCmdInvoke 2))

(define-enum AcTextFontStyle
  (acFontRegular 0)
  (acFontItalic 1)
  (acFontBold 2)
  (acFontBoldItalic 3))

(define-enum AcProxyImage
  (acProxyNotShow 0)
  (acProxyShow 1)
  (acProxyBoundingBox 2))

(define-enum AcKeyboardPriority
  (acKeyboardRunningObjSnap 0)
  (acKeyboardEntry 1)
  (acKeyboardEntryExceptScripts 2))

(define-enum AcMenuGroupType
  (acBaseMenuGroup 0)
  (acPartialMenuGroup 1))

(define-enum AcMenuFileType
  (acMenuFileCompiled 0)
  (acMenuFileSource 1))

(define-enum AcMenuItemType
  (acMenuItem 0)
  (acMenuSeparator 1)
  (acMenuSubMenu 2))

(define-enum AcToolbarItemType
  (acToolbarButton 0)
  (acToolbarSeparator 1)
  (acToolbarControl 2)
  (acToolbarFlyout 3))

(define-enum AcToolbarDockStatus
  (acToolbarDockTop 0)
  (acToolbarDockBottom 1)
  (acToolbarDockLeft 2)
  (acToolbarDockRight 3)
  (acToolbarFloating 4))

(define-enum AcLineWeight
  (acLnWt000 0)
  (acLnWt005 5)
  (acLnWt009 9)
  (acLnWt013 13)
  (acLnWt015 15)
  (acLnWt018 18)
  (acLnWt020 20)
  (acLnWt025 25)
  (acLnWt030 30)
  (acLnWt035 35)
  (acLnWt040 40)
  (acLnWt050 50)
  (acLnWt053 53)
  (acLnWt060 60)
  (acLnWt070 70)
  (acLnWt080 80)
  (acLnWt090 90)
  (acLnWt100 100)
  (acLnWt106 106)
  (acLnWt120 120)
  (acLnWt140 140)
  (acLnWt158 158)
  (acLnWt200 200)
  (acLnWt211 211)
  (acLnWtByLayer -1)
  (acLnWtByBlock -2)
  (acLnWtByLwDefault -3))

(define-enum AcWindowState
  (acNorm 1)
  (acMin 2)
  (acMax 3))

(define-enum AcPlotPaperUnits
  (acInches 0)
  (acMillimeters 1)
  (acPixels 2))

(define-enum AcPlotRotation
  (ac0degrees 0)
  (ac90degrees 1)
  (ac180degrees 2)
  (ac270degrees 3))

(define-enum AcPlotType
  (acDisplay 0)
  (acExtents 1)
  (acLimits 2)
  (acView 3)
  (acWindow 4)
  (acLayout 5))

(define-enum AcPlotScale
  (acScaleToFit 0)
  (ac1_128in_1ft 1)
  (ac1_64in_1ft 2)
  (ac1_32in_1ft 3)
  (ac1_16in_1ft 4)
  (ac3_32in_1ft 5)
  (ac1_8in_1ft 6)
  (ac3_16in_1ft 7)
  (ac1_4in_1ft 8)
  (ac3_8in_1ft 9)
  (ac1_2in_1ft 10)
  (ac3_4in_1ft 11)
  (ac1in_1ft 12)
  (ac3in_1ft 13)
  (ac6in_1ft 14)
  (ac1ft_1ft 15)
  (ac1_1 16)
  (ac1_2 17)
  (ac1_4 18)
  (ac1_5 19)
  (ac1_8 20)
  (ac1_10 21)
  (ac1_16 22)
  (ac1_20 23)
  (ac1_30 24)
  (ac1_40 25)
  (ac1_50 26)
  (ac1_100 27)
  (ac2_1 28)
  (ac4_1 29)
  (ac8_1 30)
  (ac10_1 31)
  (ac100_1 32))

(define-enum AcAlignment
  (acAlignmentLeft 0)
  (acAlignmentCenter 1)
  (acAlignmentRight 2)
  (acAlignmentAligned 3)
  (acAlignmentMiddle 4)
  (acAlignmentFit 5)
  (acAlignmentTopLeft 6)
  (acAlignmentTopCenter 7)
  (acAlignmentTopRight 8)
  (acAlignmentMiddleLeft 9)
  (acAlignmentMiddleCenter 10)
  (acAlignmentMiddleRight 11)
  (acAlignmentBottomLeft 12)
  (acAlignmentBottomCenter 13)
  (acAlignmentBottomRight 14))

(define-enum AcLineSpacingStyle
  (acLineSpacingStyleAtLeast 1)
  (acLineSpacingStyleExactly 2))

(define-enum AcDimPrecision
  (acDimPrecisionZero 0)
  (acDimPrecisionOne 1)
  (acDimPrecisionTwo 2)
  (acDimPrecisionThree 3)
  (acDimPrecisionFour 4)
  (acDimPrecisionFive 5)
  (acDimPrecisionSix 6)
  (acDimPrecisionSeven 7)
  (acDimPrecisionEight 8))

(define-enum AcDimUnits
  (acDimScientific 1)
  (acDimDecimal 2)
  (acDimEngineering 3)
  (acDimArchitecturalStacked 4)
  (acDimFractionalStacked 5)
  (acDimArchitectural 6)
  (acDimFractional 7)
  (acDimWindowsDesktop 8))

(define-enum AcDimLUnits
  (acDimLScientific 1)
  (acDimLDecimal 2)
  (acDimLEngineering 3)
  (acDimLArchitectural 4)
  (acDimLFractional 5)
  (acDimLWindowsDesktop 6))

(define-enum AcDimArrowheadType
  (acArrowDefault 0)
  (acArrowClosedBlank 1)
  (acArrowClosed 2)
  (acArrowDot 3)
  (acArrowArchTick 4)
  (acArrowOblique 5)
  (acArrowOpen 6)
  (acArrowOrigin 7)
  (acArrowOrigin2 8)
  (acArrowOpen90 9)
  (acArrowOpen30 10)
  (acArrowDotSmall 11)
  (acArrowDotBlank 12)
  (acArrowSmall 13)
  (acArrowBoxBlank 14)
  (acArrowBoxFilled 15)
  (acArrowDatumBlank 16)
  (acArrowDatumFilled 17)
  (acArrowIntegral 18)
  (acArrowNone 19)
  (acArrowUserDefined 20))

(define-enum AcDimCenterType
  (acCenterMark 0)
  (acCenterLine 1)
  (acCenterNone 2))

(define-enum AcDimFit
  (acTextAndArrows 0)
  (acArrowsOnly 1)
  (acTextOnly 2)
  (acBestFit 3))

(define-enum AcDimFractionType
  (acHorizontal 0)
  (acDiagonal 1)
  (acNotStacked 2))

(define-enum AcDimArcLengthSymbol
  (acSymInFront 0)
  (acSymAbove 1)
  (acSymNone 2))

(define-enum AcDimHorizontalJustification
  (acHorzCentered 0)
  (acFirstExtensionLine 1)
  (acSecondExtensionLine 2)
  (acOverFirstExtension 3)
  (acOverSecondExtension 4))

(define-enum AcDimVerticalJustification
  (acVertCentered 0)
  (acAbove 1)
  (acOutside 2)
  (acJIS 3)
  (acUnder 4))

(define-enum AcDimTextMovement
  (acDimLineWithText 0)
  (acMoveTextAddLeader 1)
  (acMoveTextNoLeader 2))

(define-enum AcDimToleranceMethod
  (acTolNone 0)
  (acTolSymmetrical 1)
  (acTolDeviation 2)
  (acTolLimits 3)
  (acTolBasic 4))

(define-enum AcDimToleranceJustify
  (acTolBottom 0)
  (acTolMiddle 1)
  (acTolTop 2))

(define-enum AcViewportScale
  (acVpScaleToFit 0)
  (acVpCustomScale 1)
  (acVp1_1 2)
  (acVp1_2 3)
  (acVp1_4 4)
  (acVp1_5 5)
  (acVp1_8 6)
  (acVp1_10 7)
  (acVp1_16 8)
  (acVp1_20 9)
  (acVp1_30 10)
  (acVp1_40 11)
  (acVp1_50 12)
  (acVp1_100 13)
  (acVp2_1 14)
  (acVp4_1 15)
  (acVp8_1 16)
  (acVp10_1 17)
  (acVp100_1 18)
  (acVp1_128in_1ft 19)
  (acVp1_64in_1ft 20)
  (acVp1_32in_1ft 21)
  (acVp1_16in_1ft 22)
  (acVp3_32in_1ft 23)
  (acVp1_8in_1ft 24)
  (acVp3_16in_1ft 25)
  (acVp1_4in_1ft 26)
  (acVp3_8in_1ft 27)
  (acVp1_2in_1ft 28)
  (acVp3_4in_1ft 29)
  (acVp1in_1ft 30)
  (acVp1and1_2in_1ft 31)
  (acVp3in_1ft 32)
  (acVp6in_1ft 33)
  (acVp1ft_1ft 34))

(define-enum AcISOPenWidth
  (acPenWidth013 13)
  (acPenWidth018 18)
  (acPenWidth025 25)
  (acPenWidth035 35)
  (acPenWidth050 50)
  (acPenWidth070 70)
  (acPenWidth100 100)
  (acPenWidth140 140)
  (acPenWidth200 200)
  (acPenWidthUnk -1))

(define-enum AcSaveAsType
  (acUnknown -1)
  (acR12_dxf 1)
  (acR13_dwg 4)
  (acR13_dxf 5)
  (acR14_dwg 8)
  (acR14_dxf 9)
  (ac2000_dwg 12)
  (ac2000_dxf 13)
  (ac2000_Template 14)
  (ac2004_dwg 24)
  (ac2004_dxf 25)
  (ac2004_Template 26)
  (ac2007_dwg 36)
  (ac2007_dxf 37)
  (ac2007_Template 38)
  (ac2010_dwg 48)
  (ac2010_dxf 49)
  (ac2010_Template 50)
  (acNative 48)
  (acR15_dwg 12)
  (acR15_dxf 13)
  (acR15_Template 14)
  (acR18_dwg 24)
  (acR18_dxf 25)
  (acR18_Template 26))

(define-enum AcPrinterSpoolAlert
  (acPrinterAlwaysAlert 0)
  (acPrinterAlertOnce 1)
  (acPrinterNeverAlertLogOnce 2)
  (acPrinterNeverAlert 3))

(define-enum AcPlotPolicyForNewDwgs
  (acPolicyNewDefault 0)
  (acPolicyNewLegacy 1))

(define-enum AcPlotPolicyForLegacyDwgs
  (acPolicyLegacyDefault 0)
  (acPolicyLegacyQuery 1)
  (acPolicyLegacyLegacy 2))

(define-enum AcOleQuality
  (acOQLineArt 0)
  (acOQText 1)
  (acOQGraphics 2)
  (acOQPhoto 3)
  (acOQHighPhoto 4))

(define-enum AcLoadPalette
  (acPaletteByDrawing 0)
  (acPaletteBySession 1))

(define-enum AcInsertUnits
  (acInsertUnitsUnitless 0)
  (acInsertUnitsInches 1)
  (acInsertUnitsFeet 2)
  (acInsertUnitsMiles 3)
  (acInsertUnitsMillimeters 4)
  (acInsertUnitsCentimeters 5)
  (acInsertUnitsMeters 6)
  (acInsertUnitsKilometers 7)
  (acInsertUnitsMicroinches 8)
  (acInsertUnitsMils 9)
  (acInsertUnitsYards 10)
  (acInsertUnitsAngstroms 11)
  (acInsertUnitsNanometers 12)
  (acInsertUnitsMicrons 13)
  (acInsertUnitsDecimeters 14)
  (acInsertUnitsDecameters 15)
  (acInsertUnitsHectometers 16)
  (acInsertUnitsGigameters 17)
  (acInsertUnitsAstronomicalUnits 18)
  (acInsertUnitsLightYears 19)
  (acInsertUnitsParsecs 20))

(define-enum AcAlignmentPointAcquisition
  (acAlignPntAcquisitionAutomatic 0)
  (acAlignPntAcquisitionShiftToAcquire 1))

(define-enum AcInsertUnitsAction
  (acInsertUnitsPrompt 0)
  (acInsertUnitsAutoAssign 1))

(define-enum AcPlotPolicy
  (acPolicyNamed 0)
  (acPolicyLegacy 1))

(define-enum AcDrawingAreaShortCutMenu
  (acNoDrawingAreaShortCutMenu 0)
  (acUseDefaultDrawingAreaShortCutMenu 1))

(define-enum AcDrawingAreaSCMDefault
  (acRepeatLastCommand 0)
  (acSCM 1))

(define-enum AcDrawingAreaSCMEdit
  (acEdRepeatLastCommand 0)
  (acEdSCM 1))

(define-enum AcDrawingAreaSCMCommand
  (acEnter 0)
  (acEnableSCMOptions 1)
  (acEnableSCM 2))

(define-enum AcLayerStateMask
  (acLsNone 0)
  (acLsOn 1)
  (acLsFrozen 2)
  (acLsLocked 4)
  (acLsPlot 8)
  (acLsNewViewport 16)
  (acLsColor 32)
  (acLsLineType 64)
  (acLsLineWeight 128)
  (acLsPlotStyle 256)
  (acLsAll 65535))

(define-enum AcShadePlot
  (acShadePlotAsDisplayed 0)
  (acShadePlotWireframe 1)
  (acShadePlotHidden 2)
  (acShadePlotRendered 3))

(define-enum AcHatchObjectType
  (acHatchObject 0)
  (acGradientObject 1))

(define-enum AcGradientPatternType
  (acPreDefinedGradient 0)
  (acUserDefinedGradient 1))

(define-enum AcTableDirection
  (acTableTopToBottom 0)
  (acTableBottomToTop 1))

(define-enum AcCellAlignment
  (acTopLeft 1)
  (acTopCenter 2)
  (acTopRight 3)
  (acMiddleLeft 4)
  (acMiddleCenter 5)
  (acMiddleRight 6)
  (acBottomLeft 7)
  (acBottomCenter 8)
  (acBottomRight 9))

(define-enum AcRowType
  (acUnknownRow 0)
  (acDataRow 1)
  (acTitleRow 2)
  (acHeaderRow 4))

(define-enum AcValueDataType
  (acUnknownDataType 0)
  (acLong 1)
  (acDouble 2)
  (acString 4)
  (acDate 8)
  (acPoint2d 16)
  (acPoint3d 32)
  (acObjectId 64)
  (acBuffer 128)
  (acResbuf 256)
  (acGeneral 512))

(define-enum AcValueUnitType
  (acUnitless 0)
  (acUnitDistance 1)
  (acUnitAngle 2)
  (acUnitArea 4)
  (acUnitVolume 8))

(define-enum AcParseOption
  (acParseOptionNone 0)
  (acSetDefaultFormat 1)
  (acPreserveMtextFormat 2))

(define-enum AcFormatOption
  (kFormatOptionNone 0)
  (acForEditing 1)
  (acForExpression 2)
  (acUseMaximumPrecision 4)
  (acIgnoreMtextFormat 8))

(define-enum AcGridLineType
  (acInvalidGridLine 0)
  (acHorzTop 1)
  (acHorzInside 2)
  (acHorzBottom 4)
  (acVertLeft 8)
  (acVertInside 16)
  (acVertRight 32))

(define-enum AcCellType
  (acUnknownCell 0)
  (acTextCell 1)
  (acBlockCell 2))

(define-enum AcCellEdgeMask
  (acTopMask 1)
  (acRightMask 2)
  (acBottomMask 4)
  (acLeftMask 8))

(define-enum AcRotationAngle
  (acDegreesUnknown -1)
  (acDegrees000 0)
  (acDegrees090 1)
  (acDegrees180 2)
  (acDegrees270 3))

(define-enum AcMergeCellStyleOption
  (acMergeCellStyleNone 0)
  (acMergeCellStyleCopyDuplicates 1)
  (acMergeCellStyleOverwriteDuplicates 2)
  (acMergeCellStyleConvertDuplicatesToOverrides 4)
  (acMergeCellStyleIgnoreNewStyles 8))

(define-enum AcSelectType
  (acTableSelectWindow 1)
  (acTableSelectCrossing 2))

(define-enum AcOleType
  (acOTLink 1)
  (acOTEmbedded 2)
  (acOTStatic 3))

(define-enum AcOlePlotQuality
  (acOPQMonochrome 0)
  (acOPQLowGraphics 1)
  (acOPQHighGraphics 2))

(define-enum AcTableStyleOverrides
  (acTitleSuppressed 1)
  (acHeaderSuppressed 2)
  (acFlowDirection 3)
  (acHorzCellMargin 4)
  (acVertCellMargin 5)
  (acTitleRowColor 6)
  (acHeaderRowColor 7)
  (acDataRowColor 8)
  (acTitleRowFillNone 9)
  (acHeaderRowFillNone 10)
  (acDataRowFillNone 11)
  (acTitleRowFillColor 12)
  (acHeaderRowFillColor 13)
  (acDataRowFillColor 14)
  (acTitleRowAlignment 15)
  (acHeaderRowAlignment 16)
  (acDataRowAlignment 17)
  (acTitleRowTextStyle 18)
  (acHeaderRowTextStyle 19)
  (acDataRowTextStyle 20)
  (acTitleRowTextHeight 21)
  (acHeaderRowTextHeight 22)
  (acDataRowTextHeight 23)
  (acTitleRowDataType 24)
  (acHeaderRowDataType 25)
  (acDataRowDataType 26)
  (acTitleHorzTopColor 40)
  (acTitleHorzInsideColor 41)
  (acTitleHorzBottomColor 42)
  (acTitleVertLeftColor 43)
  (acTitleVertInsideColor 44)
  (acTitleVertRightColor 45)
  (acHeaderHorzTopColor 46)
  (acHeaderHorzInsideColor 47)
  (acHeaderHorzBottomColor 48)
  (acHeaderVertLeftColor 49)
  (acHeaderVertInsideColor 50)
  (acHeaderVertRightColor 51)
  (acDataHorzTopColor 52)
  (acDataHorzInsideColor 53)
  (acDataHorzBottomColor 54)
  (acDataVertLeftColor 55)
  (acDataVertInsideColor 56)
  (acDataVertRightColor 57)
  (acTitleHorzTopLineWeight 70)
  (acTitleHorzInsideLineWeight 71)
  (acTitleHorzBottomLineWeight 72)
  (acTitleVertLeftLineWeight 73)
  (acTitleVertInsideLineWeight 74)
  (acTitleVertRightLineWeight 75)
  (acHeaderHorzTopLineWeight 76)
  (acHeaderHorzInsideLineWeight 77)
  (acHeaderHorzBottomLineWeight 78)
  (acHeaderVertLeftLineWeight 79)
  (acHeaderVertInsideLineWeight 80)
  (acHeaderVertRightLineWeight 81)
  (acDataHorzTopLineWeight 82)
  (acDataHorzInsideLineWeight 83)
  (acDataHorzBottomLineWeight 84)
  (acDataVertLeftLineWeight 85)
  (acDataVertInsideLineWeight 86)
  (acDataVertRightLineWeight 87)
  (acTitleHorzTopVisibility 100)
  (acTitleHorzInsideVisibility 101)
  (acTitleHorzBottomVisibility 102)
  (acTitleVertLeftVisibility 103)
  (acTitleVertInsideVisibility 104)
  (acTitleVertRightVisibility 105)
  (acHeaderHorzTopVisibility 106)
  (acHeaderHorzInsideVisibility 107)
  (acHeaderHorzBottomVisibility 108)
  (acHeaderVertLeftVisibility 109)
  (acHeaderVertInsideVisibility 110)
  (acHeaderVertRightVisibility 111)
  (acDataHorzTopVisibility 112)
  (acDataHorzInsideVisibility 113)
  (acDataHorzBottomVisibility 114)
  (acDataVertLeftVisibility 115)
  (acDataVertInsideVisibility 116)
  (acDataVertRightVisibility 117)
  (acCellAlign 130)
  (acCellBackgroundFillNone 131)
  (acCellBackgroundColor 132)
  (acCellContentColor 133)
  (acCellTextStyle 134)
  (acCellTextHeight 135)
  (acCellTopGridColor 136)
  (acCellRightGridColor 137)
  (acCellBottomGridColor 138)
  (acCellLeftGridColor 139)
  (acCellTopGridLineWeight 140)
  (acCellRightGridLineWeight 141)
  (acCellBottomGridLineWeight 142)
  (acCellLeftGridLineWeight 143)
  (acCellTopVisibility 144)
  (acCellRightVisibility 145)
  (acCellBottomVisibility 146)
  (acCellLeftVisibility 147)
  (acCellDataType 148))

(define-enum AcMLineJustification
  (acTop 0)
  (acZero 1)
  (acBottom 2))

(define-enum AcBlockScaling
  (acAny 0)
  (acUniform 1))

(define-enum AcDynamicBlockReferencePropertyUnitsType
  (acNoUnits 0)
  (acAngular 1)
  (acDistance 2)
  (acArea 3))

(define-enum AcSectionState
  (acSectionStatePlane 1)
  (acSectionStateBoundary 2)
  (acSectionStateVolume 4))

(define-enum AcSectionSubItem
  (acSectionSubItemkNone 0)
  (acSectionSubItemSectionLine 1)
  (acSectionSubItemSectionLineTop 2)
  (acSectionSubItemSectionLineBottom 4)
  (acSectionSubItemBackLine 8)
  (acSectionSubItemBackLineTop 16)
  (acSectionSubItemBackLineBottom 32)
  (acSectionSubItemVerticalLineTop 64)
  (acSectionSubItemVerticalLineBottom 128))

(define-enum AcSectionType
  (acSectionTypeLiveSection 1)
  (acSectionType2dSection 2)
  (acSectionType3dSection 4))

(define-enum AcSectionGeneration
  (acSectionGenerationSourceAllObjects 1)
  (acSectionGenerationSourceSelectedObjects 2)
  (acSectionGenerationDestinationNewBlock 16)
  (acSectionGenerationDestinationReplaceBlock 32)
  (acSectionGenerationDestinationFile 64))

(define-enum AcHelixConstrainType
  (acTurnHeight 0)
  (acTurns 1)
  (acHeight 2))

(define-enum AcShadowDisplayType
  (acCastsAndReceivesShadows 0)
  (acCastsShadows 1)
  (acReceivesShadows 2)
  (acIgnoreShadows 3))

(define-enum AcLoftedSurfaceNormalType
  (acRuled 0)
  (acSmooth 1)
  (acFirstNormal 2)
  (acLastNormal 3)
  (acEndsNormal 4)
  (acAllNormal 5)
  (acUseDraftAngles 6))

(define-enum AcHelixTwistType
  (acCCW 0)
  (acCW 1))

(define-enum AcUnderlayLayerOverrideType
  (acNoOverrides 0)
  (acApplied 1))

(define-enum AcTableFlowDirection
  (acTableFlowRight 1)
  (acTableFlowDownOrUp 2)
  (acTableFlowLeft 4))

(define-enum AcCellMargin
  (acCellMarginTop 1)
  (acCellMarginLeft 2)
  (acCellMarginBottom 4)
  (acCellMarginRight 8)
  (acCellMarginHorzSpacing 16)
  (acCellMarginVertSpacing 32))

(define-enum AcCellContentLayout
  (acCellContentLayoutFlow 1)
  (acCellContentLayoutStackedHorizontal 2)
  (acCellContentLayoutStackedVertical 4))

(define-enum AcCellProperty
  (acInvalidCellProperty 0)
  (acLock 1)
  (acDataType 2)
  (acDataFormat 4)
  (acRotation 8)
  (acScale 16)
  (acAlignmentProperty 32)
  (acContentColor 64)
  (acBackgroundColor 128)
  (acTextStyle 256)
  (acTextHeight 512)
  (acMarginLeft 1024)
  (acMarginTop 2048)
  (acMarginRight 4096)
  (acMarginBottom 8192)
  (acEnableBackgroundColor 16384)
  (acAutoScale 32768)
  (acMergeAll 0)
  (acFlowDirBtoT 0)
  (acContentLayout 0)
  (acDataTypeAndFormat 6)
  (acContentProperties 33662)
  (acBitProperties 49152)
  (acAllCellProperties 65535))

(define-enum AcGridLineStyle
  (acGridLineStyleSingle 1)
  (acGridLineStyleDouble 2))

(define-enum AcDataLinkUpdateDirection
  (acUpdateDataFromSource 1)
  (acUpdateSourceFromData 2))

(define-enum AcDataLinkUpdateOption
  (acUpdateOptionNone 0)
  (acUpdateOptionOverwriteContentModifiedAfterUpdate 0)
  (acUpdateOptionOverwriteFormatModifiedAfterUpdate 0)
  (acUpdateOptionUpdateFullSourceRange 0)
  (acUpdateOptionIncludeXrefs 0))

(define-enum AcCellContentType
  (acCellContentTypeUnknown 0)
  (acCellContentTypeValue 1)
  (acCellContentTypeField 2)
  (acCellContentTypeBlock 4))

(define-enum AcCellState
  (acCellStateNone 0)
  (acCellStateContentLocked 1)
  (acCellStateContentReadOnly 2)
  (acCellStateFormatLocked 4)
  (acCellStateFormatReadOnly 8)
  (acCellStateLinked 16)
  (acCellStateContentModified 32)
  (acCellStateFormatModified 64))

(define-enum AcCellOption
  (kCellOptionNone 0)
  (kInheritCellFormat 1))

(define-enum AcTextAttachmentType
  (acAttachmentTopOfTop 0)
  (acAttachmentMiddleOfTop 1)
  (acAttachmentBottomOfTop 2)
  (acAttachmentBottomOfTopLine 3)
  (acAttachmentMiddle 4)
  (acAttachmentMiddleOfBottom 5)
  (acAttachmentBottomOfBottom 6)
  (acAttachmentBottomLine 7)
  (acAttachmentAllLine 8))

(define-enum AcMLeaderType
  (acStraightLeader 1)
  (acSplineLeader 2)
  (acInVisibleLeader 0))

(define-enum AcMLeaderContentType
  (acNoneContent 0)
  (acBlockContent 1)
  (acMTextContent 2))

(define-enum AcTextAlignmentType
  (acLeftAlignment 0)
  (acCenterAlignment 1)
  (acRightAlignment 2))

(define-enum AcTextAngleType
  (acInsertAngle 0)
  (acHorizontalAngle 1)
  (acAlwaysRightReadingAngle 2))

(define-enum AcBlockConnectionType
  (acConnectExtents 0)
  (acConnectBase 1))

(define-enum AcPredefBlockType
  (acBlockImperial 0)
  (acBlockSlot 1)
  (acBlockCircle 2)
  (acBlockBox 3)
  (acBlockHexagon 4)
  (acBlockTriangle 5)
  (acBlockUserDefined 6))

(define-enum AcDrawLeaderOrderType
  (acDrawLeaderHeadFirst 0)
  (acDrawLeaderTailFirst 1))

(define-enum AcDrawMLeaderOrderType
  (acDrawContentFirst 0)
  (acDrawLeaderFirst 1))

(define-enum AcSegmentAngleType
  (acDegreesAny 0)
  (acDegrees15 1)
  (acDegrees30 2)
  (acDegrees45 3)
  (acDegrees60 4)
  (acDegrees90 6)
  (acDegreesHorz 12))

(define-enum AcTextAttachmentDirection
  (acAttachmentHorizontal 0)
  (acAttachmentVertical 1))

(define-enum AcVerticalTextAttachmentType
  (acAttachmentCenter 0)
  (acAttachmentLinedCenter 1))

(define-enum AcMeshCreaseType
  (acNoneCrease 0)
  (acAlwaysCrease 1)
  (acCreaseByLevel 2))