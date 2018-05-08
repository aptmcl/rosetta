#lang racket/base
(require (for-syntax racket/base))
(require racket/generic)
(require (for-syntax racket/generic))
(require racket/file)
(require racket/runtime-path)
(require racket/system)
(require racket/date)
(require racket/list)

(provide (all-defined-out))

(define-generics radiance
  (radiance-string radiance))

(struct material
  (name
   type
   red
   green
   blue)
  #:methods gen:radiance
  [(define (radiance-string mat)
     (format "void ~A ~A~%0~%0~%3 ~A ~A ~A~%"
             (material-type mat)
             (material-name mat)
             (material-red mat)
             (material-green mat)
             (material-blue mat)))])

(define (create-material
         name
         type
         #:gray [gray 0.3]
         #:red [red gray]
         #:green [green gray]
         #:blue [blue gray])
  (material name type red green blue))


(struct extended-material material
  (specularity
   roughness)
  #:methods gen:radiance
  [(define (radiance-string mat)
     (format "void ~A ~A~%0~%0~%5 ~A ~A ~A ~A ~A~%"
             (material-type mat)
             (material-name mat)
             (material-red mat)
             (material-green mat)
             (material-blue mat)
             (extended-material-specularity mat)
             (extended-material-roughness mat)))])

(define (create-extended-material
         name
         type
         #:gray [gray 0.3]
         #:red [red gray]
         #:green [green gray]
         #:blue [blue gray]
         #:specularity [specularity 0]
         #:roughness [roughness 0])
  (extended-material name type red green blue specularity roughness))

(define (create-plastic-material name
                                 #:gray [gray 0.3]
                                 #:red [red gray]
                                 #:green [green gray]
                                 #:blue [blue gray]
                                 #:specularity [specularity 0]
                                 #:roughness [roughness 0])
  (create-extended-material name
                            "plastic"
                            #:red red
                            #:green green
                            #:blue blue
                            #:specularity specularity
                            #:roughness roughness))

(define (create-metal-material name
                                 #:gray [gray 0.3]
                                 #:red [red gray]
                                 #:green [green gray]
                                 #:blue [blue gray]
                                 #:specularity [specularity 0]
                                 #:roughness [roughness 0])
  (create-extended-material name
                            "metal"
                            #:red red
                            #:green green
                            #:blue blue
                            #:specularity specularity
                            #:roughness roughness))

(define (create-glass-material name
                               #:gray [gray 0.3]
                               #:red [red gray]
                               #:green [green gray]
                               #:blue [blue gray])
  (create-material name
                   "glass"
                   #:red red
                   #:green green
                   #:blue blue))


(struct trans-material extended-material
  (transmissivity
   transmitted-specular)
  #:methods gen:radiance
  [(define (radiance-string mat)
     (format "void ~A ~A~%0~%0~%7 ~A ~A ~A ~A ~A ~A ~A~%"
             (material-type mat)
             (material-name mat)
             (material-red mat)
             (material-green mat)
             (material-blue mat)
             (extended-material-specularity mat)
             (extended-material-roughness mat)
             (trans-material-transmissivity mat)
             (trans-material-transmitted-specular mat)))])

(define (create-trans-material
         name
         type
         #:gray [gray 0.3]
         #:red [red gray]
         #:green [green gray]
         #:blue [blue gray]
         #:specularity [specularity 0]
         #:roughness [roughness 0]
         #:transmissivity [transmissivity 0]
         #:transmitted-specular [transmitted-specular 0])
  (trans-material name type red green blue specularity roughness transmissivity transmitted-specular))


;;Some pre-defined materials
(define material-white (create-plastic-material "white" #:gray 1.0))
(define generic-ceiling-70 (create-plastic-material "GenericCeiling_70" #:gray 0.7))
(define generic-ceiling-80 (create-plastic-material "GenericCeiling_80" #:gray 0.8))
(define generic-ceiling-90 (create-plastic-material "HighReflectanceCeiling_90" #:gray 0.9))
(define generic-floor-20 (create-plastic-material "GenericFloor_20" #:gray 0.2))
(define generic-interior-wall-50 (create-plastic-material "GenericInteriorWall_50" #:gray 0.5))
(define generic-interior-wall-70 (create-plastic-material "GenericInteriorWall_70" #:gray 0.7))
(define generic-furniture-50 (create-plastic-material "GenericFurniture_50" #:gray 0.5))
(define outside-facade-30 (create-plastic-material "OutsideFacade_30" #:gray 0.30))
(define outside-facade-35 (create-plastic-material "OutsideFacade_35" #:gray 0.35))
(define generic-glass-80 (create-glass-material "Glass_80" #:gray 0.8))
(define generic-metal (create-metal-material "SheetMetal_80" #:gray 0.8))
