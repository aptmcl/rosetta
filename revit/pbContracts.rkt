#lang racket/base
;; Generated using protoc-gen-racket v1.1
(require "rosetta/protobuf1/syntax.rkt")

(define-message-type namestrc ((required primitive:string name 1)))
(define-message-type
 boxstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required primitive:double p2coordx 7)
  (required primitive:double p2coordy 8)
  (required primitive:double p2coordz 9)
  (required primitive:double p3coordx 10)
  (required primitive:double p3coordy 11)
  (required primitive:double p3coordz 12)
  (required primitive:double height 13)))
(define-message-type
 cylinderstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double radius 4)
  (required primitive:double height 5)))
(define-message-type
 cylinderbstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double radius 4)
  (required primitive:double p1coordx 5)
  (required primitive:double p1coordy 6)
  (required primitive:double p1coordz 7)))
(define-message-type
 spherestrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required primitive:double p2coordx 7)
  (required primitive:double p2coordy 8)
  (required primitive:double p2coordz 9)))
(define-message-type
 familyelementstrc
 ((required struct:idstrc familyid 1)
  (required primitive:bool flag 2)
  (repeated primitive:string names 3)
  (repeated primitive:double values 4)))
(define-message-type
 wallstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required struct:idstrc level 7)))
(define-message-type
 wallheightstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required primitive:double height 7)
  (required struct:idstrc level 8)))
(define-message-type
 walllevelstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required struct:idstrc levelb 7)
  (required struct:idstrc levelt 8)))
(define-message-type
 polywallstrc
 ((repeated primitive:double pts 1)
  (required struct:idstrc levelb 2)
  (required struct:idstrc levelt 3)
  (required struct:idstrc familyid 4)))
(define-message-type
 curtainwallstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required primitive:double p2coordx 7)
  (required primitive:double p2coordy 8)
  (required primitive:double p2coordz 9)
  (required primitive:double p3coordx 10)
  (required primitive:double p3coordy 11)
  (required primitive:double p3coordz 12)
  (required struct:idstrc level 13)))
(define-message-type
 wallinfostrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required primitive:double baseelevation 7)
  (required primitive:string baselevelname 8)
  (required primitive:double topelevation 9)
  (required primitive:string toplevelname 10)
  (required primitive:string walltype 11)))
(define-message-type
 beaminfostrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required struct:idstrc family 7)))
(define-message-type polywallinfostrc ((repeated struct:wallinfostrc walls 1)))
(define-message-type idstrc ((required primitive:int32 id 1)))
(define-message-type doublestrc ((required primitive:double height 1)))
(define-message-type boolstrc ((required primitive:bool answer 1)))
(define-message-type doublevolumestrc ((required primitive:double volume 1)))
(define-message-type polyidstrc ((repeated struct:idstrc ids 1)))
(define-message-type polylevelstrc ((repeated struct:levelstrc levels 1)))
(define-message-type
 levelstrc
 ((required primitive:double h 1) (required primitive:string name 2)))
(define-message-type
 upperlevelstrc
 ((required struct:idstrc current 1) (required primitive:double elevation 2)))
(define-message-type
 insertdoorstrc
 ((required primitive:int32 hostid 1)
  (required primitive:double p0coordx 2)
  (required primitive:double p0coordy 3)
  (required primitive:double p0coordz 4)
  (required struct:idstrc family 5)))
(define-message-type
 insertdoorbstrc
 ((required primitive:int32 hostid 1)
  (required primitive:double deltax 2)
  (required primitive:double deltay 3)
  (required struct:idstrc family 4)))
(define-message-type
 insertwindowstrc
 ((required primitive:int32 hostid 1)
  (required primitive:double deltax 2)
  (required primitive:double deltay 3)))
(define-message-type
 roundfloorstrc
 ((required primitive:double radius 1)
  (required primitive:double center-x 2)
  (required primitive:double center-y 3)
  (required primitive:double center-z 4)
  (required struct:idstrc level 5)))
(define-message-type
 floorstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required struct:idstrc level 7)))
(define-message-type
 columnstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required struct:idstrc baselevel 4)
  (required struct:idstrc toplevel 5)
  (required primitive:double width 6)
  (required struct:idstrc familyid 7)))
(define-message-type
 flooropeningstrc
 ((required primitive:double p0coordx 1)
  (required primitive:double p0coordy 2)
  (required primitive:double p0coordz 3)
  (required primitive:double p1coordx 4)
  (required primitive:double p1coordy 5)
  (required primitive:double p1coordz 6)
  (required primitive:int32 floorid 7)))
(define-message-type
 polylinefloorstrc
 ((required struct:idstrc floor 1)
  (repeated primitive:double points 2)
  (required struct:idstrc familyid 3)))
(define-message-type linestrc ((repeated primitive:double points 1)))
(define-message-type matrixstrc ((repeated struct:linestrc lines 1)))
(define-message-type
 intersectstrc
 ((required struct:idstrc wall-id 1) (required struct:idstrc floor-id 2)))
(define-message-type
 stairrunstrc
 ((required struct:idstrc bottom-level 1)
  (required struct:idstrc top-level 2)
  (required primitive:double bottomp0coordx 3)
  (required primitive:double bottomp0coordy 4)
  (required primitive:double bottomp0coordz 5)
  (required primitive:double bottomp1coordx 6)
  (required primitive:double bottomp1coordy 7)
  (required primitive:double bottomp1coordz 8)
  (required primitive:double topp0coordx 9)
  (required primitive:double topp0coordy 10)
  (required primitive:double topp0coordz 11)
  (required primitive:double topp1coordx 12)
  (required primitive:double topp1coordy 13)
  (required primitive:double topp1coordz 14)))
(define-message-type
 stairstrc
 ((required struct:idstrc bottom-level 1)
  (required struct:idstrc top-level 2)
  (required primitive:double bottomp0coordx 3)
  (required primitive:double bottomp0coordy 4)
  (required primitive:double bottomp0coordz 5)
  (required primitive:double topp0coordx 6)
  (required primitive:double topp0coordy 7)
  (required primitive:double topp0coordz 8)))
(define-message-type
 landingstrc
 ((required primitive:double bottomleftcornerx 1)
  (required primitive:double bottomleftcornery 2)
  (required primitive:double bottomleftcornerz 3)
  (required primitive:double topleftcornerx 4)
  (required primitive:double topleftcornery 5)
  (required primitive:double topleftcornerz 6)
  (required primitive:double bottomrightcornerx 7)
  (required primitive:double bottomrightcornery 8)
  (required primitive:double bottomrightcornerz 9)
  (required primitive:double toprightcornerx 10)
  (required primitive:double toprightcornery 11)
  (required primitive:double toprightcornerz 12)
  (required struct:idstrc stairsrunid 13)))
(define-message-type
 slabwallstrc
 ((required primitive:double bottomleftcornerx 1)
  (required primitive:double bottomleftcornery 2)
  (required primitive:double bottomleftcornerz 3)
  (required primitive:double topleftcornerx 4)
  (required primitive:double topleftcornery 5)
  (required primitive:double topleftcornerz 6)
  (required primitive:double bottomrightcornerx 7)
  (required primitive:double bottomrightcornery 8)
  (required primitive:double bottomrightcornerz 9)
  (required primitive:double toprightcornerx 10)
  (required primitive:double toprightcornery 11)
  (required primitive:double toprightcornerz 12)
  (required struct:idstrc level-id 13)))
(define-message-type
 masswallstrc
 ((required primitive:double bottomleftcornerx 1)
  (required primitive:double bottomleftcornery 2)
  (required primitive:double bottomleftcornerz 3)
  (required primitive:double topleftcornerx 4)
  (required primitive:double topleftcornery 5)
  (required primitive:double topleftcornerz 6)
  (required primitive:double bottomrightcornerx 7)
  (required primitive:double bottomrightcornery 8)
  (required primitive:double bottomrightcornerz 9)
  (required primitive:double toprightcornerx 10)
  (required primitive:double toprightcornery 11)
  (required primitive:double toprightcornerz 12)
  (required primitive:double height 13)
  (required struct:idstrc level-id 14)))
(define-message-type
 masssweepstrc
 ((repeated primitive:double profile1 1)
  (repeated primitive:double path 2)
  (repeated primitive:double profile2 3)))
(define-message-type
 wallsfromslabsstrc
 ((required struct:idstrc slabid 1)
  (required struct:idstrc blevel 2)
  (required primitive:double height 3)))
(define-message-type
 holeslabstrc
 ((required struct:idstrc slabid 1) (repeated primitive:double pts 2)))
(define-message-type railingsstrc ((required struct:idstrc slabid 1)))
(define-message-type toposurfacestrc ((repeated primitive:double pts 1)))
(define-message-type
 buildingpadstrc
 ((repeated primitive:double pts 1) (required struct:idstrc level-id 2)))
(define-message-type
 extrusionstrc
 ((repeated primitive:double pts 1) (required primitive:double elevation 2)))
(define-message-type
 movestrc
 ((required struct:idstrc element 1)
  (required primitive:double vectorx 2)
  (required primitive:double vectory 3)
  (required primitive:double vectorz 4)))
(define-message-type
 rotatestrc
 ((required struct:idstrc element 1)
  (required primitive:double angle 2)
  (required primitive:double p0x 3)
  (required primitive:double p0y 4)
  (required primitive:double p0z 5)
  (required primitive:double p1x 6)
  (required primitive:double p1y 7)
  (required primitive:double p1z 8)))

(provide (all-defined-out))
