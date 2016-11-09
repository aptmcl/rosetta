#lang racket/base
;; Generated using protoc-gen-racket v1.1
(require "protobuf1/syntax.rkt")

(define-message-type namemessage ((required primitive:string name 1)))
(define-message-type
 elementid
 ((required primitive:string guid 1)
  (required primitive:bool crashmaterial 2)))
(define-message-type
 elementidlist
 ((repeated primitive:string guid 1)
  (required primitive:bool crashmaterial 2)))
(define-message-type
 pointmessage
 ((required primitive:double p0x 1) (required primitive:double p0y 2)))
(define-message-type
 polyarcmessage
 ((required primitive:int32 begindex 1)
  (required primitive:int32 endindex 2)
  (required primitive:double arcangle 3)))
(define-message-type doublemessage ((required primitive:double d 1)))
(define-message-type intlistmsg ((repeated primitive:int32 ilist 1)))
(define-message-type repeateddoublemsg ((repeated primitive:double dlist 1)))
(define-message-type
 additionalparams
 ((repeated primitive:string names 1)
  (repeated primitive:double doubles 2)
  (repeated primitive:string strings 3)
  (repeated primitive:int32 integers 4)
  (repeated primitive:bool booleans 5)
  (repeated primitive:string paramtype 6)
  (repeated struct:intarray intarrays 7)
  (repeated struct:doublearray doublearrays 8)
  (repeated struct:boolarray boolarrays 9)
  (repeated struct:stringarray stringarrays 10)
  (repeated primitive:bool isarray 11)))
(define-message-type
 rooflevelsmsg
 ((repeated primitive:double angle 1) (repeated primitive:double height 2)))
(define-message-type
 transmatmessage
 ((required primitive:double p0 1)
  (required primitive:double p1 2)
  (required primitive:double p2 3)
  (required primitive:double p3 4)
  (required primitive:double p4 5)
  (required primitive:double p5 6)
  (required primitive:double p6 7)
  (required primitive:double p7 8)
  (required primitive:double p8 9)
  (required primitive:double p9 10)
  (required primitive:double p10 11)
  (required primitive:double p11 12)))
(define-message-type
 wallmsg
 ((required primitive:int32 bottomindex 1)
  (required primitive:double thickness 2)
  (optional struct:polyarcsmessage arcs 3)
  (required primitive:int32 upperindex 4)
  (required primitive:string material 5)
  (required primitive:string type 6)
  (required primitive:string referenceline 7)
  (required primitive:double alphaangle 8)
  (required primitive:double betaangle 9)
  (required primitive:string typeprofile 10)
  (optional struct:pointsmessage pts 11)
  (optional primitive:string profilename 12)
  (optional primitive:double height 13)
  (optional primitive:bool flipped 14)
  (optional primitive:double bottomoffset 15)
  (optional primitive:string layer 16)
  (repeated struct:windowmessage windows 17)
  (repeated primitive:double windoworder 18)
  (required primitive:double refoffset 19)
  (required primitive:string refmat 20)
  (required primitive:string oppmat 21)
  (required primitive:string sidmat 22)
  (optional primitive:bool toplinked 23)))
(define-message-type
 getwallmsg
 ((repeated struct:wallmsg walls 1) (repeated primitive:string guid 2)))
(define-message-type
 wallmessage
 ((required primitive:double p0x 1)
  (required primitive:double p0y 2)
  (required primitive:double p1x 3)
  (required primitive:double p1y 4)
  (required primitive:double height 5)
  (required primitive:double thickness 6)
  (required primitive:double angle 7)
  (required primitive:double bottom 8)
  (required primitive:string material 9)
  (required primitive:string type 10)
  (required primitive:string referenceline 11)
  (optional primitive:int32 bottomstory 12)
  (optional primitive:int32 topstory 13)))
(define-message-type
 doormessage
 ((required primitive:string guid 1)
  (required primitive:double objloc 2)
  (required primitive:double zpos 3)
  (required primitive:double height 4)
  (required primitive:double width 5)
  (required primitive:bool hole 6)
  (required primitive:string name 7)
  (required primitive:bool flipx 8)
  (required primitive:bool flipy 9)
  (required struct:additionalparams params 10)
  (required primitive:double depthoffset 11)
  (optional primitive:string layer 12)))
(define-message-type
 windowmessage
 ((optional primitive:string guid 1)
  (required primitive:double objloc 2)
  (required primitive:double zpos 3)
  (required primitive:string name 4)
  (optional struct:additionalparams params 5)
  (required primitive:double width 6)
  (required primitive:double height 7)
  (required primitive:double depthoffset 8)
  (required primitive:bool flipx 9)
  (required primitive:bool flipy 10)
  (optional primitive:string layer 11)))
(define-message-type
 circlemessage
 ((required primitive:double p0x 1)
  (required primitive:double p0y 2)
  (required primitive:double radius 3)))
(define-message-type
 arcmessage
 ((required primitive:double p0x 1)
  (required primitive:double p0y 2)
  (required primitive:double radius 3)
  (required primitive:double angle 4)
  (required primitive:double begang 5)
  (required primitive:double endang 6)))
(define-message-type
 spheremessage
 ((required primitive:double c0x 1)
  (required primitive:double c0y 2)
  (required primitive:double c0z 3)
  (required primitive:double radius 4)
  (required primitive:int32 level 5)))
(define-message-type
 cylindermsg
 ((required primitive:double p0x 1)
  (required primitive:double p0y 2)
  (required primitive:double p0z 3)
  (required primitive:double p1x 4)
  (required primitive:double p1y 5)
  (required primitive:double p1z 6)
  (required primitive:double radius 7)
  (required primitive:int32 level 8)))
(define-message-type
 shellcomplexmessage
 ((required primitive:int32 numpoints 1)
  (required primitive:int32 numarcs 2)
  (required primitive:int32 numholes 3)
  (required primitive:int32 numhpoints 4)
  (required primitive:int32 numharcs 5)
  (required primitive:double holeheight 6)
  (required primitive:double reflectx 7)
  (required primitive:double reflecty 8)))
(define-message-type
 shellsimplemessage
 ((required primitive:int32 numpoints 1)))
(define-message-type
 shellmessage
 ((required primitive:int32 numpoints 1) (required primitive:int32 numarcs 2)))
(define-message-type
 pointsmessage
 ((repeated primitive:double px 1)
  (repeated primitive:double py 2)
  (repeated primitive:double pz 3)))
(define-message-type
 polyarcsmessage
 ((repeated primitive:int32 begindex 1)
  (repeated primitive:int32 endindex 2)
  (repeated primitive:double arcangle 3)))
(define-message-type
 rotshellmessage
 ((required primitive:string axis 1)
  (required primitive:double angle 2)
  (required primitive:string guid 3)))
(define-message-type
 tshellmessage
 ((required primitive:double tx 1)
  (required primitive:double ty 2)
  (required primitive:double tz 3)
  (required primitive:string guid 4)))
(define-message-type
 oldholemessage
 ((required primitive:double height 1) (required primitive:string guid 2)))
(define-message-type
 curtainwallmsg
 ((required struct:pointsmessage pts 1)
  (required struct:polyarcsmessage arcs 2)
  (required primitive:int32 bottomindex 3)
  (required primitive:int32 upperindex 4)
  (repeated primitive:double primaries 5)
  (repeated primitive:double secondaries 6)
  (repeated primitive:bool mainpanels 7)
  (required primitive:string panelmaterial 8)
  (required primitive:string secpanelmaterial 9)
  (required primitive:string verticalframematerial 10)
  (required primitive:string horizontalframematerial 11)
  (required primitive:string framematerial 12)
  (required primitive:double panelsangle 13)
  (required primitive:double offset 14)
  (optional primitive:string layer 15)
  (optional primitive:double height 16)
  (optional primitive:bool toplinked 17)
  (optional primitive:double mainpanelthickness 18)
  (optional primitive:double secondarypanelthickness 19)
  (optional primitive:double bframewidth 20)
  (optional primitive:double bframedepth 21)
  (optional primitive:double bframeoffset 22)
  (optional primitive:double mframewidth 23)
  (optional primitive:double mframedepth 24)
  (optional primitive:double mframeoffset 25)
  (optional primitive:double tframewidth 26)
  (optional primitive:double tframedepth 27)
  (optional primitive:double tframeoffset 28)))
(define-message-type
 translatemsg
 ((required primitive:double tx 1)
  (required primitive:double ty 2)
  (required primitive:double tz 3)
  (required primitive:string guid 4)))
(define-message-type
 slabmessage
 ((required primitive:double level 1)
  (required primitive:string material 2)
  (required primitive:double thickness 3)
  (required primitive:string type 4)
  (required primitive:int32 bottomlevel 5)
  (repeated primitive:int32 subpolygons 6)
  (required struct:pointsmessage pts 7)
  (required struct:polyarcsmessage parcs 8)
  (optional primitive:string layer 9)
  (optional primitive:string reference 10)))
(define-message-type
 meshmessage
 ((required primitive:double level 1)
  (required primitive:string material 2)
  (required primitive:int32 bottomlevel 3)
  (optional primitive:string overridematerial 4)
  (optional primitive:string layer 5)))
(define-message-type
 rotatemsg
 ((repeated primitive:string guid 1)
  (required primitive:string axis 2)
  (required primitive:double angle 3)
  (required primitive:bool copy 4)))
(define-message-type
 trimmsg
 ((repeated primitive:string guids 1) (repeated primitive:string guids2 2)))
(define-message-type
 intersectmsg
 ((required primitive:string guid1 1) (required primitive:string guid2 2)))
(define-message-type
 columnmsg
 ((required primitive:double posx 1)
  (required primitive:double posy 2)
  (required primitive:double bottom 3)
  (optional primitive:double height 4)
  (required primitive:bool circlebased 5)
  (required primitive:double angle 6)
  (required primitive:double depth 7)
  (required primitive:double width 8)
  (optional primitive:int32 bottomindex 9)
  (optional primitive:int32 upperindex 10)
  (required primitive:double slantangle 11)
  (required primitive:double slantdirection 12)
  (required primitive:string profilename 13)
  (optional primitive:double bottomoffset 14)
  (optional primitive:string layer 15)
  (optional primitive:bool toplinked 16)))
(define-message-type
 storymsg
 ((required primitive:double height 1) (required primitive:string name 2)))
(define-message-type
 storyinfo
 ((required primitive:bool exists 1)
  (required primitive:int32 index 2)
  (required primitive:double level 3)
  (required primitive:string name 4)))
(define-message-type
 upperlevelmsg
 ((required primitive:int32 index 1) (required primitive:double height 2)))
(define-message-type
 wallsfromslab
 ((required primitive:string guid 1)
  (required primitive:double height 2)
  (required primitive:string material 3)
  (required primitive:string type 4)
  (required primitive:string referenceline 5)
  (required primitive:double thickness 6)
  (optional primitive:string layer 7)))
(define-message-type
 columnsfromslab
 ((required primitive:string guid 1)
  (required primitive:double height 2)
  (required primitive:string material 3)
  (required primitive:double depth 4)
  (required primitive:double width 5)
  (required primitive:bool circlebased 6)
  (optional primitive:string layer 7)))
(define-message-type
 stairsmsg
 ((required primitive:string name 1)
  (required primitive:double posx 2)
  (required primitive:double posy 3)
  (required primitive:double xratio 4)
  (required primitive:double yratio 5)
  (required primitive:double bottom 6)
  (required primitive:double angle 7)
  (required primitive:int32 bottomindex 8)
  (required primitive:bool usexyfixsize 9)
  (required struct:additionalparams params 10)
  (optional primitive:string layer 11)
  (optional primitive:double height 12)
  (optional primitive:int32 upperindex 13)
  (optional primitive:bool toplinked 14)))
(define-message-type
 roofmsg
 ((required primitive:double height 1)
  (required primitive:string material 2)
  (required primitive:double thickness 3)
  (required primitive:string type 4)
  (required primitive:int32 bottomlevel 5)
  (optional primitive:string layer 6)))
(define-message-type
 holemsg
 ((required primitive:string guid 1)
  (optional struct:pointsmessage pts 2)
  (optional struct:polyarcsmessage arcs 3)
  (optional primitive:int32 type 4)))
(define-message-type
 mirrormsg
 ((required primitive:string guid 1)
  (required primitive:string axis 2)
  (required primitive:bool copy 3)))
(define-message-type
 morphmsg
 ((required primitive:double refx 1)
  (required primitive:double refy 2)
  (required primitive:double refz 3)
  (optional struct:pointsmessage pts 4)
  (optional struct:pointsmessage edges 5)
  (optional struct:pointsmessage polygons 6)
  (optional struct:intlistmsg sizespolygons 7)
  (required primitive:string material 8)
  (required primitive:int32 level 9)))
(define-message-type
 boxmsg
 ((required primitive:double x1 1)
  (required primitive:double y1 2)
  (required primitive:double z1 3)
  (required primitive:double x2 4)
  (required primitive:double y2 5)
  (required primitive:double z2 6)
  (required primitive:int32 bottomlevel 7)))
(define-message-type
 wallinfo
 ((repeated primitive:double thickness 1)
  (repeated primitive:double length 2)
  (repeated primitive:double height 3)
  (repeated primitive:double volume 4)))
(define-message-type
 wallrepeated
 ((repeated primitive:double p0x 1)
  (repeated primitive:double p0y 2)
  (repeated primitive:double p1x 3)
  (repeated primitive:double p1y 4)
  (repeated struct:storyinfo bottomlevel 5)
  (repeated primitive:double thickness 6)
  (repeated primitive:double angle 7)
  (repeated struct:storyinfo toplevel 8)
  (repeated primitive:string material 9)
  (repeated primitive:string type 10)
  (repeated primitive:string referenceline 11)
  (repeated primitive:double alphaangle 12)
  (repeated primitive:double betaangle 13)
  (repeated primitive:string typeprofile 14)
  (repeated primitive:string guid 15)))
(define-message-type
 slabrepeated
 ((repeated struct:pointsmessage points 1)
  (repeated struct:storyinfo bottomlevel 2)
  (repeated primitive:double thickness 3)
  (repeated primitive:string material 4)
  (repeated primitive:string type 5)
  (repeated primitive:string guid 6)
  (repeated struct:intlistmsg subpolygons 7)))
(define-message-type
 columnrepeated
 ((repeated primitive:double px 1)
  (repeated primitive:double py 2)
  (repeated struct:storyinfo bottomlevel 3)
  (repeated struct:storyinfo toplevel 4)
  (repeated primitive:bool circular 5)
  (repeated primitive:double angle 6)
  (repeated primitive:double depth 7)
  (repeated primitive:double width 8)
  (repeated primitive:double slantangle 9)
  (repeated primitive:double slantdirection 10)
  (repeated primitive:string guid 11)))
(define-message-type
 objectrepeated
 ((repeated primitive:string name 1)
  (repeated primitive:double px 2)
  (repeated primitive:double py 3)
  (repeated primitive:double angle 4)
  (repeated primitive:double xratio 5)
  (repeated primitive:double yratio 6)
  (repeated primitive:double bottomoffset 7)
  (repeated struct:storyinfo bottomlevel 8)
  (repeated primitive:string guid 9)
  (repeated primitive:bool stairs 10)
  (repeated primitive:bool usexyfixsize 11)))
(define-message-type
 roofrepeated
 ((repeated struct:pointsmessage points 1)
  (repeated struct:storyinfo bottomlevel 2)
  (repeated primitive:double thickness 3)
  (repeated primitive:string material 4)
  (repeated primitive:string type 5)
  (repeated primitive:string guid 6)
  (repeated struct:intlistmsg subpolygons 7)
  (repeated primitive:double height 8)))
(define-message-type levelrepeated ((repeated struct:storyinfo levels 1)))
(define-message-type
 extrusionmsg
 ((required primitive:double vx 1)
  (required primitive:double vy 2)
  (required primitive:double vz 3)
  (required primitive:double height 4)))
(define-message-type
 openmessage
 ((required primitive:string path 1) (required primitive:string extension 2)))
(define-message-type
 transformmsg
 ((required primitive:string guid 1)
  (required primitive:string op 2)
  (required primitive:double x 3)
  (required primitive:double y 4)
  (required primitive:double z 5)
  (required primitive:double angle 6)
  (required primitive:double scale 7)))
(define-message-type
 applymatrix
 ((required primitive:string guid 1) (repeated primitive:double matrix 2)))
(define-message-type
 beammsg
 ((required primitive:double x0 1)
  (required primitive:double y0 2)
  (required primitive:double x1 3)
  (required primitive:double y1 4)
  (required primitive:double beamheight 5)
  (required primitive:double beamwidth 6)
  (required primitive:double levelheight 7)
  (required primitive:int32 bottomlevel 8)
  (required primitive:double angle 9)
  (required primitive:string material 10)
  (required primitive:string profilename 11)
  (optional primitive:string layer 12)))
(define-message-type intarray ((repeated primitive:int32 lst 1)))
(define-message-type doublearray ((repeated primitive:double lst 1)))
(define-message-type boolarray ((repeated primitive:bool lst 1)))
(define-message-type stringarray ((repeated primitive:string lst 1)))
(define-message-type
 objectmsg
 ((required primitive:int32 index 1)
  (required primitive:double posx 2)
  (required primitive:double posy 3)
  (required primitive:bool usexyfixsize 4)
  (required primitive:double xratio 5)
  (required primitive:double yratio 6)
  (required primitive:bool useobjsectattrs 7)
  (required primitive:double bottom 8)
  (required primitive:double angle 9)
  (required struct:additionalparams params 10)
  (optional primitive:string name 11)
  (required primitive:int32 level 12)
  (optional primitive:string layer 13)))
(define-message-type
 libpartmsg
 ((required primitive:string name 1)
  (required primitive:string twocode 2)
  (required primitive:string threecode 3)
  (required primitive:string type 4)
  (required primitive:string parentid 5)
  (required primitive:string mastercode 6)
  (required primitive:string parametercode 7)
  (repeated primitive:string names 8)
  (repeated primitive:double doubles 9)
  (repeated primitive:string strings 10)
  (repeated primitive:int32 integers 11)
  (repeated primitive:bool booleans 12)
  (repeated primitive:string paramtype 13)
  (repeated struct:intarray intarrays 14)
  (repeated struct:doublearray doublearrays 15)
  (repeated struct:boolarray boolarrays 16)
  (repeated struct:stringarray stringarrays 17)
  (repeated primitive:bool isarray 18)))
(define-message-type
 profilemsg
 ((required struct:pointsmessage pts 1)
  (required struct:polyarcsmessage arcs 2)
  (required primitive:string material 3)
  (required primitive:string name 4)))
(define-message-type
 layermsg
 ((required primitive:string name 1) (optional primitive:int32 connection 2)))
(define-message-type
 layerelementmsg
 ((required primitive:string guid 1) (required primitive:string layer 2)))
(define-message-type
 splinemsg
 ((required struct:pointsmessage points 1) (required primitive:bool closed 2)))
(define-message-type linemsg ((optional struct:pointsmessage pts 1)))
(define-message-type
 polylinemsg
 ((optional struct:pointsmessage pts 1)
  (optional struct:polyarcsmessage arcs 2)))
(define-message-type
 getlinesmsg
 ((repeated struct:linemsg lines 1) (repeated primitive:string guids 2)))
(define-message-type
 getpolylinesmsg
 ((repeated struct:polylinemsg polylines 1)
  (repeated primitive:string guids 2)))
(define-message-type
 revshellmsg
 ((optional struct:pointsmessage pts 1)
  (optional struct:polyarcsmessage arcs 2)
  (required primitive:int32 level 3)
  (required primitive:bool flipped 4)
  (required primitive:double slantangle 5)
  (required primitive:double revangle 6)
  (required primitive:double distortionangle 7)
  (required primitive:double begangle 8)
  (repeated primitive:double axis 9)
  (required primitive:string material 10)
  (required primitive:string type 11)
  (required primitive:double thickness 12)
  (required primitive:double height 13)))
(define-message-type
 extshellmsg
 ((optional struct:pointsmessage pts 1)
  (optional struct:polyarcsmessage arcs 2)
  (required primitive:int32 level 3)
  (required primitive:bool flipped 4)
  (required primitive:double cextx 5)
  (required primitive:double cexty 6)
  (required primitive:double extx 7)
  (required primitive:double exty 8)
  (required primitive:double extz 9)
  (repeated primitive:bool visible 10)
  (required primitive:string material 11)
  (required primitive:string type 12)
  (required primitive:double thickness 13)))
(define-message-type rendermsg ((required primitive:string file 1)))
(define-message-type
 cameramsg
 ((required primitive:double cx 1)
  (required primitive:double cy 2)
  (required primitive:double cz 3)
  (required primitive:double tx 4)
  (required primitive:double ty 5)
  (required primitive:double tz 6)
  (required primitive:double lens 7)
  (required primitive:double sunazimuth 8)
  (required primitive:double sunaltitude 9)))

(provide (all-defined-out))
