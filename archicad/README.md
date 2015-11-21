# ArchiGD
ArchiCAD Generative Design Tool

1. Installation

It should install automatically. Although some errors might occur. Here are some possible solutions to those:

	1.1.	Access Denied: If this happens, try to find your Racket folder, normally C:\Program Files\Racket and find the DrRacket.exe, right-click it, and run as administrator.

		1.1.1	Once you have your file open, write (require archigd), and try to  run the file. Now it should be able to the necessary copy of files needed for the installation.
		
		1.1.2	Finally, to avoid the need to always run DrRacket.exe as an administrator, write (set! do-not-install #t) on your files that use (require archigd). 
				Otherwise you can go to the file itself, and change the line (define do-not-install #f) to (define do-not-install #t), this will avoid the (set! do-not-install #t) on your programs.  	

	1.2.	The system cannot find the path specified: if this happens, the file could not locate your ArchiCAD folder.
		
		1.2.1	You need to find the file install.rkt, that is in the path C:\Users\[Name]\AppData\Roaming\Racket\[Version]\pkgs\archigd\archigd
		[Name] = name of your computer
		[Version] = version of your DrRacket
		Once you open the file, just change the following line 
		(define to-folder (string->some-system-path "C:\\Program Files\\GRAPHISOFT\\ArchiCAD 18\\Add-ons" 'windows)) 
		to one that uses your ArchiCAD installation folder path, i.e., change the string  "C:\\Program Files\\GRAPHISOFT\\ArchiCAD 18\\Add-ons" to the correct path.



