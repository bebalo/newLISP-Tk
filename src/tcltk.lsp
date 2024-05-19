#!/usr/bin/env newlisp
;; Time-stamp: <2024-05-19 20:52:34 paul>
;; demo how to write Tcl/Tk GUIs controlled from newLISP

;;  a Tcl/Tk installation is required
;;  the example has been tested on Mac OS X and Unix

(set (global 'cl) "")

(context 'tst)
;; setup communications
(map set '(myin tcout) (pipe))
(map set '(tcin myout) (pipe))
(println "wait ...")
(process "/usr/bin/wish" tcin tcout)

(context 'MAIN)
;; make GUI
(write
 tst:myout 
[text]
wm geometry . 250x90
wm title . "Tcl/Tk and newLISP"

button .one -text {red}
button .two -text {green}
button .three -text {blue}
label .colorlabel -width 25

grid .one .two .three -padx 8 -row 0
grid .colorlabel -column 0 -columnspan 3 -pady 6

.one config -command {puts {(call-back "red")}}
.two config -command {puts {(call-back "green")}}
.three config -command {puts {(call-back "blue")}}

bind . <Destroy> {puts {(exit)}}
[/text]
 );write-buffer

;;event handler (command)
(define (call-back color)
      (write-line tst:myout
                  (append ".colorlabel config -background " color))
      );call-back

;; run event loop
(while (read-line tst:myin)                 ;Programm wartet
   (setq cl (current-line))
   (eval-string cl);<========================
   )

