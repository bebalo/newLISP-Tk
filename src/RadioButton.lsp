#!/usr/bin/env newlisp
## Time-stamp: <2024-06-05 18:21:59 paul>

;; RadioButtons

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit));end

(define (MAIN:nop)
   (begin)
   ;; (ts:setw (Button (Name "btn-nop") (Text "Do nothing") ))
   ;; (:build btn-nop)
   ;; ;; place button at the bottom row
   ;; (:setgrid btn-nop (Row 0) (Column 0) (Padx 5) (Pady 5))
)

(define (MAIN:nop1)
   (begin)
   ;; (ts:setw (Button (Name "btn-end") (Text "Do nothing") ))
   ;; (:build btn-end)
   ;; ;; place button at the bottom row
   ;; (:setgrid btn-end (Row 0) (Column 0) (Padx 5) (Pady 5))
             );nop1


(define (MAIN:nop2)
   (begin)
   ;; (ts:setw (Button (Name "btn-end") (Text "Do nothing") ))
   ;; (:build btn-end)
   ;; ;; place button at the bottom row
   ;; (:setgrid btn-end (Row 0) (Column 0) (Padx 5) (Pady 5))
             );nop2


## -----------------------------------------------------------------------

(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

## -----------------------------------------------------------------------
;; create frame for toolbar box
(ts:setw (Frame (Name "box")))          ;toolbar frame
(:build box)
;; (:setgrid box (Sticky "news"))
(:setgrid box (Sticky "n"))


## ab Row #1
(ts:setw (Notebook (Name "nb")) (Parent "box")
         )
(:build nb)
(:setgrid nb (Row 1) (Column 0) (Columnspan 20)
          (Sticky "news")
          )

## ------------- ADD 2 FRAMES --------------------------------------------
(ts:setw (Frame (Name "fr1") (Parent "nb")  (Width 200) (Height 200)))
(:build fr1)
(:add-frame nb fr1 "Label 1") 

(ts:setw (Frame (Name "fr2") (Parent "nb") ))
(:build fr2)
(:add-frame nb fr2 "Label 2")

;; One creates one or more buttons, and ties them together by supplying a variable name via the
;; -variable argument. Each button with the same variable toggles on or off. For the programmer
;; to know which button is selected, they provide a unique string to the -value argument. That
;; value is assigned to the variable when the button has been selected.

(ts:setw (Radiobutton (Parent "fr1") (Name "rbtn1") (Text "Zuletzt bearbeitet")
                      (Variable "Farbe") (Value "zuletzt")
                      (Command "nop1")))
(:build rbtn1)
(:setgrid rbtn1 (Row 0) (Column 0) (Padx 5))      ;make it visible

(ts:setw (Radiobutton (Parent "fr1") (Name "rbtn2") (Text "VolltextSuche")
                      (Variable "Farbe") (Value "vtsuche")
                      (Command "nop2")))
(:build rbtn2)
(:setgrid rbtn2 (Row 0) (Column 1) (Padx 5))      ;make it visible



## -----------------------------------------------------------------------
(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 3) (Column 0) (Padx 5) (Pady 5))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

