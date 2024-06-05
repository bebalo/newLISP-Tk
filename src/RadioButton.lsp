#!/usr/bin/env newlisp
## Time-stamp: <2024-06-05 08:37:02 paul>

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

## -----------------------------------------------------------------------

(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

## -----------------------------------------------------------------------
;; ;; tcl/tk:
;; set Text ""
;; set Farbe "rot"
;; ttk::radiobutton .rbRot -text "rot" -variable Farbe -value "rot" -command {Klick $Farbe}
;; ttk::radiobutton .rbGelb -text "gelb" -variable Farbe -value "gelb" -command {Klick $Farbe}
;; ttk::radiobutton .rbBlau -text "blau" -variable Farbe -value "blau" -command {Klick $Farbe}
;; ttk::label .lbAnzeige -textvariable Text
(ts:setw (Radiobutton (Name "rbRot") (Text "rot")
                      (Variable "Farbe") (Value "rot")
                      (Command "nop")))
(:build rbRot)
(:setgrid rbRot (Row 0) (Column 0) (Padx 5) (Sticky "w"))

(ts:setw (Radiobutton (Name "rbGelb") (Text "gelb")
                      (Variable "Farbe") (Value "gelb")
                      (Command "nop")))
(:build rbGelb)
(:setgrid rbGelb (Row 1) (Column 0) (Padx 5) (Sticky "w"))

(ts:setw (Radiobutton (Name "rbBlau") (Text "blau")
                      (Variable "Farbe") (Value "blau")
                      (Command "nop")))
(:build rbBlau)
(:setgrid rbBlau (Row 2) (Column 0) (Padx 5) (Sticky "w"))


## -----------------------------------------------------------------------
(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 3) (Column 0) (Padx 5) (Pady 5))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

