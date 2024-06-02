#!/usr/bin/env newlisp 
## Time-stamp: <2024-06-02 23:05:22 paul>
;; source:  https://wiki.tcl-lang.org/page/toolbar
;; filename: ~/.local/newLISP/newLISP-Tk/src/Toolbar.tcl

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

## ------------- COMMANDS ------------------------------------------------

(define (MAIN:end)
   (ts:quit))

(define (nop) (begin))                  ;nop = no operation

## ------------- INIT ----------------------------------------------------
(Tk:init)

(ts:require "tooltip")

(ts:setw (Window (Name "win")
                 (Title "tooltest newLISP-Tk")
                 ;; (Minsize (Width 200) (Height 50))
                 ))
(:build win)
## ------------- GUI -----------------------------------------------------
;; create icons:
;; tcltk: image create photo Bildname -file DateinameDesBildes
;; (ts:image-create-photo (Name "<name>") (File "<path>"))

(ts:image-create-photo (Name "icon-new")
 (File "/usr/share/icons/Tango/22x22/actions/document-new.png"))

(ts:image-create-photo (Name "icon-open")
 (File "/usr/share/icons/Tango/22x22/actions/document-open.png"))

(ts:image-create-photo (Name "icon-save")
 (File "/usr/share/icons/Tango/22x22/actions/document-save.png"))

(ts:image-create-photo (Name "icon-print")
 (File "/usr/share/icons/Tango/22x22/actions/document-print.png"))

## ------------- edit ----------------------------

(ts:image-create-photo (Name "icon-undo")
 (File "/usr/share/icons/Tango/22x22/actions/edit-undo.png"))

(ts:image-create-photo (Name "icon-redo")
 (File "/usr/share/icons/Tango/22x22/actions/edit-redo.png"))

(ts:image-create-photo (Name "icon-cut")
 (File "/usr/share/icons/Tango/22x22/actions/edit-cut.png"))

(ts:image-create-photo (Name "icon-copy")
 (File "/usr/share/icons/Tango/22x22/actions/edit-copy.png"))

(ts:image-create-photo (Name "icon-paste")
 (File "/usr/share/icons/Tango/22x22/actions/edit-paste.png"))

(ts:image-create-photo (Name "icon-find")
 (File "/usr/share/icons/Tango/22x22/actions/edit-find.png"))

(ts:image-create-photo (Name "icon-replace")
 (File "/usr/share/icons/Tango/22x22/actions/edit-find-replace.png"))


;; create frame for toolbar box
(ts:setw (Frame (Name "box")))          ;toolbar frame
(:build box)
(:setgrid box (Sticky "news"))

;; create buttons
(ts:setw (Button (Name "tbutton1") (Parent "box") (Text "New")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-new") ))
(:build tbutton1)
(:setgrid tbutton1 (Row 0) (Column 0) (Pady "3") (Padx "0"))
## ----------------------
(ts:setw (Button (Name "tbutton2") (Parent "box") (Text "Open")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-open") ))
(:build tbutton2)
(:setgrid tbutton2    (Row 0) (Column 1) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Button (Name "tbutton3") (Parent "box") (Text "Save")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-save") ))
(:build tbutton3)
(:setgrid tbutton3 (Row 0) (Column 2) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Separator (Parent "box") (Name "separator1") (Orient "vertical")))
(:build separator1)
(:setgrid separator1   (Row 0)(Column 3) (Pady 7) (Padx 2) (Sticky "news"))
## ----------------------
(ts:setw (Button (Name "tbutton4") (Parent "box") (Text "Print")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-print") ))
(:build tbutton4)
(:setgrid tbutton4 (Row 0) (Column 4) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Separator (Parent "box") (Name "separator2") (Orient "vertical")))
(:build separator2)
(:setgrid separator2   (Row 0)(Column 5) (Pady 7) (Padx 2) (Sticky "news"))
## ----------------------
(ts:setw (Button (Name "tbutton5") (Parent "box") (Text "Undo")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-undo") ))
(:build tbutton5)
(:setgrid tbutton5 (Row 0) (Column 6) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Button (Name "tbutton6") (Parent "box") (Text "Redo")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-redo") ))
(:build tbutton6)
(:setgrid tbutton6 (Row 0) (Column 7) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Separator (Parent "box") (Name "separator3") (Orient "vertical")))
(:build separator3)
(:setgrid separator3   (Row 0)(Column 8) (Pady 7) (Padx 2) (Sticky "news"))
## ----------------------
(ts:setw (Button (Name "tbutton7") (Parent "box") (Text "Cut")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-cut") ))
(:build tbutton7)
(:setgrid tbutton7 (Row 0) (Column 9) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Button (Name "tbutton8") (Parent "box") (Text "Copy")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-copy") ))
(:build tbutton8)
(:setgrid tbutton8 (Row 0) (Column 10) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Button (Name "tbutton9") (Parent "box") (Text "Paste")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-paste") ))
(:build tbutton9)
(:setgrid tbutton9 (Row 0) (Column 11) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Separator (Parent "box") (Name "separator4") (Orient "vertical")))
(:build separator4)
(:setgrid separator4   (Row 0)(Column 12) (Pady 7) (Padx 2) (Sticky "news"))
## ----------------------
(ts:setw (Button (Name "tbutton10") (Parent "box") (Text "Search")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-find") ))
(:build tbutton10)
(:setgrid tbutton10 (Row 0) (Column 13) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Button (Name "tbutton11") (Parent "box") (Text "Replace")
                 (Command "nop") (Style "Toolbutton") (Compound "top")
                 (Image "icon-replace") ))
(:build tbutton11)
(:setgrid tbutton11 (Row 0) (Column 14) (Pady "3") (Padx "2"))
## ----------------------
(ts:setw (Label (Name "tfill") (Parent "box") ))
(:build tfill)
(:setgrid tbutton11 (Row 0) (Column 15) (Pady "3") (Padx "0") (Sticky "ew"))
## ----------------------
(ts:setw (Text (Parent "box") (Name "textarea") ))
(:build textarea )
##grid .box.textarea -row 1 -column 0 -sticky ewns  -columnspan 20
(:setgrid textarea  (Row 1) (Column 0) (Sticky "ewns") (Columnspan 20))

(:setgrid box (Sticky "news"))

;; not yet available in ts:
(Tk "grid columnconfigure .box 15 -weight 1")
(Tk "grid rowconfigure    .box 1  -weight 1")
(Tk "grid columnconfigure .    0  -weight 1")
(Tk "grid rowconfigure    .    0  -weight 1")

## 
## ------------- QUIT ----------------------------------------------------
(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 2) (Column 0) (Padx 5) (Pady 5))
(:tooltip btn-end "this is the end")

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events


