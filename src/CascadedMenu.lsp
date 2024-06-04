#!/usr/bin/env newlisp
## Time-stamp: <2024-05-30 23:22:03 paul>

;; Menu-test
;;

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit));end

 

## =======================================================================

(define (MAIN:nop)
   (ts:setw (Button (Name "btn-end") (Text "Do nothing") ))
   (:build btn-end)
   ;; place button at the bottom row
   (:setgrid btn-end (Row 0) (Column 0) (Padx 5) (Pady 5)
             ))

## ---------------- GUI INIT ---------------------------------------------

(Tk:init)
(ts:setw (Window (Name "root")
                 (Title "newLISP-Tk")
                 (Minsize (Width 300) (Height 100)) ))
(:build root)

## ------------- GUI -----------------------------------------------------

(ts:setw (Menu (Name "menubar") )) ;object
(:build menubar)

;; ------------- filemenu ------------------------------------------------
(ts:setw (Menu (Name "filemenu") (Parent "menubar")
               ;; (Tearoff "0")
               )) ;object
(:build filemenu)
(:add-command filemenu (Label "New") (Command "nop"))
(:add-command filemenu (Label "Open") (Command "nop"))
(:add-command filemenu (Label "Save") (Command "nop"))
(:add-command filemenu (Label "Save as...") (Command "nop"))
(:add-command filemenu (Label "Close") (Command "nop"))
(:add-separator filemenu)
(:add-command filemenu (Label "Exit") (Command "exit"))

(:add-cascade menubar (Label "File") (Menu "filemenu"))

## ------------- editmenu ------------------------------------------------
(ts:setw (Menu (Name "editmenu") (Parent "menubar")
               ;; (Tearoff "0")
               )) ;object
(:build editmenu)

(:add-command editmenu (Label "Undo") (Command "nop"))
(:add-separator editmenu)
(:add-command editmenu (Label "Cut") (Command "nop"))
(:add-command editmenu (Label "Copy") (Command "nop"))
(:add-command editmenu (Label "Paste") (Command "nop"))
(:add-command editmenu (Label "Delete") (Command "nop"))
(:add-command editmenu (Label "Select All") (Command "nop"))

(:add-cascade menubar (Label "Edit") (Menu "editmenu"))

## ------------- helpmenu ------------------------------------------------
(ts:setw (Menu (Name "helpmenu") (Parent "menubar")
               ;; (Tearoff "0")
               )) ;object
(:build helpmenu)

(:add-command helpmenu (Label "Help Index") (Command "nop"))
(:add-command helpmenu (Label "About...") (Command "nop"))

(:add-cascade menubar (Label "Help") (Menu "helpmenu"))

## -----------------------------------------------------------------------
;; (:configure root (Menu "menubar"))      ;Vorspann als configure?

(ts:xquit)                      ;quit using the x-button of the window

(Tk:mainloop )        
