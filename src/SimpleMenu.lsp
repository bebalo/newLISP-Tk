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

 
## ---------------- GUI INIT ---------------------------------------------

(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

## -----------------------------------------------------------------------
;; tcltk:
;; option add *Menu.tearOff 0
;; . configure -menu.m
;; menu .m

(ts:setw (Menu (Name "m")))
(:build m)

(:add-command m (Label "Open") (Command "tk_getOpenFile"))
(:add-command m (Label "Save") (Command "tk_getSaveFile"))
(:add-command m (Label "Close") (Command "exit"))


## --------- QUIT --------------------------------------------------------

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
;; place button at the bottom row
(:setgrid btn-end (Row 0) (Column 0) (Padx 5) (Pady 5) (Sticky "e"))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events


