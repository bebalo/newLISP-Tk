#!/usr/bin/env newlisp
## Time-stamp: <2024-05-21 09:34:36 paul>

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit));end


(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

## -----------------------------------------------------------------------

(ts:setw (Frame (Name 'fr)))
(:build fr)                          ;grid .fr -row 0 -column 0
(:setgrid fr (Row 0) (Column 0)) 

(ts:setw (Scrolledtext (Parent 'fr) (Name 'tx) (Width 600) (Height 400)
                       (Wrap "word") ; none, char, word
                       (State "normal")))
(:build tx)
(:setgrid tx (Padx 10) (Pady 10) (Sticky "nesw"))

(:insert tx "1.0" "editable")

;; or:

;; (:import tx "end" "readme.txt")

## -----------------------------------------------------------------------

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 1) (Column 0) (Padx 5) (Pady 5) (Sticky "e"))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

