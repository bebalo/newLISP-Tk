#!/usr/bin/env newlisp
## Time-stamp: <2024-05-21 09:35:45 paul>

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit))

(Tk:init)

(ts:require "tooltip")

;; tcltk:
;; pack [label .l -text "Hover your mouse over me!"]
;; tooltip::tooltip .l "I'm a helpful hint!"
;;
(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

(ts:setw (Label (Name "lbl")  (Text "Hugo1")))
(:build lbl)
(:setgrid lbl (Row 0) (Column 0))
;;(Tk "tooltip::tooltip .lbl \"I'm a helpful hint!\"")
(:tooltip lbl "I'm a helpful hint number one!") 


(ts:setw (Frame (Name "frm")))
(:build frm)
(:setgrid frm (Row 1) (Column 0))

(ts:setw (Label (Name "lbl")  (Parent "frm") (Text "Hugo2")))
(:build lbl)
(:setgrid lbl (Row 0) (Column 0))
;;(Tk "tooltip::tooltip .frm.lbl \"I'm a helpful hint!\"")
(:tooltip lbl "I'm a helpful hint number two!") 

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 2) (Column 0) (Padx 5) (Pady 5))
;;(Tk {tooltip::tooltip .btn-end "this is the end"})
(:tooltip btn-end "this is the end")

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

