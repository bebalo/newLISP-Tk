#!/usr/bin/env newlisp
## Time-stamp: <2024-05-21 09:35:45 paul>

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit));end

(define (changeText)
   (:configure lbl (Text "new text"))
   (Tk "update idletasks"))


(Tk:init)

;; if we want a window title:
;; (ts:setw (Window (Name "win")
;;                  (Title "newLISP-Tk")
;;                  (Minsize (Width 200) (Height 50)) ))
;; (:build win)

## -----------------------------------------------------------------------

(ts:setw (Label (Name "lbl") (Text "This is a short text"))) ;define label
(:build lbl)                  ;(Label:build lb1) send to Tk
(:setgrid lbl (Row 0) (Column 0)
          (Padx 5) (Pady 10) (Sticky "e")) ;position

(ts:setw (Button (Name "btn") (Text "change label") (Command "changeText")))
(:build btn)
(:setgrid btn (Row 1) (Column 0) (Padx 5) (Pady 5))

## -----------------------------------------------------------------------

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 1) (Column 1) (Padx 5) (Pady 5))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

