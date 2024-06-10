#!/usr/bin/env newlisp
## Time-stamp: <2024-06-05 18:40:53 paul>

## Labelframe with checkbuttons

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

## ======= Labelframe ====================================================
(ts:setw (Labelframe (Name "lf") (Text "Title")
                     (Labelanchor "nw")
                     ;; (Width 400) (Height 250)
                     ))
(:build lf)
(:setgrid lf (Row 0) (Column 0) (Columnspan 4) (Sticky "ew") (Padx 10) (Pady 5))


## ------- CheckButtons --------------------
## Column 0 -----------------------------------------------------------
(ts:setVar "cbvar00" "0")       ;define & send it
(ts:setw (Checkbutton (Parent "lf") (Name "cbtn00")
                      (Text "All folders")
                      (Variable "cbvar00")
                      ))
(:select-it cbtn00)
(:build cbtn00)                   ;translate and send to Tk
(:setgrid cbtn00 (Row 0) (Column 0) (Padx 5) (Pady 5) (Sticky "w"))

## -----------

(ts:setVar "cbvar10" "0")       ;define & send it
(ts:setw (Checkbutton (Parent "lf") (Name "cbtn10")
                      (Text "Mirror")
                      (Variable "cbvar10")
                      ))
(:deselect-it cbtn10)
(:build cbtn10)                   ;translate and send to Tk
(:setgrid cbtn10 (Row 1) (Column 0) (Padx 5) (Pady 5) (Sticky "w"))


## -----------

(ts:setVar "cbvar20" "0")       ;define & send it
(ts:setw (Checkbutton (Parent "lf") (Name "cbtn20")
                      (Text "Move")
                      (Variable "cbvar20")
                      ))
(:deselect-it cbtn20)
(:build cbtn20)                   ;translate and send to Tk
(:setgrid cbtn20 (Row 2) (Column 0) (Padx 5) (Pady 5) (Sticky "w"))



## -----------------------------------------------------------------------

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 1) (Column 0) (Padx 5) (Pady 5))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

