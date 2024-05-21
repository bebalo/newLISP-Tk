#!/usr/bin/env newlisp
## Time-stamp: <2024-05-21 15:14:40 paul>

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit));end


(define (click)
   (let (checked "")
      (setq checked (ts:getVar "cbtnVar" ))
      ;; (println nl "click.checked: " checked)
      (if (= checked "1")
          (ts:setVar "Text" "button checked")
          (ts:setVar "Text" "button not checked")
          )));hide_unhide_options

## -------------- GUI ----------------------------------------------------


(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

;; Ceckbutton
(ts:setVar "cbtnVar" "0")               ;define & send it
(ts:setw (Checkbutton (Name 'cbtn) (Text "show state")
                      (Variable "cbtnVar")
                      (State "normal")  ;(State "disabled")
                      (Command "click")
                      ))
(:build cbtn)                           ;send to Tk
(:deselect-it cbtn) ; (:select-it cbtn_expert)
(:setgrid cbtn (Row 0) (Column 0) (Padx 10) (Pady 10) (Sticky "n"))

;; Label
(ts:setVar "Text" "") 
(ts:setw (Label (Name 'lbl) (Textvariable "Text"))) ;define label
(:build lbl)                            ;(Label:build lb1) send to Tk
(:setgrid lbl (Row 1) (Column 0)
          (Padx 5) (Pady 10) ; (Sticky "e")
          ) ;position


## ----------------- QUIT

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 2) (Column 0) (Padx 5) (Pady 5))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

