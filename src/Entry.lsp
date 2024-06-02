#!/usr/bin/env newlisp
## Time-stamp: <2024-05-21 12:43:24 paul>

;; reading and writing an Entry field

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

## ------- commands ------------------------------------------------------
(define (MAIN:end)
   (ts:quit));end

(define (showText)
   (let (text "")
      (setq text (ts:getVar "NameText"))
      ;; (println "showText: " text)
      (:configure lbl2 (Text text))
      (Tk "update idletasks")
      ))

## ---------- GUI --------------------------------------------------------

(Tk:init)

##;; if we want a window title:
;; (ts:setw (Window (Name "win")
;;                  (Title "newLISP-Tk")
;;                  (Minsize (Width 200) (Height 50)) ))
;; (:build win)

## -----------------------------------------------------------------------
(ts:setw (Label (Name "lbl2") (Text "Name given"))) ;define label
(:build lbl2)
(:setgrid lbl2 (Row 1) (Column 0)
          (Padx 5) ; (Pady 10) (Sticky "e") ;position
          )

(ts:setw (Button (Name "show") (Text "show-it") (Command "showText")))
(:build show)
(:setgrid show (Row 1) (Column 1) (Padx 5) (Pady 5))


(ts:setw (Label (Name "lbl") (Text "Name:"))) ;define label
(:build lbl)
(:setgrid lbl (Row 0) (Column 0)
          (Padx 5) ; (Pady 10) (Sticky "e") ;position
          )

(ts:setw (Entry (Name "en") ; (Width 80)
                (Textvariable "NameText")
                ))
(:build en) 
(:setgrid en (Row 0) (Column 1)
          (Padx 5) ; (Pady 10) ; (Sticky "n")
          )
(:insert en 0 "enter name here")

(:focus en)
(:selection en "0" "end")

## -----------------------------------------------------------------------

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 2) (Column 1) (Padx 5) (Pady 5))

(ts:xquit)                      ;quit using the x-button of the window
(println "Entry-start")
(Tk:mainloop )                  ;Tk: listening to in-coming events

