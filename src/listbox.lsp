#!/usr/bin/env newlisp
## Time-stamp: <2024-05-25 07:52:08 paul>

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit));end

(setq mylist
      '("hugo" "fritz" "anna" "lena" "alex" "marie" "peter" "claudia"
        "ulrike" "jonas"))

(setq str (join mylist " "))
(setq str-tk (string "set Liste {" str "} "))

## ---------------- GUI --------------------------------------------------

(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

## -----------------------------------------------------------------------

;; (ts:setw (Frame (Name "frme")))
;; (:build frme)
;; (:setgrid frme (Row 0) (Column 0))

(Tk str-tk )
##listbox .lbox -listvariable Liste -selectmode extended
(ts:setw (Listbox (Name "lbox") ; (Parent "frme")
                  (Width 20) (Height 20)
                  (Listvariable "Liste")
                  (Selectmode "extended")
                  ))
(:build lbox)
(:setgrid lbox (Row 0) (Column 0))

## -----------------------------------------------------------------------

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 1) (Column 0) (Padx 5) (Pady 5) (Sticky "e"))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

