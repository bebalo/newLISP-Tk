#!/usr/bin/env newlisp
## Time-stamp: <2024-06-02 11:14:58 paul>

;; Listbox with two Scrollbars
;;

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit));end


(setq str
      "{Anton Berta Caesar} {Hugo Fritz Anna Lena Marie Peter Claudia Frida Dora Emil} {Friedrich Gustav Heinrich Ida} {Ulrike Jonas} {Julius Antonius Simon} {Ferdinand Maria Mirjam} {Anton Berta Caesar}"
      )

## ---------------- GUI INIT ---------------------------------------------

(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

## --- sbX --- "forward definition" -------------
(ts:setw (Scrollbar (Name "sbX")
                    (Orient "horizontal")
                    ))
(:build sbX)

## --- sbY ---
(ts:setw (Scrollbar (Name "sbY")
                    (Orient "vertical")
                    ))
(:build sbY)

## --- lbox ---
(ts:setVar "Liste" str)                 ;lbox-values
(ts:setw (Listbox (Name "lbox")
                  (Listvariable "Liste")
                  (Xscrollcommand "sbX")
                  (Yscrollcommand "sbY")
                  (Selectmode "extended")
                  (Width 20) (Height 5)
                  ))
(:build lbox)

;; --- connect scrollbars to listbox ---
(:configure sbX                         ;Scrollbar
            (Connect "lbox")
            (Command "xview")
            )

(:configure sbY                         ;Scrollbar
            (Connect "lbox")
            (Command "yview")
            )

;; place them in our grid
(:setgrid sbX  (Row 1) (Column 0) (Sticky "we")) 
(:setgrid sbY  (Row 0) (Column 1) (Sticky "ns")) 
(:setgrid lbox (Row 0) (Column 0) (Sticky "nsew"))

;; not yet done in ts:
(Tk "grid rowconfigure . 0 -weight 1")
(Tk "grid columnconfigure . 0 -weight 1")


## --------- QUIT --------------------------------------------------------

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
;; place button at the bottom row
(:setgrid btn-end (Row 2) (Column 0) (Padx 5) (Pady 5) (Sticky "e"))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events


