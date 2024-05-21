#!/usr/bin/env newlisp
## Time-stamp: <2024-05-21 08:40:56 paul>

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) ;GUI-Server ’ts (interface)
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)
(load (string LIBS "msg.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)                      ;handler for quit-button
   (let (res "no")                      ;"no"/"yes"
      (while (= res "no")
         (setq res
               (:askyesno
                (MessageBox (Title "askyesno")
                            (Text
                             (string "Are you sure?" ))))))

      (ts:quit)) 
   )

(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

(ts:setw (Label (Name "lbl") (Text "Hello World!")))  ;define label
(:build lbl)
(:setgrid lbl (Row 0) (Column 0) (Padx 5) (Pady 10)) ;position

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 1) (Column 0) (Padx 5) (Pady 5))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

