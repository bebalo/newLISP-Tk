#!/usr/bin/env newlisp
## Time-stamp: <2024-06-03 16:53:34 paul>

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

## ------------- GUI -----------------------------------------------------

(Tk:init)

;; "if we want a window title:"
(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 200)) ))
(:build win)

## -----------------------------------------------------------------------

(ts:setw (Notebook (Name "nb") ))
(:build nb)
(:setgrid nb (Row 0) (Column 0)
          (Sticky "news")
          )

(ts:setw (Frame (Name "fr1") (Parent "nb")  (Width 200) (Height 200)))
(:build fr1)
(:add-frame nb fr1 "Label 1") 

(ts:setw (Frame (Name "fr2") (Parent "nb") ))
(:build fr2)
(:add-frame nb fr2 "Label 2")


## ---- frame 1 -------
(ts:setw (Label (Parent "fr1") (Name "lbl1")
                (Text "Page 1 and some text in it.") ; (Width 50)
                ))
(:build lbl1)
(:setgrid lbl1 (Row 0) (Column 0) (Padx 10) (Pady 10))      ;make it visible

(ts:setw (Button (Parent "fr1") (Name "btn1") (Text "Quit") (Command "end")))
(:build btn1)
(:setgrid btn1 (Row 1) (Column 0))      ;make it visible


## ---- frame 2 -------
(ts:setw (Label (Parent "fr2") (Name "fr2lbl1")
                (Text "Page 2: again som text here.") ; (Width 50)
                ))
(:build fr2lbl1)
(:setgrid fr2lbl1 (Row 0) (Column 0) (Padx 10) (Pady 10)) ;make it visible
;; .nb select .nb.fr2
(:select fr2)                            ;has no effect

## -----------------------------------------------------------------------
;; not yet available in ts:
(Tk "grid columnconfigure .    0  -weight 1")
(Tk "grid rowconfigure    .    0  -weight 1")

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

