#!/usr/bin/env newlisp
## Time-stamp: <2024-05-25 07:52:08 paul>

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))

(load (string LIBS "Tk.lsp")) 
(load (string LIBS "msg.lsp")) 
(load (string LIBS "ts.lsp")) ;GUI-Server ’ts (interface)

(define (MAIN:end)
   (ts:quit));end
 
## ---------------- GUI --------------------------------------------------

(Tk:init)

(ts:setw (Window (Name "win")
                 (Title "newLISP-Tk")
                 (Minsize (Width 200) (Height 50)) ))
(:build win)

## -----------------------------------------------------------------------

[text]
frame .fr
listbox .fr.lbox -width 5 -height 5 -listvariable Liste -xscrollcommand {.fr.sbX set} -yscrollcommand {.fr.sbY set}
scrollbar .fr.sbX -orient horizontal -command {.fr.lbox xview}
scrollbar .fr.sbY -orient vertical   -command {.fr.lbox yview}
[/text]

(ts:setw (Frame (Name "fr")))
(:build fr)
(:setgrid fr (Row 0) (Column 0))

;; lbox forward declaration:
(ts:setw (Listbox (Name "lbox") (Parent "fr")))

(ts:setw (Scrollbar (Name "sbY") (Parent "fr")
                    (Orient "vertical") (Command "yview")
                    ))
(:build sbY)
(:setgrid sbY  (Row 0) (Column 1) (Sticky "sn"))

;; lbox full declaration:
(ts:setw (Listbox (Name "lbox") (Parent "fr")
                  (Width 5) (Height 5)
                  (Yscrollcommand "sbY")
                  ;; (Listvariable "Liste")
                  ;; (Selectmode "extended")
                  ))
(:build lbox)
(:setgrid lbox (Row 0) (Column 0))


;; (Tk "grid rowconfigure . 0 -weight 1")
;; (Tk "grid columnconfigure . 0 -weight 1")


;; ---------- tcltk ------------------------------------------------------
;; Tcl/Tk-Buch (Bsp192):
;; set Liste {Anton Berta Caesar Dora Emil Friedrich Gustav Heinrich Ida Julius Kaufmann}
;; lappend Liste {Ludwig Martha Nordpol Otto Paula Quelle Richard Samuel Schule Theodor}
;; set Liste [concat $Liste {Ulrich Viktor Wilhelm Xanthippe Ypsilon Zacharias}]
;; 
;; listbox .lbox -width 15 -height 15 -xscrollcommand {.sbX set} -yscrollcommand {.sbY set} -listvariable Liste
;; ttk::scrollbar .sbX -orient horizontal -command {.lbox xview}
;; ttk::scrollbar .sbY -command {.lbox yview}
;; 
;; grid .sbX -row 1 -column 0 -sticky we
;; grid .sbY -row 0 -column 1 -sticky ns
;; grid .lbox -row 0 -column 0 -sticky nsew
;; grid rowconfigure . 0 -weight 1
;; grid columnconfigure . 0 -weight 1


;; ------------- tkinter -------------------------------------------------
;; https://www.tutorialspoint.com/python/tk_scrollbar.htm :
;; 
;; from tkinter import *
;; 
;; root = Tk()
;; scrollbar = Scrollbar(root)
;; scrollbar.pack( side = RIGHT, fill=Y )
;; 
;; mylist = Listbox(root, yscrollcommand = scrollbar.set )
;; for line in range(100):
;;    mylist.insert(END, "This is line number " + str(line))
;; 
;; mylist.pack( side = LEFT, fill = BOTH )
;; scrollbar.config( command = mylist.yview )
;; 
;; mainloop()


## --------- QUIT --------------------------------------------------------

(ts:setw (Button (Name "btn-end") (Text "Quit") (Command "end")))
(:build btn-end)
(:setgrid btn-end (Row 1) (Column 0) (Padx 5) (Pady 5) (Sticky "e"))

(ts:xquit)                      ;quit using the x-button of the window
(Tk:mainloop )                  ;Tk: listening to in-coming events

