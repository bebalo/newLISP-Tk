#!/usr/bin/env newlisp 
## Time-stamp: <2024-05-20 19:37:00 paul>
(setf doc-cmd
      (string 
       "newlispdoc"
       " ../lib/Tk.lsp"
       " ../lib/msg.lsp"
       " ../lib/ts.lsp"
       ))

## Dieses newLISP-Programm in den emacs-Editor laden und mit F5 ausführen.

## oder als feste newlisp-Funktion ("executable"):
## To build this program as a self-contained executable, follow these steps:

#$ newlisp -x nldoc.lsp nldoc
#$ chmod 755 nldoc # give executable permission

(setq nl "\n")
(println doc-cmd nl)

(dolist (x (exec doc-cmd)) (println x))

(exit)


