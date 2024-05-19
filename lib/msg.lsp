#!/usr/bin/env newlisp 
(context 'MessageBox)
(constant 'TS (string "This is context >" (context)
		      "<, Time-stamp: <2024-05-19 15:26:30 paul>"))
## Emacs: use >Alt-x time-stamp< to update the above line
######################################################################
;; @module msg.lsp
;; @description This module holds the messagebox variations of Tcl/Tk written in newLISP using the lib Tk.lsp
;;
;; This module needs Tk.lsp and should therefore be loaded together with it.
;; It also needs the FOOP-classes as defined in the lib provided with
;; ts.lsp.  So this module needs to be loaded as well.
##################### MAIN ###############################################
;; (context MAIN)

## ------- GLOBAL VARs ---------------------------------------------------
;; (set (global 'h) "")                   ;place holder for tcltk variable


##################### MessageBox #########################################
(context 'MessageBox)
## constructor/default functor
(define (MessageBox:MessageBox )
  (cons (context) (args)))

(define (MessageBox:askokcancel )
  "stub");not yet done

(define (MessageBox:askquestion )
  "stub");not yet done

(define (MessageBox:askretrycancel )
  "stub");not yet done

(define (MessageBox:askyesno )
   (let (title-string "" message-string "" cl "" h "") 
      (setq title-string (or (last (assoc MAIN:Title (self))) "")) 
      (setq message-string (or (last (assoc MAIN:Text (self))) "")) 
      (Tk
       (string
        "set answer ["
        "tk_messageBox"
        " -title \"" title-string "\""
        " -message \"" message-string "\""
        " -icon question"
        " -type yesno"
        " -default no] "
        " ; puts \"{$answer}\""
        ))
      (Tk:wait)
      ;; (println "askyesno.current-line: " (current-line))
      ;; (println "askyesno.eval-string: " (eval-string (current-line)))
      (eval-string (current-line))      ;yes or no
      ));MessageBox:askyesno

(define (MessageBox:askyesnocancel )
   (let (title-string "" message-string "" widget-string "") 
      (setq title-string (or (last (assoc MAIN:Title (self))) "")) 
      (setq message-string (or (last (assoc MAIN:Text (self))) "")) 
      (setq widget-string
            (string
             "tk_messageBox"
             " -title \"" title-string "\""
             " -message \"" message-string "\""
             " -icon question"
             " -type yesnocancel"))
      (Tk widget-string)
      (Tk:wait)
      (eval-string (current-line))
      ));MessageBox:askyesnocancel

(define (MessageBox:showerror )
   [text]show an error message to the user.  It uses an ok-Button
   example:
   (:showerror (MessageBox (Title "message")
                           (Text "file copy done")) ) ;ok
   [/text]
   (let (title-string "" message-string "" widget-string "" ) 
      (setq title-string (or (last (assoc MAIN:Title (self))) "")) 
      (setq message-string (or (last (assoc MAIN:Text (self))) ""))
      (setq widget-string (string
                           "set answer ["
                           "tk_messageBox"
                           " -title \"" title-string "\""
                           " -message \"" message-string "\""
                           " -icon error"
                           " -type ok] ; puts \"{$answer}\""))
      (Tk widget-string)
      (Tk:wait)
      ;; (println "showerror.current-line4: " (current-line)) 
      ;; (println "showerror.eval-string2: "
      (eval-string (current-line))
      ;; )
      ));MessageBox:showerror

(define (MessageBox:showinfo )
   [text]
   show a message to the user.  There is an ok-Button
   example:
   (:showinfo (MessageBox (Title \"message\") (Text \"file copy done\")) ) ;ok
   [/text]
   (let (title-string "" message-string "" widget-string "") 
      (setq title-string (or (last (assoc MAIN:Title (self))) "")) 
      (setq message-string (or (last (assoc MAIN:Text (self))) "")) 
      (setq widget-string
            (string
             "tk_messageBox"
             " -title \"" title-string "\""
             " -message \"" message-string "\""
             " -icon info"
             " -type ok")) 
      (Tk widget-string)));MessageBox:showinfo

(define (MessageBox:showwarning )
   [text]
   show a warning message to the user. Message has an ok-Button
   example:
   (:showwarning (MessageBox (Title \"warning\") (Text \"no directory chosen\")) ) ;ok
   [/text]
   (let (title-string "" message-string "" widget-string "" A "")
      (setq title-string (or (last (assoc MAIN:Title (self))) "")) 
      (setq message-string (or (last (assoc MAIN:Text (self))) "")) 
      (setq widget-string
            (string
             "set answer ["
             "tk_messageBox"
             " -title \"" title-string "\""
             " -message \"" message-string "\""
             " -icon warning"
             " -type ok] ; puts \"{$answer}\"" ))
      (Tk widget-string)
      (Tk:wait)
      (eval-string (current-line))
      ));MessageBox:showwarning

;; The message boxes are modal and will return a subset of
;; (True, False, OK, None, Yes, No) based on the user’s selection.


(println "loading msg: " 'done)


(context MAIN)

