(global 'MAIN:PID)
(context MAIN)
(set (global 'started) nil)             ;mainloop not yet started
(context 'Tk) 
(constant 'TK "Time-stamp: <2024-05-24 21:57:28 paul>")
## Emacs: use >Alt-x time-stamp< to update the above line 
##########################################################################
;; @module Tk.lsp 
;; @description This module provides a couple of functions used to communicate with Tcl/Tk (<wish>).
;;  
;; @author @ pdb 2024
;; 
;; Base process is Tcl/Tk's </usr/bin/wish>.
;; 
;; This module provides the functions:
;; <blockquote>
;; <ul>
;; <li>init</li>
;; <li>Tk</li>
;; <li>mainloop</li>
;; </ul>
;; </blockquote>
;; 
;; filename: ~/.local/lib/newLISP/Tk-server/Tk.lsp
##########################################################################
##------------------------------------------------------------------------
;; @syntax (Tk:Tcl-procs)
;;
;; This function will send two procs to <wish> providing the ability to
;; <blockquote>
;; <ul>
;; <li>create a new window</li>
;; <li>to center it on the screen</li>
;; </ul>
;; </blockquote>
;;
##------------------------------------------------------------------------
(define (Tk:Tcl-procs)
   "send help procedures to wish"
   (when (and tcin myout myin tcout)	;may communicate with wish
    (Tk:Tk [text]
	   
  # Create new window
  proc ::create_window {win ttl} {
    if {[winfo exists $win]} { destroy $win }
    toplevel $win
    wm title $win $ttl
    wm resizable $win 0 0
  }

  # Center the window (without showing it moving)
  proc ::center_window {win} {
    wm withdraw $win
    update idletasks

    set w [winfo width $win]
    set h [winfo height $win]
    wm geometry $win "+[expr {($::SW - $w) / 2}]+[expr {($::SH - $h) / 2}]"

    wm deiconify $win
  }

[/text]));when
);Tk:Tcl-procs


##------------------------------------------------------------------------
;; @syntax (Tk:init)
;;
;; Establish the pipes between <Tk.lsp> and <wish>.
;; There are two pipes being used:
;; <blockquote>
;; <ul>
;; <li>one for reading (from whish via myin/tcout)</li>
;; <li>one for writing (to whish via tcin/myout)</li>
;; </ul>
;; </blockquote>
;; When running on a Linux-system, there will be a third pipe (errout),
;; showing error-messages.
##-------------------------------------------------------------------------
(define (Tk:init )
   "setup communications to Tcl/Tk (wish)"
   (map set '(myin tcout) (pipe))
   (map set '(tcin myout) (pipe))
   ##start wish
   (if (= ostype "Linux")
       (begin
          (map set '(errin errout) (pipe))
          (setq MAIN:PID (process "/usr/bin/wish" tcin tcout errout)))
       (begin
          (setq MAIN:PID (process "/usr/bin/wish" tcin tcout ))))
   (println "Tk:init.PID: " MAIN:PID));Tk:init


##-------------------------------------------------------------------------
;; @syntax (Tk:Tk)
;; 
;; This is the Default-Functor for the (context ’Tk) and the basic
;; sending-machine. It mainly does a write-line into the pipe myout.
;; So it’s a sender only.
;; 
;; Data being sent to <wish> via a <write-line> and the according internal
;; pipe-handler <myout>.
;; 
;; @example
;; (Tk "wm geometry . +600+100")
;; (Tk "wm title    . {Label-Test}")
;; 
;; (Tk "ttk::label .lb1 -text \"Feld 1\" -borderwidth 2 -relief solid")
;; (Tk "grid .lb1 -row 0 -column 0")
##-------------------------------------------------------------------------
(define (Tk:Tk )
   [text]
   Tk:Tk function to pass commands to Tcl/Tk.
   example:
   (Tk "bind . <Destroy> {puts {(exit)}}")
   [/text]
   ;; (println "Tk:Tk:(args): " nl (args))
   ;; (println "Tk:Tk:(apply string (args)): " nl (apply string (args)))
   (write-line
    myout
    (append "if { [catch {puts [" (apply string (args)) "] }] } {" 
            "tk_messageBox -message $errorInfo; exit }\n\t\t       ")
    ));Tk:Tk


##------------------------------------------------------------------------
;; @syntax (Tk:mainloop)
;;
;; This function defines an endless loop, waiting for the output
;; from <wish>.
;; This output comes from events triggered by the user, such as
;; mouse-clicks or data entry into the GUI.
;; Output from <wish> will be interpreted using <eval-string>.
;; Commands associated with a button for instance, can be called this way.
;;
;; <Tk:mainloop> can only be stopped via an event like pressing an
;; exit-Button and therefore calling a command which in turn will stop
;; (exit) the application.
##------------------------------------------------------------------------
(define (Tk:mainloop)           
   (let (cl "" pwd "4Osya" )
      ;; (println "Tk:mainloop.peek0: " (peek myin))
      (unless MAIN:started
         (Tk "puts \"" pwd "\"")        ;send start signal ("password")
         (do-while (!= (current-line) pwd)
            (read-line myin)
            ;; (println "Tk:mainloop.peek: " (current-line))
            )
         (setq MAIN:started true)) ;start signal was read, pipe is empty now
      ;; mainloop
      (while (read-line myin)
         (setq cl (current-line))
         ;; (println "Tk:mainloop.cl: " nl cl)
         (eval-string cl);<========================
         )));Tk:mainloop


##------------------------------------------------------------------------
;; @syntax (Tk:wait)
;;
;; @result a non-empty line read by the newLISP read-line function.
;; 
;; This function defines a loop, waiting for the output from <wish>.
;; This output normally is the answer to a question sent to <wish>.
;;
;; The loop is left when read-line detects a non-empty line.
##------------------------------------------------------------------------
(define (Tk:wait)
   "wait for a response from tcltk"
   (catch
         (while (read-line Tk:myin) 
            ;; (println "wait.line-was-read!" )
            (if (not (empty? (current-line))) ;leave loop
                (throw true))
            ;; (println "wait.read-line-n:" nl) ;waiting
            ))
   ;; (current-line) now holds the last answer from wish
   );wait




##-------------------------------------------------------------------------
(context MAIN)

(println "loading Tk: " 'done)


