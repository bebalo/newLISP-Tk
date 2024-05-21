#!/usr/bin/env newlisp 
(context 'ts)                           ;gui-server Tk: Tk-Server=ts
(constant 'TS
          (string "This is context >" (context)
		  "<, Time-stamp: <2024-05-21 12:02:11 paul>"))
## Emacs: mittels >Alt-x time-stamp< wird die obige Zeile aktualisiert
##########################################################################
;; @module ts.lsp
;; @description This GUIserver (ts.lsp) realizes an interface between
;; the base modul (Tk.lsp) and the user-application.
;; It is possible to define widgets and their position within the GUI.
;; @author pdb 2024
;; 
;; A connection will be used to the Tcl/Tk-Server </usr/bin/wish> via base
;; modul <Tk.lsp>.
;; 
;; filename: /home/paul/.local/lib/newLISP/Tk-server/ts.lsp

###################### MAIN ##############################################
(context MAIN)

(constant (global 'LIBS)
          (string (env "HOME") "/.local/newLISP/newLISP-Tk/lib/" ))
## ------- INIT/IMPORT -----------------------------------------------
;; (load (string LIBS "Tk.lsp"))     ;(context 'Tk)
;; (load (string LIBS "msg.lsp"))    ;interface to Tcl/Tk message boxes 

## ------- GLOBAL VARs ---------------------------------------------------
(set (global 'hh) "")                 ;place holder for tcltk variable


(define (assert condition (message "User-Error"))
   (unless condition
      (throw-error message)
      )
   );assert

###################### ts ################################################
(context 'ts)                           ;gui-server Tk: Tk-Server=ts

##------------ CLASSES & METHODS -------------------------------------
(constant 'ts:ts-dbg nil) ; true)                 ;true/nil


## =================== MAIN ==============================================
(context MAIN)
## main Class:
(new Class 'Window)

## methods:
##--------------------------------------------------------------------
;; @syntax (:build-tk-name <obj>)
;; @param <obj> = (self) Object of any ts-class
;;
;; Method :build-tk-name uses the Name and Parent information of the objects
;; used in the Name-Parent-chain to build a name in Tcl/Tk-syntax.
;; 
;; @example
;; (ts:setw (Window (Name "win") (Title "MyTitel") (Minsize (Width 100) (Height 50))))
;; > win
;; (Window (Name "win") (Title "MyTitel") (Minsize (Width 100) (Height 50)))
;; > frm 
;; (Frame (Parent win) (Name "frm"))
;; > lb1
;; (Label (Parent frm) (Name "lb1") (Text "Feld 1"))
;; 
;; > (:build-tk-name lb1)
;; ".frm.lb1"

(define (Window:build-tk-name)             ;cover function
   "build a name in Tcl/Tk-syntax"
   (string "."
           (join (map string (:_build-tk-name (self))) "."))
   );build-name


(define (Window:_build-tk-name )
   (let (e '() nn '() pp '())
      (MAIN:assert
       (member
        (and (list? (self)) (not (empty? (self))) (first (self)))
        '(Label Labelframe Window Frame Button Entry Checkbutton Scrolledtext)) ;objects known so far
       "_build-tk-name: argument is not a known object")
      
      (setq nn (assoc Name   (self)))   ;objetc's own Name
      (when nn (setq e (rest nn)))      ;part of result
      (setq pp (assoc Parent (self))) 
      (when (not (nil? pp))             ;Parent name given
         (if (eval (last pp))           ;Parent exists
             (setq e (append (:_build-tk-name (eval (last pp))) e )) ;recursion
             (setq e (append (map term (rest pp)) e)) ;no recursion, use it as it is
             ));when
      e
      ));_build-tk-name


##--------------------------------------------------------------------
;; @syntax (:setgrid <obj> <row> <column> [<columnspan> <padx> <pady> <sticky>])
;; 
;; @param  <obj> is the widget-object to which the position will be added
;; @param  <row> is of the form (Row <num>) will be added to <obj>
;; @param  <column> is of the form (Column <num>) will be added to <obj>
;; 
;; optional:
;; 
;; @param  <columnspan> is of the form  (Columnspan number) and will be added to <obj>
;; @param  <padx> is of the form (Padx <num>) will be added to <obj>
;; @param  <pady> is of the form (Pady <num>) will be added to <obj>
;; @param  <sticky> is of the form (Sticky <str-sticky>) will be added to <obj>
;;
;; All parameters are referenced by the use of the (self) or the (args)
;; function.
;; Therefore they are not named in the parameter list of the function
;; definition.
;; 
;; notice:
;; positions of (args) are interchangeable. 
;; 
;; Method <:setgrid> adds a grid position to (self).
;; This stored position will be unique: the last one given, will be used.
;; The grid position is being sent to Tk.
;;
;; Optionally there can be padx and pady values given, in order to have
;; some space around the widget (self).
;; 
;; The <sticky> option should have a string containing any combination of
;; the characters in [nsew]
;; (standing for "north", "south", "east", "west").
;;  
;; @example
;; > (ts:setw (Label (Name 'lbl_quelle) (Text "Quelle"))) ;define label
;; (Label (Name "lbl_quelle") (Text "Quelle"))
;; 
;; > (:setgrid lbl_quelle (Row 0) (Column 0) (Padx 5) (Pady 10))
;; "grid .lbl_quelle -row 0 -column 0 -padx 5 -pady 10"
;; 
;; (:setgrid lf (Row 2) (Column 3) (Columnspan 4)
;;           (Sticky "ew") (Padx 10) (Pady 5))
;; 
##--------------------------------------------------------------------
(define (Window:setgrid  )
   ":setgrid adds or replaces a grid position to (self)"
   (MAIN:assert (not (empty? (args))) ":setgrid needs at least 2 arguments")

   [text]example:
   > (ts:setw (Label (Name "hugo") (Text "holla") ))
   (Label (Name "hugo") (Text "holla"))
   > (:setgrid hugo (Row 1) (Column 2) (Padx 5) (Pady 10))
   args: ((Row 1) (Column 2) (Padx 5) (Pady 10))
   self: (Label (Name "hugo") (Text "holla"))
   "grid .hugo -row 1 -column 2 -padx 5 -pady 10"
   > hugo
   (Label (Name "hugo") (Text "holla") (Padx 5) (Pady 10) (Grid (Row 1) (Column 2)))
   [/text]
   
   (let (name-string ""  grid-string ""  r 0  c 0  pos '())
      (setq name-string  (:build-tk-name (self)))
      (doargs (item)                    ;Arguments
         (when (= (first item) 'Row)    (setq r (last item)))
         (when (= (first item) 'Column) (setq c (last item)))
         (when (= (first item) 'Padx)   (setf (self) (push item (self) -1)))
         (when (= (first item) 'Pady)   (setf (self) (push item (self) -1)))
         );doargs
      (setq pos (Grid (Row r) (Column c)))
      (if (assoc Grid (self))           ;overwrite, Grid already there
          (begin (setf (assoc Grid (self)) pos))
          (begin (setf (self) (push pos (self) -1))))
      (setq grid-string
            (string "grid " name-string
                    " -row " (last (assoc '(Grid Row) (self)))
                    " -column " (last (assoc '(Grid Column) (self)))
                    ));setq
      (when (assoc Columnspan (args))
         (setq grid-string
               (string grid-string
                       " -columnspan "
                       (last (assoc Columnspan (args)))
                       )));when
      (when (assoc Padx (args))
         (setq grid-string
               (string grid-string
                       " -padx "
                       (last (assoc Padx (args)))
                       )));when
      (when (assoc Pady (args))
         (setq grid-string
               (string grid-string
                       " -pady "
                       (last (assoc Pady (args)))
                       )));when
      (when (assoc Sticky (args))
         (setq grid-string
               (string grid-string
                       " -sticky "
                       (last (assoc Sticky (args)))
                       )));when
      ;; (println "setgrid: " grid-string) 
      (Tk grid-string) ; ==> send to Tk
      ));:setgrid


(define (Window:grid)
   "unhide self: show widget"
   ;; (ts:setw (Labelframe (Name 'lf)))
   ;; ...
   ;; ... (:setgrid lf ...)
   ;; (:grid lf) --> Tcl/Tk: grid .lf
   (let (widget-string ""  name-string "")
      (setq name-string (:build-tk-name (self)))
      (when (assoc Name (self))
         (setq widget-string
               (string "grid " name-string))
         ;; (println "Window:grid.widget-string: " widget-string )
         (Tk widget-string )
         );when
      ));Window:grid

   
(define (Window:ungrid)
   "hide self (hide widget)"
   ;; (ts:setw (Labelframe (Name 'lf)))
   ;; ...
   ;; ... (:setgrid lf ...)
   ;; (:ungrid lf) --> Tcl/Tk: grid remove .lf
   (let (widget-string ""  name-string "")
      (setq name-string (:build-tk-name (self)))
      (when (assoc Name (self))
         (setq widget-string
               (string "grid remove " name-string))
         ;; (println "Window:ungrid.widget-string: " widget-string )
         (Tk widget-string )
         );when
      ));Window:ungrid


##--------------------------------------------------------------------
(define (Window:delgrid)
   ":delgrid deletes grid position from (self)"
   (pop-assoc Grid (self))
   );delgrid


##--------------------------------------------------------------------
;; @syntax (Window:build)
;; 
;; Method <:build> builds the main window-Tcl/Tk-string being sent to the
;; wish-server via Tk.
;; 
;; @example
;; > (ts:setw (Window (Name 'win)          ;main window
;;                    (Title "Datensicherung")
;;                    (Minsize (Width 100)
;;                             (Height 50))))
;; (Window (Name "win") (Title "Datensicherung") (Minsize (Width 720) (Height 250)))
;; 
;; (:build win)                            ;will send it to Tk like this:
;; wm title . "Datensicherung"
;; wm minsize . 720 250

##--------------------------------------------------------------------
(define (Window:build)
   "build main Window from (self)" 
   (let (name-string "." title-string "" min-string "" max-string "")
      ;; tcltk example:
      ;; wm title . "MyTitle"
      ;; wm minsize . 100 50
      ;; wm maxsize . 300 100
      (when (assoc Title (self))        ;window title
         (setq title-string
               (string
                "wm title " name-string
                " \"" (last (assoc Title (self))) "\""
                ))
         (if title-string (Tk title-string)) ; ==> send to Tk
         )
      (when (assoc Minsize (self))      ;window minsize (X x Y)
         (setq min-string
               (string
                "wm minsize " name-string
                " " (last (assoc (list Minsize Width) (self))) 
                " " (last (assoc (list Minsize Height) (self)))
                ))
         (if min-string (Tk min-string))
         )
      (when (assoc Maxsize (self))      ;window maxsize (X x Y)
         (setq max-string
               (string
                "wm maxsize " name-string
                " " (last (assoc (list Maxsize Width) (self))) 
                " " (last (assoc (list Maxsize Height) (self)))
                ))
         (if max-string (Tk max-string))
         )
      );let
   );Window:build


##--------------------------------------------------------------------
## example:
## ttk::entry .en
## .en          insert end "Bitte hier Ihre Einabe"
## .line_edit_q insert end "Bitte hier Ihre Eingabe"

;; @syntax (:insert <obj-name> <index> <str-val>)
;;
;; @param <obj-name> widget (entry)
;; @param <index> number (postion from where it starts, minimum 0) or "end"
;; @param <str-val> string to enter into entry field.
;;
;; Inserts a text into <obj> if possible.  Normally an entry- or text-field.
;;
;; @example
;;  (:insert en "end" "Text Hugo")
;; --> tcltk:
;;  ".en insert end \"Text Hugo\""

(define (Window:insert num-ind str-val)
   "Insert Text into an Entry-widget"
   (let (name-string ""   insert-string "")
      (setq name-string  (:build-tk-name (self)))
      (setq insert-string
            (string name-string
                    " insert "
                    num-ind
                    (string " \"" str-val "\"")
                    ))
      ;; (println "insert-string: " insert-string)
      (Tk insert-string)
      );let
   );Window:insert


##--------------------------------------------------------------------
## example:
## set filename "readme.txt"
## .fr.tx import $filename end
;; @syntax (:import <obj-name> <str-pos> <str-file-name> )
;;
;; @param <obj-name> widget (entry)
;; @param <str-pos> position "0"... or "end"
;; @param <str-file-name> string denoting a file path
;;
;; Imports the content of a text file named in <str-file-name> into
;; the widget described by <obj-name>.  Normally a Scrolledtext area.
;;
;; @example
;;  (:import tx "end" "readme.txt")
;; --> tcltk:
;;  ".tx import \"readme.txt\" end"

(define (Window:import ind str-file-name )
   "Import Text from a file and enter into an Entry-widget/Scrolledtext"
   (let (name-string ""   import-string "")
      (setq name-string  (:build-tk-name (self)))
      (setq import-string
            (string name-string
                    " import "
                    (string " \"" str-file-name "\" ")
                    ind
                    ))
      ;; (println nl "import-string: " import-string)
      (Tk import-string)
      );let
   );Window:insert


##--------------------------------------------------------------------
## Tcl/Tk:
## Elementname delete IndexFrom IndexTo
## ttk::entry .en
## .en          delete 0 end 
## ts: (:erase <obj> <from-index> <to-index>) ; index from '0' to 'end'
;; ----------------------------------------------------------------------
;; @syntax (:erase <obj-name> <from-index> <to-index>)
;;
;; @param <obj-name> widget
;; @param <from-index> number (position)
;; @param <to-index> number (position) or "end"
;;
;; Deletes text from <obj> if possible.  Normally an entry- or text-field.
;;
;; @example
;; > (ts:setw (Entry (Name 'en) (Text "hugo")))
;; (Entry (Name "en") (Text "hugo"))
;;
;; > (:erase en 0 "end" )
;; --> tcltk:
;; .en delete 0 end
(define (Window:erase from-index (to-index "end"))
   "Delete Text from an Entry-widget"
   ;;example: (:erase line_edit_q 0 "end")
   (let (name-string "" widget-string "")
      (setq name-string  (:build-tk-name (self)))
      (setq widget-string
            (string name-string
                    " delete "
                    from-index
                    " " to-index        ;optional
                    ))
      ;; (println "Window:erase.widget-string:\n" widget-string)
      (Tk widget-string)
      ));Window:erase


##--------------------------------------------------------------------
## Tcl/Tk:
## Elementname configure -state normal/disabled/readonly
;; ----------------------------------------------------------------------
;; @syntax (:configure <obj-name> <obj-option>... )
;;
;; @param <obj-name> widget
;; @param <obj-option> option to change
;;
;; Changes an option of a widget.
;;
;; @example
;; (ts:setw (Entry (Name 'en) (Text "hugo")))
;; 
;; (:configure en (State "disabled")) 
;; 
;; ttk::entry .en -text "hugo"
;; .en         configure -state disabled 
;; 
;; (:configure <obj> (Text "new text")) 
(define (Window:configure )
   "change attribute of object/widget"
   (let (name-string "" widget-string "" option-string "")
      (MAIN:assert (not (empty? (args))) ":configure needs an argument")
      (setq name-string  (:build-tk-name (self)))
      (setq widget-string
            (string name-string
                    " configure "
                    ))
      (when (assoc State (args))
         (setq option-string
               (string " -state "
                       (last (assoc State (args))) ))
         (setq widget-string
               (string widget-string option-string))
         );when
      (when (assoc Text (args))
         (setq option-string
               (string " -text "
                       "\""(last (assoc Text (args)))"\""
                       ))
         (setq widget-string
               (string widget-string option-string))
         );when
      ;; to be continued
      ;; (println "Window:configure.widget-string:\n" widget-string)
      (Tk widget-string)
      ));Window:configure


(define (Window:focus)
   "set focus on widget"
   (let (name-string "" widget-string "")
      (setq name-string  (:build-tk-name (self)))
      (setq widget-string
            (string "focus "
                    name-string
                    ))
      ;; (println "Window:focus.widget-string: " widget-string)
      (Tk widget-string)
      ));Window:focus


(define (Window:selection from-index (to-index "end"))
   "select content of a widget" ;.en selection range 0 end
   (let (name-string "" widget-string "")
      (setq name-string  (:build-tk-name (self)))
      (setq widget-string
            (string 
             name-string
             " selection"
             " range " from-index " " to-index
             ))
      ;; (println "Window:selection.widget-string: " widget-string)
      (Tk widget-string)
      ));Window:focus

## ===================================================================
## Sub-Classes with inherited methods from 'Window:

# --------------------------------------------------------------------
(new 'Window 'Label)            ;SubClass 'Label inherits from 'Window
##--------------------------------------------------------------------
;; @syntax (Label (Parent <symbol-or-string>) {option}... )
;;
;; A Label always needs a parent. If no parent is given, then the
;; default value for parent will be: >.< in (:build), which means: main window.
;;
;; A label ist set up like this:
;; 
;; @example
;; with a parent:
;; > (ts:setw (Label (Parent 'win) (Name "lb1") (Text "Feld 1")))
;; (Label (Parent win) (Name "lb1") (Text "Feld 1"))
;; 
;; > (ts:setw (Label (Parent ".") (Name "lb1") (Text "Feld 1")))
;; (Label (Parent ".") (Name "lb1") (Text "Feld 1"))
;; 
;; without a parent:
;; > (ts:setw (Label (Name "lb1") (Text "Feld 1")))
;; (Label (Name "lb1") (Text "Feld 1"))


##--------------------------------------------------------------------
;; @syntax (Label:build)
;; 
;; Method <:build> builds the Tcl/Tk-string being sent to the wish-server
;; via Tk.lsp (context 'Tk).
;;
;; The name of the Label always needs a parent. If no parent is given,
;; then the default value for parent is: >.<, which means: main window.
;; 
;; @example
;; > (ts:setw (Label (Name 'lbl_quelle) (Text "Quelle"))) ;define label
;; > (:build lbl_quelle)
;; ttk::label .lbl_quelle -text "Quelle"
##--------------------------------------------------------------------
(define (Label:build)
   [text]
   Build Tcl/Tk-string describing the Label-object.  Example:
   (Label (Name "lb1") (Text "Field 1") (Grid (Row 0) (Column 0)))
   Using :build this will be translated to: 
   "ttk::label .lb1 -text \"Field 1\""
   [/text]
   (let (name-string ""   widget-string ""   text-string "")
      (setq name-string  (:build-tk-name (self))) ;.{<parent-name>.}<label-name>
      (when (assoc Text (self))                   ;Label Text
         (setq text-string            
               (string
                " -text" 
                " \"" (last (assoc Text (self))) "\""
                ))
         (setq widget-string (string "ttk::label "
                                     name-string text-string))
         );when
      ;; (println "Label:build.widget-string: " widget-string)
      (Tk widget-string) ; ==> send to Tk
      );let
   );Label:build



## =======================================================================
## -----------------------------------------------------------------------
(new 'Window 'Button)

##------------------------------------------------------------------------
;; @syntax (Button:build)
#;; @param  <str-file> Dies ist der ...
;; 
;; Method <:build> builds the button Tcl/Tk-string being sent to the
;; wish-server via Tk.lsp (context 'Tk).
;;
;; The name of a button always needs a parent. If no parent is given,
;; then the default value for parent is: >.<, which means: main window.
;; 
;; @example
;; (ts:setw (Button (Name 'btQuit) (Text "Quit") (Command "end")))
;; --> ttk::button .btQuit -text "Quit" -command {(MAIN:end)}
##--------------------------------------------------------------------
(define (Button:build)
   [text]
   Build Tcl/Tk-string describing the Button-object.  Example:
   (ts:setw (Button (Name 'btQuitd) (Text "Quit") (Command "end")))
   --> ttk::button .btQuit -text "Quit" -command {(MAIN:end)}
   will be translated to: 
   ttk::button .btQuit -text {Quit} -command {puts {(exit)}; exit}
                                                   {)}
   [/text]
   (let (name-string ""  widget-string "" text-string "" command-string "" )
      (setq name-string  (:build-tk-name (self))) 
      (when (assoc Text (self))        ;button Text
         (setq text-string            
               (string
                " -text" 
                " \"" (last (assoc Text (self))) "\""
                ))
         (setq widget-string (string "ttk::button "
                                     name-string text-string))
         );when
      (when (assoc Command (self))
         (setq command-string            
               (string
                " -command {puts \"(MAIN:" 
                (last (assoc Command (self)))
                ")\"}")
               )
         (setq widget-string (string widget-string command-string))
         );when
      (Tk widget-string) ; ==> send to Tk
      );let
   );Button:build


## -------------------------------------------------------------------
(new 'Window 'Entry)

(define (Entry:build)
   [text] width in characters
   (ts:setw (Entry (Name 'line_edit_q) (Width 40) (Textvariable "textvar")
                   ))
   [/text]
   (let (name-string ""   widget-string "")
      (setq name-string  (:build-tk-name (self)))
      (setq widget-string
            (string
             "ttk::entry "
             name-string
             ))
      (when (assoc Width (self))
         (setq widget-string
               (string widget-string
                       " -width "
                       (last (assoc Width (self)))
                       )));when
      (when (assoc Textvariable (self))
         (setq widget-string
               (string widget-string
                       " -textvariable "
                       (last (assoc Textvariable (self)))
                       )));when
      ;; (println "Entry:build.widget-string: " widget-string)
      (Tk widget-string) ; ==> send to Tk
      );let
   );Entry:build


## -------------------------------------------------------------------
(new 'Window 'Checkbutton)
(define (Checkbutton:build)
   [text]
   Build Tcl/Tk-string describing the Checkbutton-object.
   Example:
   (ts:setw (Checkbutton (Name 'cbtn) (Text "Expert Mode")
                         (Command "hide_unhide_controls")))
   (Checkbutton (Name "cbtn") (Text "Expert Mode")
                (Command "hide_unhide_controls"))
   will be translated to:
   ttk::checkbutton .cbtn -text "Expert Mode" -command {puts \"(MAIN:hide_unhide_controls)\"}
   [/text]
   (let (name-string ""  widget-string "" var nil  option-string "")
      (setq name-string  (:build-tk-name (self))) 
      (when (assoc Text (self))        ;button Text
         (setq option-string
               (string " -text" 
                " \"" (last (assoc Text (self))) "\""  ))
         (setq widget-string
               (string "ttk::checkbutton " name-string option-string))
         );when
      (when (assoc Variable (self))
         (setq option-string
               (string " -variable " (last (assoc Variable (self))) ))
         (setq widget-string (string widget-string option-string))
         );when
      (when (assoc Width (self))
         (setq option-string
               (string " -width " (last (assoc Width (self))) ))
         (setq widget-string (string widget-string option-string))
         );when
      (when (assoc State (self))
         (setq option-string
               (string " -state " (last (assoc State (self))) ))
         (setq widget-string (string widget-string option-string))
         );when
      (when (assoc Command (self))
         (setq command-string            
               (string " -command {puts \"(MAIN:" 
                (last (assoc Command (self))) ;a function w/o args
                ")\"}") )
         (setq widget-string (string widget-string command-string))
         );when
      ;; (println "Checkbutton:build.widget-string:" nl widget-string) 
      (Tk widget-string) ; ==> send to Tk
      );let
   );Checkbutton:build


(define (Checkbutton:select-it)
   "pre-select the Checkbutton (self)"
      (if (assoc Variable (self))
          (begin (Tk "set " (last (assoc Variable (self))) " 1"))
          (begin (throw-error "Checkbutton:select-it - no variable defined"))
         );if
      )


(define (Checkbutton:deselect-it)
   "pre-de-select the Checkbutton (self)"
      (if (assoc Variable (self))
          (begin (Tk "set " (last (assoc Variable (self))) " 0"))
          (begin (throw-error "Checkbutton:deselect-it - no variable defined"))
          );if
      )


## -------------------------------------------------------------------
(new 'Window 'Labelframe)
(define (Labelframe:build)
   ;; width: characters
   ;; height: pixels
   ;; ttk::labelframe .lbframe -text Title -labelanchor n -width 100 -height 50
   ;; ttk::label .lbframe.lb -text "A Label"
   ;; 
   ;; (ts:setw (Labelframe (Name 'lf) (Text "Title")
   ;;                      (Labelanchor "nw")
   ;;                      ;; (Width 400) (Height 250)
   ;;                      ))
   ;; (:build lf)
   (let (name-string "" widget-string "" option-string "" 
                     labelanchor-string "" )
      (setq name-string (:build-tk-name (self)))
      (setq widget-string
               (string "ttk::labelframe " name-string ))
      (when (assoc Text (self))
         (setq option-string
               (string " -text "
                       "\"" (last (assoc Text (self))) "\"" ))
         (setq widget-string (string widget-string option-string))
         );when Text
      (when (assoc Width (self))
         (setq option-string
               (string " -width " (last (assoc Width (self))) ))
         (setq widget-string (string widget-string option-string))
         );when Width
      (when (assoc Height (self))
         (setq option-string
               (string " -height " (last (assoc Height (self))) ))
         (setq widget-string (string widget-string option-string))
         );when Height
      ;; -labelanchor
      (if (assoc Labelanchor (self))  ;default: nw
          (begin
             (setq labelanchor-string
                   (string " -labelanchor "
                           (last (assoc Labelanchor (self))) )))
          (begin
             (setq labelanchor-string (string " -labelanchor nw" ))) ; default
          );if
      (setq widget-string
            (string widget-string labelanchor-string))
      ;; (println "Labelframe:build.widget-string:" nl widget-string) 
      (Tk widget-string) ; ==> send to Tk
      ));Labelframe:build


## -------------------------------------------------------------------
(new 'Window 'Frame)
(define (Frame:build)
   "simple frame to contain other widgets. In newLISP-tk: No options possible here."
   ;; ttk::frame .fr -padding {30 20 10 0} -borderwidth 5 -relief solid -cursor hand2
   ;; (ts:setw (Frame (Name 'fr)))
   (let (name-string ""   widget-string ""   )
      (setq name-string (:build-tk-name (self)))
      (setq widget-string
            (string "ttk::frame " name-string ))
      ;; (println "Frame:build.widget-string:" nl widget-string) 
      (Tk widget-string) ; ==> send to Tk
      ));Frame:build


## -------------------------------------------------------------------
(new 'Window 'Scrolledtext)
(define (Scrolledtext:build)
   "define a text widget with scrollbars. Package require >Iwidgets<"
   ;; Parent, Name, Wrap, State
   ;; (ts:setw (Scrolledtext (Parent 'fr) (Name 'tx) ; (Width 45) (Height 15)
   ;;                        (Wrap "word") ; none, char, word
   ;;                        (State "normal")))
   ;; (:build tx)
   ;; (:setgrid tx (Padx 10) (Pady 10) (Sticky "nesw"))
   (let (name-string ""  widget-string ""
                     option-string "" wrap-string   ""  )
      (setq name-string (:build-tk-name (self)))
      ;; package require Iwidgets
      ;; iwidgets::scrolledtext .t -wrap none
      (setq widget-string
            (string "package require Iwidgets\n"
                    "iwidgets::scrolledtext " name-string ))
      (when (assoc Wrap (self))
         (setq wrap-string
               (string " -wrap " (last (assoc Wrap (self)))))
         (setq widget-string (string widget-string wrap-string))
         );when
      (when (assoc Width (self))
         (setq option-string
               (string " -width " (last (assoc Width (self)))))
         (setq widget-string (string widget-string option-string))
         );when
      (when (assoc Height (self))
         (setq option-string
               (string " -height " (last (assoc Height (self)))))
         (setq widget-string (string widget-string option-string))
         );when
      (when (assoc State (self))
         (setq option-string
               (string " -state " (last (assoc State (self)))))
         (setq widget-string (string widget-string option-string))
         );when
      ;; (println "Scrolledtext:build.widget-string:" nl widget-string) 
      (Tk widget-string) ; ==> send to Tk
      ));Scrolledtext:build

## -------------------------------------------------------------------
## not yet done:
(new 'Window 'Radiobutton)
(new 'Window 'Menubutton)
(new 'Window 'Listbox)
(new 'Window 'Combobox)
(new 'Window 'Notebook)

## ------------------------------------------------------------------------
## Parameters/Options as Classes:
(new Class 'Name) 
(define (Name:Name )                    ;Name-Constructor
   [text]
   widget name, make sure it will come out as a string.
   (Name 'Hugo) -> (Name \"Hugo\")
   [/text]
   (let (l '())
      (setq l
            (map (lambda (x)
                    (or (and (symbol? x)
                             (term x))     ;make term if it's a symbol or
                        x))                ;leave it as it is
                 (args)))
      (cons (context) l)));Name:Name
#;; > (ts:setw (Label (Name "lb1") (Text "Feld 1"))) ; string
#;; (Label (Name "lb1") (Text "Feld 1"))             ; Name is a string
#;; > (ts:setw (Label (Name 'lb1) (Text "Feld 1")))  ; symbol
#;; (Label (Name "lb1") (Text "Feld 1"))             ; Name is a string


(new Class 'Variable)
(define (Variable:Variable )                    ;Variable-Constructor
   [text]
   widget name, make sure it will come out as a string.
   (Variable 'Hugo) -> (Variable \"Hugo\")
   [/text]
   (let (l '())
      (setq l
            (map (lambda (x)
                    (or (and (symbol? x)
                             (term x))     ;make term if it's a symbol or
                        x))                ;leave it as it is
                 (args)))
      (cons (context) l)));Variable:Variable


(new Class 'Parent)
(define (Parent:Parent)
   [text]
   widget name, make sure it will come out as a symbol.
   (Parent "Hugo") -> (Parent Hugo)
   [/text]
   (let (l '())
      (setq l
            (map (lambda (x)
                    (or (and (string? x)
                             (sym x 'MAIN)) ;make symbol if it's a string
                        x))                 ;or else leave it as it is
                 (args)))
      (cons (context) l)));Parent:Parent


## --- Parameters/Options as Classes, continued --------------------------
(new Class 'Width)
(new Class 'Height)
(new Class 'Title)
(new Class 'Text)
(new Class 'Textvariable)
(new Class 'Padx)
(new Class 'Pady)
(new Class 'Grid)
(new Class 'Column)
(new Class 'Columnspan)
(new Class 'Row)
(new Class 'Rowspan)
(new Class 'Minsize)
(new Class 'Maxsize)
(new Class 'Command)
(new Class 'State)
(new Class 'Relief)
(new Class 'Borderwidth)
(new Class 'Labelanchor)
(new Class 'Sticky)
(new Class 'Wrap)


## ----------- ts - LOCAL FUNCTIONS --------------------------------------
(context 'ts)

;; use widget-variables to retrieve their actual values
;; Use a dictionary for these variables.
;; Then we can see both: variable name and it's value
(define TkVar:TkVar)    ;create a hash-table, holding all Tk-variables
(define (ts:setVar name val)
   "send name and set it's value to `val` "
   (if (not (and (string? name) (string? val)))
       (throw-error "ts:setVar arguments must both be of type string"))
   (TkVar name val)                     ;now we know it's value
   (Tk "set " name " " val)             ;send it to Tk 
   );ts:setVar


(define (ts:getVar name )
   "send name and get it's actual value from Tk"
   (let (cl "" )
      (MAIN:assert (string? name) "ts:getVar argument must be a string")
      (Tk (string "puts \"(setq hh {$" name "})\"")) ;Tk
      (Tk:wait)             ;wait for the answer
      (eval-string  (current-line))
      (TkVar name hh)               ;remember result
      hh
      ));ts:getVar


##-----------------------------------------------------------------------
;; @syntax (ts:setw <class-widget-definition>) 
;; @param  <class-widget-definition> contains a list of nested objects.
;; 
;; Method <ts:setw> "set widget" creates a newLISP symbol derived from the string given to
;; Class 'Name.
;; 
;; @example
;; define a label with name "lb1"
;; > (ts:setw (Label (Name "lb1") (Text "Feld 1"))) ;define label with name lb1
;; (Label (Name "lb1") (Text "Feld 1"))
;; 
;; > lb1            ; newLISP-variable
;; (Label (Name "lb1") (Text "Feld 1"))
;; 
;; > (ts:setw (Window (Name ".") (Title "Test-GUI") (Minsize (Width 720) (Height 250))))
;; (Window (Name ".") (Title "Test-GUI") (Minsize (Width 720) (Height 250)))
;; > .
;; (Window (Name ".") (Title "Test-GUI") (Minsize (Width 720) (Height 250)))
##--------------------------------------------------------------------
(define (ts:setw obj)
   [text]
   ts:setw will use the Name-obj in <obj>, if any,
   to establish a newLISP-variable with the same name and obj as value.
   [/text]
   (let (nme nil) 
      (if (set 'nme (assoc Name obj)) ;nil otherwise
          (begin (set (sym (last nme) MAIN) obj))
          (begin
             (println nl "ERROR: object given: " nl obj)
             (throw-error "ts:setw - no name given"))
         )));ts:setw


##--------------------------------------------------------------------
(define (ts:ask-directory)
   "Use the Tcl/Tk-chooseDirectory-dialog to ask for a directory-path"
   (let (A ""  tcl-string "" cl "")
      (setq tcl-string
            (string
             "set Dirname [tk_chooseDirectory ]; puts \"(setq A {$Dirname})\"" 
             ))
      ;;(println "ts:ask-directory.tcl-string:" nl tcl-string)
      (Tk tcl-string)
      ##
      (Tk:wait)
      ##
      (eval-string (current-line))      ;eval answer
      A
      );let
   );ts:ask-directory


(define (ts:quit)
   (Tk "puts \"(exit)\"; exit")
   ;; (Tk "destroy .")
   )


(define (ts:xquit)
   (Tk "bind . <Destroy> {puts {(exit)}}") ; quit using 'x'
   )


(println "loading ts: " 'done)

##(save "wsp.lsp")

(context MAIN)

