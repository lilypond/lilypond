;; lilypond-font-lock.el

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 1997: Han-Wen Nienhuys
;; Author: 1995-1996 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Created:       Feb 1992
;; Version:       0.0
;; Last Modified: 12SEP97
;; Keywords: lilypond languages music notation

;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;; This started out as a cannabalised version of python-mode.el, by hwn
;; For changes see the LilyPond ChangeLog
;;
;; TODO:
;;   - should handle block comments too.
;;   - handle lexer modes (\header, \melodic, \lyric) etc.
;;   - indentation
;;   - notenames?
;;   - fontlock: \melodic \melodic

(defconst LilyPond-font-lock-keywords
  (let* ((keywords '(

"apply" "arpeggio" "autochange" "spanrequest" "commandspanrequest"
"simultaneous" "sequential" "accepts" "alternative" "bar" "breathe"
"char" "chordmodifiers" "chords" "clef" "cm" "consists" "consistsend"
"context" "denies" "duration" "dynamicscript" "elementdescriptions"
"font" "grace" "header" "in" "lyrics" "key" "mark" "musicalpitch"
"time" "times" "midi" "mm" "name" "notenames" "notes" "outputproperty"
"override" "set" "revert" "partial" "paper" "penalty" "property" "pt"
"relative" "remove" "repeat" "addlyrics" "partcombine" "score"
"script" "stylesheet" "skip" "textscript" "tempo" "translator"
"transpose" "type" "unset" 
		      ))
       (kwregex (mapconcat (lambda (x) (concat "\\\\" x))  keywords "\\|")))

    (list 
      (concat ".\\(" kwregex "\\)[^a-zA-Z]")
      (concat "^\\(" kwregex "\\)[^a-zA-Z]")
      '(".\\(\\\\[a-zA-Z][a-zA-Z]*\\)" 1 font-lock-variable-name-face)
      '("^[\t ]*\\([a-zA-Z][_a-zA-Z]*\\) *=" 1 font-lock-variable-name-face)     
    ))
  "Additional expressions to highlight in LilyPond mode.")

;; define a mode-specific abbrev table for those who use such things
(defvar LilyPond-mode-abbrev-table nil
  "Abbrev table in use in `LilyPond-mode' buffers.")

(define-abbrev-table 'LilyPond-mode-abbrev-table nil)

(defvar LilyPond-mode-syntax-table nil
  "Syntax table used in `LilyPond-mode' buffers.")

;;
(if LilyPond-mode-syntax-table
    ()
  (setq LilyPond-mode-syntax-table (make-syntax-table))
  (mapcar (function
	   (lambda (x) (modify-syntax-entry
			(car x) (cdr x) LilyPond-mode-syntax-table)))
	  '(( ?\( . "()" ) ( ?\) . ")(" )   ; need matching parens for inline lisp
 	    ( ?\[ . "." ) ( ?\] . "." )
	    ( ?\{ . "(}" ) ( ?\} . "){" )
	    ( ?\< . "(>" )( ?\> . ")>") 
	    ( ?\$ . "." ) ( ?\% . "." ) ( ?\& . "." )
	    ( ?\* . "." ) ( ?\+ . "." ) ( ?\- . "." )
	    ( ?\/ . "." )  ( ?\= . "." )
	    ( ?\| . "." ) (?\\ . "\\" )
	    ( ?\_ . "." )	
	    ( ?\' . "w")	
	    ( ?\" . "\"" )
	    ( ?\% . "<")
	    ( ?\n . ">")

; FIXME
;	    ( ?%  .  ". 124b" )
;	    ( ?{  .  ". 23" )
	    ))

  )	

