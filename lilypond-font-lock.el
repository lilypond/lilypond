;;; lilypond-font-lock.el --- syntax coloring for LilyPond mode

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2001: Heikki Junes
;;  * Emacs-mode: new keywords, reserved words, notenames and brackets are
;;    font-lock-keywords; implementation encourages spacing/indenting.
;; Author: 1997: Han-Wen Nienhuys
;; Author: 1995-1996 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Created:       Feb 1992
;; Version:       0.0
;; Last Modified: 1SEP2001
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

(defconst LilyPond-font-lock-keywords
  (let* ((keywords '( ; need special order due to over[lapping] of words

"accepts" "addlyrics" "alternative" "apply" "arpeggio" "autoBeamOff"
"autoBeamOn" "autochange" "bar" "BarNumberingStaffContext" "break"
"breathe" "breve" "cadenzaOn" "cadenzaOff" "char" "chord" "chordmodifiers"
"ChordNamesContext" "chordstest" "chords" "clef" "cm" "commandspanrequest"
"consistsend" "consists" "context" "default" "denies" "different"
"dotsBoth" "dotsDown" "dotsUp" "duration" "dynamicscript" "dynamicUp"
"dynamicDown" "dynamicBoth" "EasyNotation" "elementdescriptions"
"emptyText" "extreme" "ex" "fatText" "fermata" "fff" "ff" "f" "font" "foo"
"glissando" "gliss" "grace" "grstaff" "hairyChord" "HaraKiriStaffContext"
"header" "hideStaffSwitch" "include" "in" "key" "linebreak" "longa"
"lyrics" "LyricsContext" "LyricsVoiceContext" "major" "mark" "melismaEnd"
"melisma" "midi" "minor" "mm" "musicalpitch" "m" "name" "newpage"
"noBreak" "noisebeat" "noise" "normalkey" "normalsize" "notenames" "notes"
"n" "onestaff" "oneVoice" "one" "OrchestralScoreContext" "outputproperty"
"override" "paperTwentysix" "paper" "partcombine" "partial" "penalty"
"PianoStaffContext" "pp" "property" "pt" "p" "relative" "remove" "repeat"
"restsII" "rests" "revert" "rhythm" "right" "scales" "scale" "scheme"
"score" "ScoreContext" "scpaper" "scriptBoth" "scriptDown" "scriptUp"
"script" "scscore" "sd" "sequential" "settings" "set" "sfz" "shitfOnnn"
"shitfOnn" "shitfOn" "shitfOff" "showStaffSwitch" "simultaneous" "skip"
"slurBoth" "slurDown" "slurUp" "slurDotted" "slurSolid" "small"
"spanrequest" "specialKey" "staccato" "StaffContext" "staffspace"
"stemBoth" "stemDown" "stemUp" "stpaper" "stscore" "stylesheet" "su"
"tempo" "tenuto" "textII" "textI" "textscript" "thenotes" "ThreadContext"
"threevoice" "tieBoth" "tieDown" "tieDotted" "tieSolid" "tieUp" "times"
"time" "tiny" "touch" "translator" "transpose" "tupletBoth" "tupletDown"
"tupletUp" "twovoicesteminvert" "twovoice" "two" "turnOff" "type" "t"
"unset" "version" "voiceB" "VoiceContext" "voiceC" "voiceD" "voiceE"
"voiceOne" "voiceTwo" "voiceThree" "voiceFour" "zagers" "zager" "zoger"

		      ))

  (reservedwords '(

"bass" "treble" "PianoStaff"

		      ))

       (kwregex (mapconcat (lambda (x) (concat "\\\\" x))  keywords "\\|"))
       (rwregex (mapconcat (lambda (x) (concat "" x))  reservedwords "\\|"))
)

    (list 
;; Fonts in use (from GNU Emacs Lisp Reference Manual, elisp.ps):
;; font-lock- comment / string / keyword / builtin / function-name / 
;;            variable-name / type / constant / warning -face

;; Using extra spaces was both easier to parse and looks better!
;; highlight note grouping brackets; space around these { [ < brackets > ] }
;;   make the text look {less[<messyand>]erronous}
      '("\\([<{[]\\)[ \t]" 1 font-lock-warning-face)
      '("\\([\]}>]\\)[ \t]" 1 font-lock-warning-face)
      '("^\\([<{[]\\)[ \t]" 1 font-lock-warning-face)
      '("^\\([\]}>]\\)[ \t]" 1 font-lock-warning-face)
      '("\\([<{[]\\)$" 1 font-lock-warning-face)
      '("\\([\]}>]\\)$" 1 font-lock-warning-face)

;; highlight keywords; space after[ ]these commands /increases/readability
      (concat "\\([_^]?\\(" kwregex "\\)\\)[ \t(]")
      (concat "\\([_^]?\\(" kwregex "\\)\\)$")
      '("\\([_^]?\\\\[a-zA-Z][a-zA-Z]*\\)" 1 font-lock-constant-face)
      '("\\([a-zA-Z][_a-zA-Z]*\\)[ \t]*=[ \t]*" 1 font-lock-variable-name-face)
      '("[ \t]*=[ \t]*\\([a-zA-Z][_a-zA-Z]*\\)[ \t(]" 1 font-lock-variable-name-face)
      '("[ \t]*=[ \t]*\\([a-zA-Z][_a-zA-Z]*\\)$" 1 font-lock-variable-name-face)

;; other reserved words
      (cons (concat "\\(" rwregex "\\) ") 'font-lock-variable-name-face)
      (cons (concat "\\(" rwregex "\\)$") 'font-lock-variable-name-face)

;; highlight note names; separate notes from (other than ')'-type) brackets
      '("[ )\t]\\(\\(\\(\\(do\\|re\\|mi\\|fa\\|sol\\|la\\|si\\)\\(b\\|bb\\|d\\|dd\\|s\\|ss\\)?\\)\\|\\([a-hsr]\\(f\\|ff\\|s\\|ss\\|flat\\|flatflat\\|sharp\\|sharpsharp\\|is[s]?\\|is[s]?is[s]?\\|es[s]?\\|es[s]?es[s]?\\)?\\)\\|\\(as\\(as\\|es\\)?\\)\\|\\(es\\(ses\\)?\\)\\|\\(bb\\)\\)[,']*\\(64\\|32\\|16\\|8\\|4\\|2\\|1\\)?[.]*\\)" 1 font-lock-type-face)

      '("\\([(~)]\\)" 1 font-lock-builtin-face)

      )
    )
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

