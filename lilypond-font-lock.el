;;; lilypond-font-lock.el --- syntax coloring for LilyPond mode

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2001: Heikki Junes
;;  * Emacs-mode: new keywords, reserved words, identifiers, notenames, 
;;    some dynamics and brackets are font-lock-keywords
;; Author: 1997: Han-Wen Nienhuys
;; Author: 1995-1996 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Created:       Feb 1992
;; Version:       0.0
;; Last Modified: 14SEP2001
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
;;   - handle lexer modes (\header, \melodic, \lyric) etc.

(defconst LilyPond-font-lock-keywords
  (let* ((keywords '( ; need special order due to over[lapping] of words

;; all letters are lowercase
"accepts" "accompany" "addlyrics" "aeolian" "alias" "alternative"
"apply" "arpeggio" "autochange" "bar" "break" "breathe" "breve"
"beamintervals" "broken" "blend" "bcincipit" "char" "ch" "cg"
"chord\\(s\\|stest\\|chordmodifiers\\)?"
"clef \\(bass\\|treble\\|violin\\|tenor\\)?"
"clipping" "cm" "coda" "complex" "commandspanrequest" "consists\\(end\\)?"
"context" "contrabasso" "decr" "default" "denies" "different" "dirs"
"down\\(bow\\|prall\\)?" "dorian" "duration" "dynamicscript"
"eccentric" "eg" "embeddedps" "endincipit" "elementdescriptions"
"ex\\(treme\\)?" "fermata" "f+" "font" "flageolet" "fp" "fragment" "fz"
"gliss\\(ando\\)?" "global" "gg" "gmsus" "grace" "gr\\(and\\)?staff"
"header" "hsize" "in\\(clude\\|versions\\|visible\\)?" "ionian"
"key\\(s\\(ignature\\)?\\)?" "lag" "lheel" "line\\(break\\|prall\\)"
"locrian" "longa" "lower" "ltoe" "lydian" "lyrics"
"maintainer" "mark" "maxima" "mel\\(isma\\|ody\\)?" "midi" "major"
"minor" "mixolydian" "mordent" "monstrous" "multipart" "music"
"musicalpitch" "m\\(p\\|f\\|m\\)?" "name" "newpage" "noise\\(beat\\)?"
"normal\\(key\\|size\\)" "note\\(name\\)?s" "nt?"
"one\\(staff\\)?" "open" "outputproperty" "over\\(ride\\)?"
"paper" "partcombine" "partial" "penalty" "phrygian" "pitch" "p+"
"property" "pt" "prall\\(mordent\\|prall\\|up\\)?" "quickmeasure"
"relative" "remove" "repeat" "rever\\(t\\|seturn\\)" "rf" "rheel" "rhythm"
"right" "rtoe" "scales?" "scheme" "score" "scpaper" "script" "scscore" "sd"
"segno" "sequential" "set\\(tings\\)?" "sf\\(f\\|z\\)?" "shortlong"
"simultaneous" "singlepart" "skip" "small" "smarttranspose" "spanrequest"
"staccato" "staff\\(height\\|space\\)" "start" "stop\\(ped\\)?"
"st\\(paper\\|score\\)" "stuff" "stylesheet" "su" "tab" "tempo" "tenuto" 
"textscript" "thenotes" "thrd" "threevoice" "thumb" "tilt\\(down\\|up\\)" 
"timb" "times?" "timpani" "tiny" "toeters" "touch" "trans\\(lator\\|pose\\)" 
"trill" "trombe" "turn" "type" "t\\(wo\\(voice\\(steminvert\\)?\\)?\\)?" 
"un\\(der\\|set\\)" "up\\(bow\\|per\\|prall\\)?" "version" 
"viol\\(a\\|in\\(incipit\\)?\\|oncello\\)" "visible" "voicedefault" "vsize"
"x" "zagers?" "z\\(eu\\|o\\)ger"

		      ))

  (identifiers '( 

;; in principle, have one or more uppercase letters
"\\(\\(BarNumbering\\|Choir\\|Grand\\|HaraKiri\\|OrchestralPart\\|Piano\\|Rhythmic\\)Staff\\|\\(Cue\\|Lyrics\\)?Voice\\|\\(Orchestral\\)?Score\\|ChordNames\\|Grace\\|Lyrics\\|StaffGroup\\|Thread\\)Context" ; *Context
"\\(script\\|dots\\|dynamic\\|slur\\|stem\\|sustain\\|tie\\|tuplet\\)\\(Both\\|Down\\|Up\\)" ; *(Both/Down/Up)
"\\(slur\\|tie\\)\\(Dotted\\|Solid\\)" ; *(Dotted/Solid)
"\\(autoBeam\\|cadenza\\|impro\\|turn\\)\\(Off\\|On\\)" ; *(On/Off)
"\\(empty\\|fat\\)Text" ; *Text
"shift\\(On+\\|Off\\|I\\|II\\|III\\|IV\\|V\\)" ; shift*
"EasyNotation"
"\\(hide\\|show\\)StaffSwitch"
"\\(lower\\|upper\\)Voice"
"voice\\(One\\|Two\\|Three\\|Four\\|B\\|C\\|D\\|E\\)" ; voice*
"paper\\(Eleven\\|Sixteen\\|Thirteen\\|TwentySix\\)" ; paper*
"\\(lower\\|upper\\)\\(Octave\\|One\\)" ; (lower/upper)*
"hairyChord"
"\\(Piano\\|Rhythmic\\)\\(Staff\\)?"
"\\(clarinetti\\|fagotti\\|flauti\\|melodic\\|oboi\\|\\(quite\\|rather\\|somewhat\\)LongLyrics\\|violinoII?\\)?\\(Staff\\)?" ; *Staff
"\\(archi\\|bassi\\|legni\\|ottoni\\|timpani\\|viole\\|violini\\)\\(Group\\)" ; *Group
"melisma\\(End\\)?" "staff\\(One\\|Two\\)?" "rests\\(II\\)?" "specialKey"
"noBreak" "paperTwentysix" "endHorizScript" "FontBody" "text(I)+"

                      ))

  (reservedwords '(

;; Other words which look nicer when colored
"Accidentals" "autoBeamSettings" "BarLine" "Beam"
"ChordName\\([s]?\\|s.[a-zA-Z]*\\)" "Grace\\(.[a-zA-Z]*\\)?"
"\\(Grand\\|Piano\\)Staff" "Lyrics\\(.[a-zA-Z]*\\)?" "NoteHead" 
"Score\\(.[a-zA-Z]*\\)" "Stem" "Staff\\(Symbol\\)?" "TextScript" 
"TimeSignature" "Voice\\(.[a-zA-Z]*\\)?"

		      ))

       (kwregex (mapconcat (lambda (x) (concat "\\\\" x))  keywords "\\|"))
       (iregex (mapconcat (lambda (x) (concat "\\\\" x))  identifiers "\\|"))
       (rwregex (mapconcat (lambda (x) (concat "" x))  reservedwords "\\|"))
)

    (list 
;; Fonts in use (from GNU Emacs Lisp Reference Manual, elisp.ps):
;; font-lock- comment / string / keyword / builtin / function-name / 
;;            variable-name / type / constant / warning -face

      '("\\([_^]?\\\\[a-zA-Z][a-zA-Z]*\\)" 1 font-lock-constant-face)
      '("\\(\\(#'\\)?[a-zA-Z][_a-zA-Z.\-]*[ \t]*=[ \t]*#\\)" 1 font-lock-variable-name-face)
      '("\\([a-zA-Z][_a-zA-Z.\-]*\\)[ \t]*=[ \t]*" 1 font-lock-variable-name-face)
      '("[ \t]*=[ \t]*\\([a-zA-Z][_a-zA-Z]*\\)" 1 font-lock-variable-name-face)


;; other reserved words
      (cons (concat "\\(" rwregex "\\)") 'font-lock-variable-name-face)

;; highlight note names; separate notes from (other than ')'-type) brackets
      '("[ <\{[~()\t]\\(\\(\\(\\(do\\|re\\|mi\\|fa\\|sol\\|la\\|si\\)\\(bb?\\|dd?\\|ss?\\)?\\)\\|\\([a-hsr]\\(flat\\(flat\\)?\\|sharp\\(sharp\\)?\\|ff?\\|ss?\\|is\\(siss\\|s\\|is\\)?\\|es\\(sess\\|s\\|es\\)?\\)?\\)\\|\\(as\\(as\\|es\\)?\\)\\|\\(es\\(es\\)?\\)\\|\\(bb\\)\\)[,']*\\(64\\|32\\|16\\|8\\|4\\|2\\|1\\)?[.]*\\)" 1 font-lock-type-face)

;; highlight identifiers
      (cons (concat "\\([_^]?\\(" iregex "\\)\\)+\\($\\|[] \t(~{}>\\\\]\\)") '(0 font-lock-function-name-face t))

;; highlight keywords
      (cons (concat "\\([_^]?\\(" kwregex "\\)\\)+\\($\\|[] \t(~{}>\\\\]\\)") '(0 font-lock-keyword-face t))

;; highlight bracketing constructs
      '("\\([][}{]\\)" 0 font-lock-warning-face t)
;; these regexps allow angle-brackets to be highlighted,
;; but leave accented notes, e.g. a b c->, alone
      '("[^\\]\\(<\\)" 1 font-lock-warning-face t)
      '("[_^-]\\s-*[-^]\\s-*\\(>\\)" 1 font-lock-warning-face t)
      '("[^\\t\\n _^-]\\s-*\\(>\\)" 1 font-lock-warning-face t)

      '("\\([(~)]\\|\\\\<\\|\\\\!\\|\\\\>\\)" 0 font-lock-builtin-face t)

;; highlight comments (again)
      '("\\(%.*\\)" 0 font-lock-comment-face t)

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
	  '(( ?\( . "." ) ( ?\) . "." ) 
	    ( ?\[ . "." ) ( ?\] . "." )
	    ( ?\{  .  "(}2b" )
	    ( ?\}  .  "){4b" )
	    ( ?\< . "." )( ?\> . ".") 
	    ( ?\$ . "." ) ( ?\% . "." ) ( ?\& . "." )
	    ( ?\* . "." ) ( ?\+ . "." )
	    ( ?\/ . "." )  ( ?\= . "." )
	    ( ?\| . "." ) (?\\ . "\\" )
	    ( ?\- . "." ) ( ?\_ . "." ) ( ?\^ . "." )
	    ( ?\' . "w")	
	    ( ?\" . "\"" )
	    ( ?\%  .  ". 1b3b" )
	    ( ?\n . ">")
	    ( ?\r . ">")
	    ))
  )

