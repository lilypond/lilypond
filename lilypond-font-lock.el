;;; lilypond-font-lock.el --- syntax coloring for LilyPond mode

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2001: Heikki Junes
;;  * Emacs-mode: new keywords, reserved words, identifiers, notenames, 
;;    some dynamics and brackets are font-lock-keywords
;; Author: 1997: Han-Wen Nienhuys
;; Author: 1995-1996 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Created:       Feb 1992
;; Version:       1.5.50
;; Last Modified: 6APR2002
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
"accent" "accepts" "accompany" "\\(add\\)?lyrics" 
"\\(aeol\\|dor\\|ion\\|locr\\|\\(mixo\\)?lyd\\|phryg\\)ian" 
"alias" "\\(altern\\|rel\\)ative" "apply" "arpeggio" "autochange" "bar" "break"
"breathe" "breve" "beamintervals" "broken" "blend" "\\(bc\\|end\\)incipit" 
"ch\\(ar\\)?" "cg" "chord\\(s\\|stest\\|chordmodifiers\\)?"
"clef[ \t]*\\(F\\|G\\|alto\\|baritone\\|bass\\|\\(mezzo\\)?soprano\\|treble\\|violin\\|tenor\\)?"
"clipping" "cm" "coda" "complex" 
"\\(command\\)?spanrequest" "consists\\(end\\)?"
"context" "contrabasso" "\\(de\\)?cr" "default" "denies" "different" "dirs"
"down\\(bow\\|prall\\)?" "duration" "\\(dynamic\\)?script"
"eccentric" "eg" "embeddedps" "elementdescriptions"
"ex\\(treme\\)?" "fermata" "f+" "figures" "font" "flageolet" "fp" "fragment" 
"fz" "gliss\\(ando\\)?" "gg" "gmsus" "grace" "gr\\(and\\)?staff"
"header" "\\(h\\|v\\)size" "in\\(clude\\|versions\\|visible\\)?" 
"key\\(s\\(ignature\\)?\\)?" "lag" "\\(l\\|r\\)heel" "line\\(break\\|prall\\)"
"longa" "lower" "\\(l\\|r\\)toe"
"mark" "marcato" "maxima" "mel\\(isma\\|ody\\)?" "midi" "m\\(aj\\|in\\)or"
"\\(up\\|down\\)?mordent" "monstrous" "multipart" "music"
"\\(musical\\)?pitch" "m\\(p\\|f\\|m\\)?" "name" "newpage" "noise\\(beat\\)?"
"normal\\(key\\|size\\)" "note\\(name\\)?s" "nt?"
"one\\(staff\\)?" "open" "\\(output\\)?property" "over\\(ride\\)?"
"part\\(combine\\|ial\\)" "penalty" "p+" "pt" 
"prall\\(down\\|mordent\\|prall\\|up\\)?" "quickmeasure" "rc\\(ed\\)?" "remove"
"repeat[ \t]*\\(\\(un\\)?fold\\|percent\\|\\|tremolo\\|volta\\)?" "rest"
"revert" "\\(reverse\\)?turn" "rf\\(z\\)?" "rhythm"
"right" "scales?" "scheme" "\\(sc\\)?paper" "\\(sc\\)?score" "sd"
"segno" "sequential" "set\\(tings\\)?" "sf\\(f\\|z\\)?" "shortlong"
"simultaneous" "singlepart" "skip" "small" "\\(smart\\)?transpose"
"sp\\(p\\)?" "staccat\\(issim\\)?o" "staff\\(height\\|space\\)" "start" 
"stop\\(ped\\)?"
"st\\(paper\\|score\\)" "stuff" "stylesheet" "su" "tab" "tempo" "tenuto" 
"textscript" "thenotes" "thrd" "threevoice" "thumb" "tilt\\(down\\|up\\)" 
"timb" "times?" "tiny" "toeters" "touch" "translator" 
"trill" "type" "t\\(wo\\(voice\\(steminvert\\)?\\)?\\)?" 
"un\\(der\\|set\\)" "up\\(bow\\|per\\|prall\\)?" "version" 
"visible" "voicedefault" "x"

		      ))

  (identifiers '( 

;; in principle, have one or more uppercase letters
"\\(\\(BarNumbering\\|Choir\\|Grand\\|HaraKiri\\|OrchestralPart\\|Piano\\|Rhythmic\\)?Staff\\|\\(Cue\\|Lyrics\\)?Voice\\|\\(Orchestral\\)?Score\\|ChordNames\\|Grace\\|Lyrics\\|Staff\\(Group\\)?\\|Thread\\)Context" ; *Context
"\\(script\\|dots\\|dynamic\\|slur\\|stem\\|sustain\\|sostenuto\\|unaCorda\\|treCorde\\|tie\\|tuplet\\)\\(Both\\|Down\\|Up\\)" ; *(Both/Down/Up)
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
"hairyChord" "\\(Piano\\|Rhythmic\\)\\(Staff\\)?"
"\\(clarinetti\\|fagotti\\|flauti\\|melodic\\|oboi\\|\\(quite\\|rather\\|somewhat\\)LongLyrics\\|violinoII?\\)?\\(Staff\\)?" ; *Staff
"\\(archi\\|bassi\\|legni\\|ottoni\\|timpani\\|viole\\|violini\\)\\(Group\\)" ; *Group
"melisma\\(End\\)?" "staff\\(One\\|Two\\)?" "rests\\(II\\)?" "specialKey"
"noBreak" "paperTwentysix" "endHorizScript" "FontBody" "text(I)+"
"\\(modern\\|forget\\)Accidentals" ; *Accidentals
"noResetKey" "modern\\(Voice\\)?Cautionaries" "unaCorda" "treCorde"

                      ))

  (reservedwords '(

;; Other words which look nicer when colored
"Accidentals" "autoBeamSettings" "BarLine" "Beam"
"ChordName\\([s]?\\|s.[a-zA-Z]*\\)" "DynamicText"
"FiguredBass" "Hairpin" "\\(Grand\\|Piano\\)Staff"
"Slur" "Stem" "SpacingSpanner" "System\\(StartDelimiter\\)?"
"\\(Grace\\|Lyrics\\|Note\\(Head\\|Names\\)\\|Score\\|\\(Rhythmic\\)?Staff\\(Symbol\\)?\\|Thread\\|Voice\\)\\(.[a-zA-Z]*\\)?" ; combine below, if possible
"\\(Grace\\|Lyrics\\|Note\\(Head\\|Names\\)\\|Score\\|\\(Rhythmic\\)?Staff\\(Symbol\\)?\\|Thread\\|Voice\\)[ \t]*\\(.[ \t]*[a-zA-Z]*\\)?" 
"TextScript" "TimeSignature" "VerticalAlignment"

		      ))

       (kwregex (mapconcat (lambda (x) (concat "\\\\" x))  keywords "\\|"))
       (iregex (mapconcat (lambda (x) (concat "\\\\" x))  identifiers "\\|"))
       (rwregex (mapconcat (lambda (x) (concat "" x))  reservedwords "\\|"))
)

    (list 
;; Fonts in use (from GNU Emacs Lisp Reference Manual, elisp.ps):
;; font-lock- comment / string / keyword / builtin / function-name / 
;;            variable-name / type / constant / warning -face

      '("\\([_^-]?\\\\[a-zA-Z][a-zA-Z]*\\)" 1 font-lock-constant-face)
      '("\\(\\(#'\\)?[a-zA-Z][_a-zA-Z.\-]*[ \t]*=[ \t]*#\\(#f\\|#t\\)\\)" 1 font-lock-variable-name-face)
      '("\\([a-zA-Z][_a-zA-Z.\-]*\\)[ \t]*=[ \t]*" 1 font-lock-variable-name-face)
      '("[ \t]*=[ \t]*\\([a-zA-Z][_a-zA-Z]*\\)" 1 font-lock-variable-name-face)


;; other reserved words
      (cons (concat "\\(" rwregex "\\)") 'font-lock-variable-name-face)

;; highlight note names; separate notes from (other than ')'-type) brackets
      '("[ <\{[~()\t]\\(\\(\\(\\(do\\|re\\|mi\\|fa\\|sol\\|la\\|si\\)\\(bb?\\|dd?\\|ss?\\)?\\)\\|\\([a-hsrR]\\(flat\\(flat\\)?\\|sharp\\(sharp\\)?\\|ff?\\|ss?\\|is\\(siss\\|s\\|is\\)?\\|es\\(sess\\|s\\|es\\)?\\)?\\)\\|\\(as\\(as\\|es\\)?\\)\\|\\(es\\(es\\)?\\)\\|\\(bb\\)\\)[,']*\\(128\\|64\\|32\\|16\\|8\\|4\\|2\\|1\\)?[.]*\\)" 1 font-lock-type-face)

;; highlight identifiers
      (cons (concat "\\([_^-]?\\(" iregex "\\)\\)+\\($\\|[] \t(~{}>\\\\]\\)") '(0 font-lock-function-name-face t))

;; highlight keywords
      (cons (concat "\\([_^-]?\\(" kwregex "\\)\\)+\\($\\|[] \t(~{}>\\\\]\\)") '(0 font-lock-keyword-face t))

;; highlight bracketing constructs
      '("\\([][}{]\\)" 0 font-lock-warning-face t)
      ;; these regexps allow angle-brackets to be highlighted when and only when they delimit simultaneous music
      ;; fontify open < but leave crescendos \< alone
      '("[^\\]\\(<\\)" 1 font-lock-warning-face t)
      ;; fontify the close-brackets in <a b c--> (tenuto) and <a b c-^> (marcato)
      '("[_^-]\\s-*[-^]\\s-*\\(>\\)" 1 font-lock-warning-face t) 
      ;; but leave a b c-> (accent) alone, accounting for whitespace
      '("\\([^\\t\\n _^-]\\|^\\)\\s-*\\(>\\)" 2 font-lock-warning-face t)
      ;; ties ~, slurs (), hairpins \<,  \>, end-of-hairpin \!, 
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
	    ( ?\[ . "(]" ) ( ?\] . ")[" ) ;; all the other paren characters are now handled by          
	    ( ?\{  .  ".2b" )             ;; lily-specific indenting/matching code in lilypond-indent.el 
	    ( ?\}  .  ".4b" )              
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

