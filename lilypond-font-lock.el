;;; lilypond-font-lock.el --- syntax coloring for LilyPond mode

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2001-2003: Heikki Junes
;;  * Emacs-mode: new keywords, reserved words, identifiers, notenames, 
;;    some dynamics and brackets are font-lock-keywords
;;  * File lilypond.words gives keywords, identifiers and reserved words
;; Author: 1997: Han-Wen Nienhuys
;; Author: 1995-1996 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Created:       Feb 1992
;; Version:       1.7.18
;; Last Modified: 6MAY2003
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
;;   - handle lexer modes (\header, \melodic) etc.

(defconst LilyPond-font-lock-keywords
  (let* ((kwregex (mapconcat (lambda (x) (concat "\\" x))  LilyPond-keywords "\\|"))
	 (iregex (mapconcat (lambda (x) (concat "\\" x))  LilyPond-identifiers "\\|"))
	 (rwregex (mapconcat (lambda (x) (concat "" x))  LilyPond-reserved-words "\\|"))
)

    (list 
;; Fonts in use (from GNU Emacs Lisp Reference Manual, elisp.ps):
;; font-lock- (c)omment / (s)tring / (k)eyword / (b)uiltin / (f)unction-name / 
;;            (v)ariable-name / (t)ype / co(n)stant / (w)arning -face

;; The order below is designed so that proofreading would be possible.

;; Fontify...
;; ... (f) identifiers and (k) keywords.
;; ... (n) user defined indetifiers
;; ... (v) the right and the left side of '='-marks.
;; ... (v) reserved words, e.g., FiguredBass.
;; ... (t) notes and rests
;; "on top", ... (s) lyrics-mode
;; "on top", ... (w) horizontal grouping
;; "on top", ... (f) vertical grouping
;; "on top", ... (b) expressional grouping
;; "on top", ... (s) (multiline-)scheme; urgh. one should count the slurs
;; "on top", ... (s) strings
;; "on top", ... (c) (multiline-)comments

;; One should note 'font-lock-multiline' has been possible since Emacs 21.1.
;; See, e.g., text in "http://emacs.kldp.org/emacs-21.1/etc/NEWS".

;; ... identifiers (defined above, see iregex)
      (cons (concat "\\(\\([_^-]?\\(" iregex "\\)\\)+\\)\\($\\|[] \t(~{}>\\\\_()^*-]\\)") '(1 font-lock-function-name-face))

;; ... keywords (defined above, see kwregex)
      (cons (concat "\\(\\([_^-]?\\(" kwregex "\\)\\)+\\)\\($\\|[] \t(~{}>\\\\_()^*-]\\)") '(1 font-lock-keyword-face))

;; ... user defined identifiers \[a-zA-Z]+, but not \breve or \longa (durations)
      '("\\([_^-]?\\\\\\([ac-km-zA-Z]\\|l[a-np-zA-Z]\\|b[a-qs-zA-Z]\\|lo[a-mo-zA-Z]\\|br[a-df-zA-Z]\\|lon[a-fh-zA-Z]\\|bre[a-uw-zA-Z]\\|long[b-zA-Z]\\|brev[a-df-zA-Z]\\|\\(longa\\|breve\\)[a-zA-Z]\\)[a-zA-Z]*\\)" 1 font-lock-constant-face)

;; ... the left side of '=' -mark
      '("\\([_a-zA-Z.0-9-]+\\)[ \t]*=[ \t]*" 1 font-lock-variable-name-face)

;; ... the right side of '=' -mark
      '("[ \t]*=[ \t]*\\([_a-zA-Z.0-9-]+\\)" 1 font-lock-variable-name-face)

;; ... reserved words (defined above, see rwregex)
      (cons (concat "\\(" rwregex "\\)") 'font-lock-variable-name-face)

;; ... note or rest with (an accidental and) a duration (multiplied), e.g., b,?16.*3/4
      '("\\(^\\|[ <\{[/~(!)\t\\\|]\\)\\(\\(\\(\\(bb\\|as[ae]s\\|eses\\|\\(do\\|re\\|[ms]i\\|[fl]a\\|sol\\)\\(bb?\\|dd?\\|ss?\\)?\\)\\|\\([a-h]\\(\\(flat\\)+\\|\\(sharp\\)+\\|is\\(siss\\|i?s\\)?\\|es\\(sess\\|e?s\\)?\\|ff?\\|ss?\\)?\\)\\)[,']*[?!]?\\|[srR]\\)\\([ \t]*\\(128\\|6?4\\|3?2\\|16?\\|8\\|\\\\\\(breve\\|longa\\)\\)[.]*\\([ \t]*[*][ \t]*[0-9]+\\(/[1-9][0-9]*\\)?\\)?\\)\\)" 2 font-lock-type-face)
;; ... note or rest (with an accidental), e.g., b,? -- allows cis\longaX
      '("\\(^\\|[ <\{[/~(!)\t\\\|]\\)\\(\\(\\(bb\\|as[ae]s\\|eses\\|\\(do\\|re\\|[ms]i\\|[fl]a\\|sol\\)\\(bb?\\|dd?\\|ss?\\)?\\)\\|\\([a-h]\\(\\(flat\\)+\\|\\(sharp\\)+\\|is\\(siss\\|i?s\\)?\\|es\\(sess\\|e?s\\)?\\|ff?\\|ss?\\)?\\)\\)[,']*[?!]?\\|[srR]\\)" 2 font-lock-type-face)

;; "on top", ... lyrics-mode: fontify everything between '<'...'>' or '{'...'}'
;            URGH, does not know anything about inner brackets.
;            Multiple lines may need refontifying (C-c f).
      '("\\(\\\\lyrics[^{<]*\\)\\({[^}]*\\|<[^>]*\\)" 2 font-lock-string-face t)

;; "on top", ... horizontal grouping:
;;               - brackets '{[]}'
;;               - ties '~'
;;               - ligatures \[, \]
      '("\\([][~}{]\\|\\\\[][]\\)" 0 font-lock-warning-face t)

;; "on top", ... vertical grouping:
;;               - '<>'-chord brackets with '\\'-voice sep., not marcato '->'
;;               - '<< a b >>8' -chords
      '("\\(\\(-.\\)+\\|[^-^_]\\)\\([<>]+\\(\\(128\\|6?4\\|3?2\\|16?\\|8\\|\\\\\\(breve\\|longa\\)\\)[.]*\\([ \t]*[*][ \t]*[0-9]+\\(/[1-9][0-9]*\\)?\\)?\\)?\\|\\\\\\\\\\)" 3 font-lock-function-name-face t)

;; "on top", ... expressional grouping:
;;               - slurs ( ), \( \), [-^_][()]
;;               - hairpins \<, \>, \! 
      '("\\(\\\\[(<!>)]\\|[-^_]?[()]\\)" 0 font-lock-builtin-face t)

;; "on top", ... (multiline-)scheme: try find slurs up to 7th
      '("[_^-]?#\\(#[ft]\\|-?[0-9.]+\\|\"[^\"]*\"\\|['`]?[a-zA-Z-:]+\\|['`]?([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^)]*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*[^)]*)\\)" 0 font-lock-string-face t)

;; "on top", ... strings
      '("\\([_^-]?\"\\([^\"\\\\]\\|\\\\.\\|\\\\\n\\)*\"\\)" 0 font-lock-string-face t)

;; "on top", ... (multiline-)comments
      '("\\(%\\({[^%]*%\\(}\\|\\([^}][^%]*%\\)+}\\)\\|.*\\)\\)" 0 font-lock-comment-face t)

      )
    )
  "Additional expressions to fontify in LilyPond mode.")

;; define a mode-specific abbrev table for those who use such things
(defvar LilyPond-mode-abbrev-table nil
  "Abbrev table in use in `LilyPond-mode' buffers.")

(define-abbrev-table 'LilyPond-mode-abbrev-table nil)

(defvar LilyPond-mode-syntax-table nil
  "Syntax table used in `LilyPond-mode' buffers.")

(if LilyPond-mode-syntax-table
    ()
  (setq LilyPond-mode-syntax-table (make-syntax-table))
  ;; NOTE: Emacs knows only "13"-style (used), XEmacs knows also "1b3b", etc.
  (mapcar (function
	   (lambda (x) (modify-syntax-entry
			(car x) (cdr x) LilyPond-mode-syntax-table)))
	  '(
	    ;; also '['- and ']'-slurs should be handled by lilypond-indent.el
	    ;; in order to match also '\['- and '\]'-slurs in ligatures
	    ( ?\[ . "(]" ) ( ?\] . ")[" ) ; handled by the syntax table
	    ;; all the other paren characters are now handled by   
	    ;; lily-specific indenting/matching code in lilypond-indent.el 
	    ( ?\( . "." ) ( ?\) . "." ) 
	    ( ?\< . "." )( ?\> . ".") 
	    ( ?\{  .  ". 2" )  ; also 2nd char in begin of block-comment
	    ( ?\}  .  ". 4" )  ; also 2nd char in end of block-comment
	    ( ?\%  .  "< 13" ) ; comment starter, 1st char in block-comments
	    ( ?\n . ">")       ; newline: comment ender
	    ( ?\r . ">")       ; formfeed: comment ender
	    ( ?\\ . "\\" )     ; escape characters (as '\n' in strings)
	    ( ?\" . "\"" )     ; string quote characters
	    ;; word constituents (e.g., belonging to a note)
	    ( ?\' . "w") ( ?\, . "w") ; transposing octaves
	    ;; punctuation characters (separate symbols from another)
	    ( ?\$ . "." ) ( ?\& . "." )
	    ( ?\* . "." ) ( ?\+ . "." ) ( ?\/ . "." )  ( ?\= . "." )
	    ( ?\| . "." )      ; bar line
	    ( ?\- . "." ) ( ?\_ . "." ) ( ?\^ . "." ) ; accent positioners
	    ))
  )

