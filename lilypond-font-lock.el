;;; lilypond-font-lock.el --- syntax coloring for LilyPond mode

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 2001-2003: Heikki Junes
;;  * Emacs-mode: new keywords, reserved words, identifiers, notenames, 
;;    some dynamics and brackets are font-lock-keywords
;;  * File lilypond.words contains \keywords, \Identifiers and 
;;    ReservedWords notenames.
;;  * context-dependent syntax-tables
;; Author: 1997: Han-Wen Nienhuys
;; Author: 1995-1996 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Created:       Feb 1992
;; Version:       1.9.7
;; Last Modified: 18SEP2003
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
	 (ncrwregex (mapconcat (lambda (x) (concat "" x))  LilyPond-non-capitalized-reserved-words "\\|"))
	 (rwregex (mapconcat (lambda (x) (concat "" x))  LilyPond-Capitalized-Reserved-Words "\\|"))
	 (duration "\\([ \t]*\\(\\\\breve\\|128\\|6?4\\|3?2\\|16?\\|8\\)[.]*\\([ \t]*[*][ \t]*[0-9]+\\(/[1-9][0-9]*\\)?\\)?\\)") 
	 (longduration "\\([ \t]*\\(\\\\\\(longa\\|breve\\|maxima\\)\\)[.]*\\([ \t]*[*][ \t]*[0-9]+\\(/[1-9][0-9]*\\)?\\)?\\)") 
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

;; ... user defined identifiers \[a-zA-Z]+
      '("\\([_^-]?\\\\\\([a-zA-Z][a-zA-Z]*\\)\\)" 1 font-lock-constant-face)

;; ... the left side of '=' -mark
      '("\\([_a-zA-Z.0-9-]+\\)[ \t]*=[ \t]*" 1 font-lock-variable-name-face)

;; ... the right side of '=' -mark
      '("[ \t]*=[ \t]*\\([_a-zA-Z.0-9-]+\\)" 1 font-lock-variable-name-face)

;; ... reserved words (defined above, see rwregex)
      (cons (concat "\\(" rwregex "\\)") 'font-lock-variable-name-face)

;; ... note or rest with (an accidental and) a duration, e.g., b,?16.*3/4
      (cons (concat "\\(^\\|[ <\{[/~(!)\t\\\|]\\)\\(\\(\\(" ncrwregex "\\)[,']*[?!]?\\|[srR]\\)" duration "?\\)") '(2 font-lock-type-face))

;; "on top", ... notes and rests with a long duration
      (cons (concat longduration) '(0 font-lock-type-face t))

;; "on top", ... lyrics-mode: fontify everything between '<'...'>' or '{'...'}'
;            URGH, does not know anything about inner brackets.
;            Multiple lines may need refontifying (C-c f).
      '("\\(\\\\lyrics[^{<]*\\)\\({[^}]*\\|<[^>]*\\)" 2 font-lock-string-face t)

;; "on top", ... horizontal grouping, also as postfix syntax '-*':
;;               - brackets '{[]}'
;;               - ties '~'
;;               - ligatures \[, \]
      '("\\(-?[][~}{]\\|\\\\[][]\\)" 0 font-lock-warning-face t)

;; "on top", ... vertical grouping:
;;               - '<>'-chord brackets with '\\'-voice sep., not marcato '->'
;;               - '<< a b >>8' -chords
      (cons (concat "\\(\\(-.\\)+\\|[^-^_]\\)\\([<>]+\\(" duration "\\|" longduration "\\)?\\|\\\\\\\\\\)") '(3 font-lock-function-name-face t))

;; "on top", ... expressional grouping, also as postfix syntax '-*':
;;               - slurs ( ), \( \), [-^_][()]
;;               - hairpins \<, \>, \! 
      '("\\(-?\\\\[(<!>)]\\|[-^_]?[()]\\)" 0 font-lock-builtin-face t)

;; "on top", ... (multiline-)scheme: try find slurs up to 7th
      '("[_^-]?#\\(#[ft]\\|-?[0-9.]+\\|\"[^\"]*\"\\|['`]?[a-zA-Z:-]+\\|['`]?([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^)]*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*[^)]*)\\)" 0 font-lock-string-face t)

;; "on top", ... strings, match also unending strings at eof:
;;               if '\n' was not found, it must be '$' which is eof (?).
      '("\\([_^-]?\"\\([^\"\\\\]\\|\\\\.\\|\\\\\n\\)*\\(\"\\|$\\)\\)" 0 font-lock-string-face t)

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

(defun LilyPond-mode-set-syntax-table (&optional not-punct)
  "Change syntax table according to the argument `not-punct' which contains characters which are given a context dependent non-punctuation syntax: parentheses may be set to parenthesis syntax and characters `-', `^' and `_' may be set to escape syntax."
  (if (not not-punct) (setq not-punct '()))
  (setq LilyPond-mode-syntax-table (make-syntax-table))
  (let ((defaults 	  
	  '(
	    ;; NOTE: Emacs knows only "13"-style (used), XEmacs knows also "1b3b", etc.
	    ( ?\% . "< 13" )   ; comment starter, 1st char in block-comments
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
	    )))
    ;; all the paren characters are now handled by lily-specific indenting/matching code in lilypond-indent.el
    (if (or (memq ?\{ not-punct) (memq ?\} not-punct))
	(setq defaults (cons '( ?\{ . "(} 2" ) (cons '( ?\} . "){ 4" ) defaults))) ; begin and end of a block-comment
      (setq defaults (cons '( ?\{ . ". 2" ) (cons '( ?\} . ". 4" ) defaults))))    ; begin and end of a block-comment
    (if (or (memq ?\[ not-punct) (memq ?\] not-punct))
	(setq defaults (cons '( ?\[ . "(]" ) (cons '( ?\] . ")[" ) defaults)))
      (setq defaults (cons '( ?\[ . "." ) (cons '( ?\] . "." ) defaults))))
    (if (or (memq ?\< not-punct) (memq ?\> not-punct))
	(setq defaults (cons '( ?\< . "(>" ) (cons '( ?\> . ")<" ) defaults)))
      (setq defaults (cons '( ?\< . "." ) (cons '( ?\> . "." ) defaults))))
    (if (or (memq ?\( not-punct) (memq ?\) not-punct))
	(setq defaults (cons '( ?\( . "()" ) (cons '( ?\) . ")(" ) defaults)))
      (setq defaults (cons '( ?\( . "." ) (cons '( ?\) . "." ) defaults))))
    ;; In LilyPond the following chars serve as escape chars, e.g., c^> d-) e_( , 
    ;; but they may be set to punctuation chars, since inside strings they should not act as escape chars
    (setq defaults (cons (if (memq ?\- not-punct) '( ?\- . "\\" ) '( ?\- . "." ) ) defaults))
    (setq defaults (cons (if (memq ?\^ not-punct) '( ?\^ . "\\" ) '( ?\^ . "." ) ) defaults))
    (setq defaults (cons (if (memq ?\_ not-punct) '( ?\_ . "\\" ) '( ?\_ . "." ) ) defaults))
    (mapcar (function
	     (lambda (x) (modify-syntax-entry
			  (car x) (cdr x) LilyPond-mode-syntax-table)))
	    defaults)
    (set-syntax-table LilyPond-mode-syntax-table)))

(defun LilyPond-mode-context-set-syntax-table ()
  "Change syntax table according to current context."
  (interactive)
  ;; default syntax table sets parentheses to punctuation characters
  (LilyPond-mode-set-syntax-table) 
  ;; find current context
  (setq context (parse-partial-sexp (point-min) (point)))
  (cond ((nth 3 context)) ; inside string
	((nth 4 context)) ; inside a comment
	((eq (char-syntax (char-before (point))) ?\\)) ; found escape-char
	((and (eq (char-syntax (char-before (- (point) 1))) ?\\)
	      (memq (char-before (point)) '( ?\) ?\] )))) ; found escape-char
	((memq (char-before (point)) '( ?\) ))
	 (LilyPond-mode-set-syntax-table '( ?\( ?\) )))
	((memq (char-before (point)) '( ?\] ))
	 (LilyPond-mode-set-syntax-table '( ?\[ ?\] )))
	((memq (char-before (point)) '( ?\> ?\} ))
	 (LilyPond-mode-set-syntax-table '( ?\< ?\> ?\{ ?\} ?\^ ?\- ?\_ )))
	((memq (char-after (point)) '( ?\( ))
	 (LilyPond-mode-set-syntax-table '( ?\( ?\) )))
	((memq (char-after (point)) '( ?\[ ))
	 (LilyPond-mode-set-syntax-table '( ?\[ ?\] )))
	((memq (char-after (point)) '( ?\< ?\{ ))
	 (LilyPond-mode-set-syntax-table '( ?\< ?\> ?\{ ?\} ?\^ ?\- ?\_ )))
	))
