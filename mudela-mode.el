;;; mudela-mode.el --- Major mode for editing Mudela programs


;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 1997: Han-Wen Nienhuys
;; Author: 1995-1996 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Created:       Feb 1992
;; Version:       0.0
;; Last Modified: 12SEP97
;; Keywords: mudela languages music

;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.



;; Kyrie Eleison; it is my first real Elisp file
;; This is a cannabalised version of python-mode.el (HWN)
;;
;; TODO: 
;; * should handle block comments too.
;; * handle lexer modes (\header, \melodic, \lyric) etc.
;; * indentation
;; * notenames?
;; * fontlock: \melodic \melodic
;; 

(defconst mudela-font-lock-keywords
  (let* ((keywords '(
		     "accepts" "accidentals" "break" "bar" "cadenza" 
		     "clef" "cm" "consists" "contains" "duration" "absdynamic" 
		     "in" "translator" "type" "lyric" "key" "maininput" "notes"
		     "musical_pitch" "time" "midi" "mm" "header"
		     "notenames" "octave" "output" "partial" "paper" "plet"
		     "property" "pt" "shape" "relative" "include" "score"
		     "script" "skip"  "table" "spandynamic" "symboltables"
		     "tempo" "texid" "textstyle" "transpose" "version" "grouping"
		     ))
       (kwregex (mapconcat (lambda (x) (concat "\\\\" x))  keywords "\\|")))

    (list 
     (cons (concat ".\\(" kwregex "\\)[^a-zA-Z]") 1)
     (cons (concat "^\\(" kwregex "\\)[^a-zA-Z]") 1)
     '(".\\(\\\\[a-zA-Z][a-zA-Z]*\\)" 1 font-lock-variable-name-face)
     '("^[\t ]*\\([a-zA-Z][_a-zA-Z]*\\) *=" 1 font-lock-variable-name-face)     
    ))
  "Additional expressions to highlight in Mudela mode.")

;; define a mode-specific abbrev table for those who use such things
(defvar mudela-mode-abbrev-table nil
  "Abbrev table in use in `mudela-mode' buffers.")

(define-abbrev-table 'mudela-mode-abbrev-table nil)

(defvar mudela-mode-hook nil
  "*Hook called by `mudela-mode'.")

(defvar mu-mode-map ()
  "Keymap used in `mudela-mode' buffers.")

(defun mu-newline-and-indent ()
  (interactive)
  (newline)
  (indent-relative-maybe)
  "Newline and copy previous indentation")

(if mu-mode-map
    ()
  (setq mu-mode-map (make-sparse-keymap))

  (mapcar (function (lambda (key)
		      (define-key
			mu-mode-map key 'mu-newline-and-indent)))
   (where-is-internal 'newline-and-indent))

  (mapcar (function
	   (lambda (x)
	     (define-key mu-mode-map (car x) (cdr x))))
	  '(("\C-c\C-c"  . mu-foo-bar)
	    ))
  ;; should do all keybindings this way
  (define-key mu-mode-map [RET] 'mu-newline-and-indent)
  ) 

(defvar mu-mode-syntax-table nil
  "Syntax table used in `mudela-mode' buffers.")

;;
(if mu-mode-syntax-table
    ()
  (setq mu-mode-syntax-table (make-syntax-table))
  (mapcar (function
	   (lambda (x) (modify-syntax-entry
			(car x) (cdr x) mu-mode-syntax-table)))
	  '(( ?\( . "()" ) ( ?\) . ")(" )
	    ( ?\[ . "(]" ) ( ?\] . ")[" )
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

(defconst mu-stringlit-re
   "\"\\([^\"\n\\]\\|\\\\.\\)*\""	; double-quoted
  "Regexp matching a Mudela string literal.")


(defconst mu-blank-or-comment-re "[ \t]*\\($\\|%\\)"
  "Regexp matching blank or comment lines.")

(defconst mu-imenu-generic-re "^\\([a-zA-Z_][a-zA-Z0-9_]*\\) *="
  "Regexp matching Identifier definitions.")

;; Sadly we need this for a macro in Emacs 19.
(eval-when-compile
  ;; Imenu isn't used in XEmacs, so just ignore load errors.
  (condition-case ()
      (require 'imenu)
    (error nil)))

(defvar mu-imenu-generic-expression
  (list (list nil mu-imenu-generic-re 1))
  "Expression for imenu")

(defun mudela-mode ()
  "Major mode for editing Mudela files."

  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (setq comment-start "% ")
  (setq comment-end "")
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (setf comment-start-skip "%{")
  (make-local-variable 'comment-column)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression mu-imenu-generic-expression)
  (make-local-variable 'indent-line-function)
 
  ;;
  (set-syntax-table mu-mode-syntax-table)
  (setq major-mode             'mudela-mode
	mode-name              "Mudela"
	local-abbrev-table     mudela-mode-abbrev-table
	font-lock-defaults     '(mudela-font-lock-keywords)
	paragraph-separate     "^[ \t]*$"
	paragraph-start        "^[ \t]*$"
	require-final-newline  t
	comment-start          "% "
	comment-start-skip     "% *"
	comment-column         40
	indent-line-function	'indent-relative-maybe
	)
  (use-local-map mu-mode-map)

  ;; run the mode hook. mu-mode-hook use is deprecated
  (if mudela-mode-hook
      (run-hooks 'mudela-mode-hook)
    (run-hooks 'mu-mode-hook)))

(defun mu-keep-region-active ()
  ;; do whatever is necessary to keep the region active in XEmacs.
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently and doesn't its policy doesn't
  ;; require us to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))


(defun mu-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start mu-block-comment-prefix))
    (comment-region beg end arg)))

(defconst mu-version "0.0.1"
  "`mudela-mode' version number.")
(defconst mu-help-address "hanwen@cs.uu.nl"
  "Address accepting submission of bug reports.")

(defun mu-version ()
  "Echo the current version of `mudela-mode' in the minibuffer."
  (interactive)
  (message "Using `mudela-mode' version %s" mu-version)
  (mu-keep-region-active))

(provide 'mu-mode)
;;; mudela-mode.el ends here
