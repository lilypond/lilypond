;;; lilypond-mode.el --- Major mode for editing GNU LilyPond music scores

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 1999: Jan Nieuwenhuizen
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

;; This started out as a cannabalised version of python-mode.el, by hwn
;; For changes see the LilyPond ChangeLog
;;
;; TODO: 
;; * lily/ly/lilypond?
;; * syntax
;;   - should handle block comments too.
;;   - handle lexer modes (\header, \melodic, \lyric) etc.
;;   - indentation
;;   - notenames?
;;   - fontlock: \melodic \melodic


(defconst lily-version "1.3.19"
  "`lilypond-mode' version number.")

(defconst lily-help-address "hanwen@cs.uu.nl"
  "Address accepting submission of bug reports.")

(defconst lily-font-lock-keywords
  (let* ((keywords '("spanrequest" "simultaneous" "sequential" "accepts"
		     "autochange" "alternative" "bar" "breathe"
		     "cadenza" "chordmodifiers" "chords" "clef" "cm" "consists"
		     "consistsend" "context"
		     "duration" "font" "grace" "header" "in" "lyrics"
		     "key" "keysignature" "mark" "musicalpitch"
		     "time" "times" "midi" "mm" "name" "notenames"
		     "notes" "partial" "paper" "penalty" "push" "pop" "property" "pt"
		     "relative" "remove" "repeat" "repetitions" "addlyrics"
		     "scm" "scmfile" "score" "script"
		     "shape" "skip" "textscript" "tempo" "translator" "transpose"
		     "type" "version" 
		     ))
       (kwregex (mapconcat (lambda (x) (concat "\\\\" x))  keywords "\\|")))

    (list 
      (concat ".\\(" kwregex "\\)[^a-zA-Z]")
      (concat "^\\(" kwregex "\\)[^a-zA-Z]")
      '(".\\(\\\\[a-zA-Z][a-zA-Z]*\\)" 1 font-lock-variable-name-face)
      '("^[\t ]*\\([a-zA-Z][_a-zA-Z]*\\) *=" 1 font-lock-variable-name-face)     
    ))
  "Additional expressions to highlight in Mudela mode.")

;; define a mode-specific abbrev table for those who use such things
(defvar lilypond-mode-abbrev-table nil
  "Abbrev table in use in `lilypond-mode' buffers.")

(define-abbrev-table 'lilypond-mode-abbrev-table nil)

(defvar lilypond-mode-hook nil
  "*Hook called by `lilypond-mode'.")

(defvar lily-mode-syntax-table nil
  "Syntax table used in `lilypond-mode' buffers.")

;;
(if lily-mode-syntax-table
    ()
  (setq lily-mode-syntax-table (make-syntax-table))
  (mapcar (function
	   (lambda (x) (modify-syntax-entry
			(car x) (cdr x) lily-mode-syntax-table)))
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

(defconst lily-imenu-generic-re "^\\([a-zA-Z_][a-zA-Z0-9_]*\\) *="
  "Regexp matching Identifier definitions.")

;; Sadly we need this for a macro in Emacs 19.
(eval-when-compile
  ;; Imenu isn't used in XEmacs, so just ignore load errors.
  (condition-case ()
      (require 'imenu)
    (error nil)))

(defvar lily-imenu-generic-expression
  (list (list nil lily-imenu-generic-re 1))
  "Expression for imenu")


;;; we're using some handy compile commands
(require 'compile)

(defcustom lily-command "lilypond"
  "* LilyPond executable."
  :type 'string
  :group 'lily)

(defcustom lily-parameters ""
  "*."
  :type 'string
  :group 'lily)

(defvar lily-regexp-alist
  '(("\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match LilyPond errors.  See `compilation-error-regexp-alist'.")

(defcustom lily-tex-command "tex"
  "*."
  :type 'string
  :group 'lily)

(defcustom lily-xdvi-command "xdvi"
  "*."
  :type 'string
  :group 'lily)

(defun lily-compile-file (command parameters file)
  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (let ((command-args (concat command " " parameters " " file)))
	(compile-internal command-args "No more errors" "LilyPond")))

;; do we still need this, now that we're using compile-internal?
(defun lily-save-buffer ()
  (if (buffer-modified-p) (save-buffer)))

;;; return (dir base ext)
(defun split-file-name (name)
  (let* ((i (string-match "[^/]*$" name))
	 (dir (if (> i 0) (substring name 0 i) "./"))
	 (file (substring name i (length name)))
	 (i (string-match "[^.]*$" file)))
    (if (and
	 (> i 0)
	 (< i (length file)))
	(list dir (substring file 0 (- i 1)) (substring file i (length file)))
      (list dir file ""))))

;;;###autoload
(defun lily-eval-buffer ()
  "Run LilyPond on buffer."
  (interactive)
  (let ((buffer (buffer-name)))
    (if (buffer-file-name)
	(progn
	  (lily-save-buffer)
	  (lily-compile-file lily-command lily-parameters (buffer-file-name)))
      (progn
	(error "Buffer %s is not associated with a file" buffer)
	(lily-eval-region (min-point) (max-point))))))

;;;###autoload
(defun lily-eval-region (start end)
  "Run LilyPond on region."
  (interactive "r")
  (let ((basename "emacs-lily")
	(suffix (if (string-match "^[\\]score" (buffer-substring start end))
		    ".ly"
		  (if (< 50 (abs (- start end)))
		      ".fly"
		      ".sly"))))
    (write-region start end (concat basename suffix) nil 'nomsg)
    (lily-compile-file lily-command lily-parameters (concat basename suffix))))

(defun lily-running ()
  (let ((process (get-process "lilypond")))
  (and process
       (eq (process-status process) 'run))))

(defun lily-tex-file (basename)
  (call-process lily-tex-command nil t nil basename))

(defun lily-xdvi-file (basename)
  (let ((outbuf (get-buffer-create "*lily-xdvi*"))
	(name "xdvi")
	(command (concat lily-xdvi-command " " basename)))
    (if (get-process "xdvi")
	;; Don't open new xdvi window, but force redisplay
	;; We could make this an option.
	(signal-process (process-id (get-process "xdvi")) 'SIGUSR1)
      (if (fboundp 'start-process)
	  (let* ((process-environment (cons "EMACS=t" process-environment))
		 (proc (start-process-shell-command name outbuf command)))
	    ;;(set-process-sentinel proc 'compilation-sentinel)
	    ;;(set-process-filter proc 'compilation-filter)
	    (set-marker (process-mark proc) (point) outbuf))
	;;(setq compilation-in-progress (cons proc compilation-in-progress)))
	
	;; No asynchronous processes available.
	(message "Executing `%s'..." command)
	;; Fake modeline display as if `start-process' were run.
	(setq mode-line-process ":run")
	(force-mode-line-update)
	(sit-for 0)			; Force redisplay
      (call-process shell-file-name nil outbuf nil "-c" command)
      (message "Executing `%s'...done" command)))))
      

;;;###autoload
(defun lily-xdvi-buffer ()
  "Run LilyPond, TeX and Xdvi on buffer."
  (interactive)

  (let* ((split (split-file-name buffer-file-name))
	 (dir (car split))
	 (base (cadr split)))

    ;; we don't really need this...
    (let ((tex (concat dir base ".tex"))
	  (dvi (concat dir base ".dvi")))
      (if (file-exists-p tex) (delete-file tex))
      (if (file-exists-p dvi) (delete-file dvi)))

    (lily-eval-buffer)
    (set-buffer "*lilypond*")
    
    ;;(setq default-directory dir)
    (while (lily-running)
      (continue-process (get-process "lilypond")))
    (sit-for 0)			; Force redisplay
    
    (if (= 0 (process-exit-status (get-process "lilypond")))
	(progn
	  (if (= 0 (lily-tex-file base))
	      (lily-xdvi-file base))))))
  
;;;###autoload
(defun lily-xdvi-region (start end)
  "Run LilyPond, TeX and Xdvi on region."
  (interactive "r")

  (let ((dir default-directory)
	(base "emacs-lily"))

    ;; we don't really need this...
    (let ((tex (concat dir base ".tex"))
	  (dvi (concat dir base ".dvi")))
      (if (file-exists-p tex) (delete-file tex))
      (if (file-exists-p dvi) (delete-file dvi)))
    
    (lily-eval-region start end)
    (set-buffer "*lilypond*")
    
    ;;(setq default-directory dir)
    (while (lily-running)
      (continue-process (get-process "lilypond")))
    (sit-for 0)			; Force redisplay
    
    (if (= 0 (process-exit-status (get-process "lilypond")))
	(progn
	  (if (= 0 (lily-tex-file base))
	      (lily-xdvi-file base))))))

;;;###autoload
(defun lily-kill-job ()
  "Kill the currently running LilyPond job."
  (interactive)
  (quit-process (get-process "lilypond") t))

;; hmm
;;  (kill-process (get-process "xdvi") t)

(defvar lily-mode-map ()
  "Keymap used in `lilypond-mode' buffers.")

;; Note:  if you make changes to the map, you must do
;;    M-x set-variable lily-mode-map nil
;;    M-x eval-buffer
;;    M-x lilypond-mode
;; to let the changest take effect
(if lily-mode-map
    ()
  (setq lily-mode-map (make-sparse-keymap))
  (define-key lily-mode-map [f9] 'lily-eval-buffer)
  (define-key lily-mode-map [f10] 'lily-xdvi-buffer)
  (define-key lily-mode-map [S-f9] 'lily-eval-region)
  (define-key lily-mode-map [S-f10] 'lily-xdvi-region)
  ) 

(defun lilypond-mode ()
  "Major mode for editing Mudela files."
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(lily-font-lock-keywords))

  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^[ \t]*$")

  (make-local-variable 'paragraph-start)
  (setq	paragraph-start "^[ \t]*$")

  (make-local-variable 'comment-start)
  (setq comment-start "%")

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%{? *")

  (make-local-variable 'comment-end)
  (setq comment-end "\n")

  (make-local-variable 'block-comment-start)
  (setq block-comment-start "%{")

  (make-local-variable 'block-comment-end)  
  (setq block-comment-end   "%}")

  ;; (make-local-variable 'comment-column)
  ;; (setq comment-column 40)

  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression lily-imenu-generic-expression)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
 
  ;;
  (set-syntax-table lily-mode-syntax-table)
  (setq major-mode 'lilypond-mode)
  (setq mode-name "Mudela")
  (setq local-abbrev-table lilypond-mode-abbrev-table)
  (use-local-map lily-mode-map)

  ;; run the mode hook. lily-mode-hook use is deprecated
  (run-hooks 'lilypond-mode-hook))


(defun lily-keep-region-active ()
  ;; do whatever is necessary to keep the region active in XEmacs.
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently and doesn't its policy doesn't
  ;; require us to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))


;;(defun lily-comment-region (beg end &optional arg)
;;  "Like `comment-region' but uses double hash (`#') comment starter."
;;  (interactive "r\nP")
;;  (let ((comment-start lily-block-comment-prefix))
;;    (comment-region beg end arg)))

(defun lily-version ()
  "Echo the current version of `lilypond-mode' in the minibuffer."
  (interactive)
  (message "Using `lilypond-mode' version %s" lily-version)
  (lily-keep-region-active))

(provide 'lilypond-mode)
;;; lilypond-mode.el ends here
