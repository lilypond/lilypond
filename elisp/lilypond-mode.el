;;;; lilypond-mode.el -- Major mode for editing GNU LilyPond music scores
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1999--2022 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Changed 2001--2003 Heikki Junes <heikki.junes@hut.fi>
;;;;    * Add PS-compilation, PS-viewing and MIDI-play (29th Aug 2001)
;;;;    * Keyboard shortcuts (12th Sep 2001)
;;;;    * Inserting tags, inspired on sgml-mode (11th Oct 2001)
;;;;    * Autocompletion & Info (23rd Nov 2002)
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


;;; Inspired on auctex

;;; Look lilypond-init.el or Documentation/topdocs/INSTALL.texi
;;; for installing instructions.

(require 'easymenu)
(require 'compile)

(defconst LilyPond-version "2.5.20"
  "`LilyPond-mode' version number.")

(defconst LilyPond-help-address "bug-lilypond@gnu.org"
  "Address accepting submission of bug reports.")

(defvar LilyPond-mode-hook nil
  "*Hook called by `LilyPond-mode'.")

(defvar LilyPond-region-file-prefix "emacs-lily"
  "File prefix for commands on buffer or region.")

(defvar LilyPond-master-file nil
  "Master file that LilyPond will be run on.")

;; FIXME: find ``\score'' in buffers / make settable?
(defun LilyPond-get-master-file ()
  (or LilyPond-master-file
      (buffer-file-name)))

(defvar LilyPond-kick-xdvi nil
  "If true, no simultaneous xdvi's are started, but reload signal is sent.")

(defvar LilyPond-command-history nil
  "Command history list.")

(defvar LilyPond-regexp-alist
  '(("\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match LilyPond errors.  See `compilation-error-regexp-alist'.")

(defvar LilyPond-imenu nil
  "A flag to tell whether LilyPond-imenu is turned on.")
(make-variable-buffer-local 'LilyPond-imenu)

(defcustom LilyPond-include-path ".:/tmp"
  "* LilyPond include path."
  :type 'string
  :group 'LilyPond)

(defun LilyPond-words-filename ()
  "The file containing LilyPond \keywords \Identifiers and ReservedWords.
Finds file lilypond-words.el from load-path."
  (let ((fn nil)
	(lp load-path)
	(words-file "lilypond-words.el"))
    (while (and (> (length lp) 0) (not fn))
      (setq fn (concat (car lp) "/" words-file))
      (if (not (file-readable-p fn))
	  (progn (setq fn nil) (setq lp (cdr lp)))))
    (if (not fn)
	(progn (message "Warning: `lilypond-words.el' not found in `load-path'. See `lilypond-init.el'.")
	       (sit-for 5 0)))
    fn))

(defun LilyPond-add-dictionary-word (x)
  "Contains all words: \keywords \Identifiers and ReservedWords."
  (nconc '(("" . 1)) x))

(if (> emacs-major-version 20)
    (defun get-buffer-size (b) (buffer-size b))
  (defun get-buffer-size (b)
    (let (size (current-buffer (current-buffer)))
      (set-buffer b)
      (setq size (buffer-size))
      (set-buffer current-buffer)
      size
    )))

;; creates dictionary if empty
(if (and (eq (length (LilyPond-add-dictionary-word ())) 1)
	 (not (eq (LilyPond-words-filename) nil)))
    (progn
      (setq b (find-file-noselect (LilyPond-words-filename) t t))
      (setq m (set-marker (make-marker) 1 (get-buffer b)))
      (setq i 1)
      (while (> (get-buffer-size b) (marker-position m))
	(setq i (+ i 1))
	(setq copy (copy-alist (list (eval (symbol-name (read m))))))
	(setcdr copy i)
	(LilyPond-add-dictionary-word (list copy)))
      (kill-buffer b)))

(defvar LilyPond-insert-tag-current ""
  "The last command selected from the LilyPond-Insert -menu.")

(defconst LilyPond-menu-keywords
  (let ((wordlist '())
	(co (all-completions "" (LilyPond-add-dictionary-word ())))
	(currword ""))
    (progn
      (while (> (length co) 0)
	(setq currword (car co))
	(if (string-equal "-" (car (setq co (cdr co))))
	    (progn
	      (add-to-list 'wordlist currword)
	      (while (and (> (length co) 0)
			  (not (string-equal "-" (car (setq co (cdr co))))))))))
      (reverse wordlist)))
  "Keywords inserted from LilyPond-Insert-menu.")

(defconst LilyPond-keywords
  (let ((wordlist '("\\score"))
	(co (all-completions "" (LilyPond-add-dictionary-word ())))
	(currword ""))
    (progn
      (while (> (length co) 0)
	(setq currword (car co))
	(if (> (length currword) 1)
	    (if (and (string-equal "\\" (substring currword 0 1))
	             (string-match "[a-z-]+" currword)
	    	     (= (match-beginning 0) 1)
	    	     (= (match-end 0) (length currword))
		     (not (string-equal "\\longa" currword))
		     (not (string-equal "\\breve" currword))
		     (not (string-equal "\\maxima" currword))
		     (string-equal (downcase currword) currword))
		(add-to-list 'wordlist currword)))
	(if (string-equal "-" (car (setq co (cdr co))))
	    (while (and (> (length co) 0)
			(not (string-equal "-" (car (setq co (cdr co)))))))))
      (reverse wordlist)))
  "LilyPond \\keywords")

(defconst LilyPond-identifiers
  (let ((wordlist '("\\voiceOne"))
	(co (all-completions "" (LilyPond-add-dictionary-word ()))))
    (progn
      (while (> (length co) 0)
	(setq currword (car co))
	(if (> (length currword) 1)
	    (if (and (string-equal "\\" (substring currword 0 1))
	             (string-match "[a-zA-Z-]+" currword)
	    	     (= (match-beginning 0) 1)
	    	     (= (match-end 0) (length currword))
		     (not (string-equal (downcase currword) currword)))
		(add-to-list 'wordlist currword)))
	(if (string-equal "-" (car (setq co (cdr co))))
	    (while (and (> (length co) 0)
			(not (string-equal "-" (car (setq co (cdr co)))))))))
      (reverse wordlist)))
  "LilyPond \\Identifiers")

(defconst LilyPond-Capitalized-Reserved-Words
  (let ((wordlist '("StaffContext"))
	(co (all-completions "" (LilyPond-add-dictionary-word ()))))
    (progn
      (while (> (length co) 0)
	(setq currword (car co))
	(if (> (length currword) 0)
	    (if (and (string-match "[a-zA-Z_]+" currword)
	    	     (= (match-beginning 0) 0)
	    	     (= (match-end 0) (length currword))
		     (not (string-equal (downcase currword) currword)))
		(add-to-list 'wordlist currword)))
	(if (string-equal "-" (car (setq co (cdr co))))
	    (while (and (> (length co) 0)
			(not (string-equal "-" (car (setq co (cdr co)))))))))
      (reverse wordlist)))
  "LilyPond ReservedWords")

(defconst LilyPond-non-capitalized-reserved-words
  (let ((wordlist '("cessess"))
	(co (all-completions "" (LilyPond-add-dictionary-word ()))))
    (progn
      (while (> (length co) 0)
	(setq currword (car co))
	(if (> (length currword) 0)
	    (if (and (string-match "[a-z]+" currword)
	    	     (= (match-beginning 0) 0)
	    	     (= (match-end 0) (length currword))
		     (string-equal (downcase currword) currword))
		(add-to-list 'wordlist currword)))
	(if (string-equal "-" (car (setq co (cdr co))))
	    (while (and (> (length co) 0)
			(not (string-equal "-" (car (setq co (cdr co)))))))))
      (reverse wordlist)))
  "LilyPond notenames")

(defun LilyPond-check-files (derived originals extensions)
  "Check that DERIVED is newer than any of the ORIGINALS.
Try each original with each member of EXTENSIONS, in all directories
in LilyPond-include-path."
  (let ((found nil)
	(regexp (concat "\\`\\("
			(mapconcat (function (lambda (dir)
				      (regexp-quote (expand-file-name dir))))
				   LilyPond-include-path "\\|")
			"\\).*\\("
			(mapconcat 'regexp-quote originals "\\|")
			"\\)\\.\\("
			(mapconcat 'regexp-quote extensions "\\|")
			"\\)\\'"))
	(buffers (buffer-list)))
    (while buffers
      (let* ((buffer (car buffers))
	     (name (buffer-file-name buffer)))
	(setq buffers (cdr buffers))
	(if (and name (string-match regexp name))
	    (progn
	      (and (buffer-modified-p buffer)
		   (or (not LilyPond-save-query)
		       (y-or-n-p (concat "Save file "
					 (buffer-file-name buffer)
					 "? ")))
		   (save-excursion (set-buffer buffer) (save-buffer)))
	      (if (file-newer-than-file-p name derived)
		  (setq found t))))))
    found))

(defun LilyPond-running ()
  "Check the currently running LilyPond compiling jobs."
  (let ((process-names (list "lilypond" "tex" "2ps" "2midi"
			     "book" "latex"))
	(running nil))
    (while (setq process-name (pop process-names))
      (setq process (get-process process-name))
      (if (and process
	       (eq (process-status process) 'run))
	  (push process-name running)))
    running)) ; return the running jobs

(defun LilyPond-midi-running ()
  "Check the currently running Midi processes."
  (let ((process-names (list "midi" "midiall"))
	(running nil))
    (while (setq process-name (pop process-names))
      (setq process (get-process process-name))
      (if (and process
	       (eq (process-status process) 'run))
	  (push process-name running)))
    running)) ; return the running jobs

(defun LilyPond-kill-jobs ()
  "Kill the currently running LilyPond compiling jobs."
  (interactive)
  (let ((process-names (LilyPond-running))
	(killed nil))
    (while (setq process-name (pop process-names))
      (quit-process (get-process process-name) t)
      (push process-name killed))
    killed)) ; return the killed jobs

(defun LilyPond-kill-midi ()
  "Kill the currently running midi processes."
  (let ((process-names (LilyPond-midi-running))
	(killed nil))
    (while (setq process-name (pop process-names))
      (quit-process (get-process process-name) t)
      (push process-name killed))
    killed)) ; return the killed jobs

;; URG, should only run LilyPond-compile for LilyPond
;; not for tex,xdvi (lilypond?)
(defun LilyPond-compile-file (command name)
  ;; We maybe should know what we run here (Lily, lilypond, tex)
  ;; and adjust our error-matching regex ?
  (compilation-start
   (if (eq LilyPond-command-current 'LilyPond-command-master)
       command
     ;; use temporary directory for Commands on Buffer/Region
     ;; hm.. the directory is set twice, first to default-dir
     (concat "cd " (LilyPond-temp-directory) "; " command))))

;; do we still need this, now that we're using compilation-start?
(defun LilyPond-save-buffer ()
  "Save buffer and set default command for compiling."
  (interactive)
  (if (buffer-modified-p)
      (progn (save-buffer)
	     (setq LilyPond-command-next LilyPond-command-default))))

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


;; Should check whether in command-alist?
(defcustom LilyPond-command-default "LilyPond"
  "Default command. Must identify a member of LilyPond-command-alist."

  :group 'LilyPond
  :type 'string)
;;;(make-variable-buffer-local 'LilyPond-command-last)

(defvar LilyPond-command-next LilyPond-command-default)

(defvar LilyPond-command-current 'LilyPond-command-master)
;;;(make-variable-buffer-local 'LilyPond-command-master)


;; If non-nil, LilyPond-command-query will return the value of this
;; variable instead of querying the user.
(defvar LilyPond-command-force nil)

(defcustom LilyPond-lilypond-command "lilypond"
  "Command used to compile LY files."
  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-ps-command "gv --watch"
  "Command used to display PS files."

  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-pdf-command "xpdf"
  "Command used to display PDF files."

  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-midi-command "timidity"
  "Command used to play MIDI files."

  :group 'LilyPond
  :type 'string)

(defcustom LilyPond-all-midi-command "timidity -ia"
  "Command used to play MIDI files."

  :group 'LilyPond
  :type 'string)

(defun LilyPond-command-current-midi ()
  "Play midi corresponding to the current document."
  (interactive)
  (LilyPond-command (LilyPond-command-menu "Midi") 'LilyPond-get-master-file))

(defun LilyPond-command-all-midi ()
  "Play midi corresponding to the current document."
  (interactive)
  (LilyPond-command (LilyPond-command-menu "MidiAll") 'LilyPond-get-master-file))

(defun count-matches-as-number (re)
  "Count-matches in emacs 22 backwards-incompatibly returns a number"
  (let ((result (count-matches re)))
    (if (stringp result)
	(string-to-number result)
      result)))

(defun count-rexp (start end rexp)
  "Print number of found regular expressions in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (count-matches-as-number rexp))))

(defun count-midi-words ()
  "Check number of midi-scores before the curser."
  (if (eq LilyPond-command-current 'LilyPond-command-region)
      (count-rexp (mark t) (point) "\\\\midi")
    (count-rexp (point-min) (point-max) "\\\\midi")))

(defun count-midi-words-backwards ()
  "Check number of midi-scores before the curser."
  (if (eq LilyPond-command-current 'LilyPond-command-region)
      (count-rexp (mark t) (point) "\\\\midi")
    (count-rexp (point-min) (point) "\\\\midi")))

(defun LilyPond-string-current-midi ()
  "Check the midi file of the following midi-score in the current document."
  (let ((fnameprefix (if (eq LilyPond-command-current 'LilyPond-command-master)
			 (substring (LilyPond-get-master-file) 0 -3); suppose ".ly"
		       LilyPond-region-file-prefix))
	(allcount (count-midi-words))
	(count (count-midi-words-backwards)))
    (concat  fnameprefix
	     (if (and (> allcount 1) (> count 0)) ; not first score
		 (if (eq count allcount)          ; last score
		     (concat "-" (number-to-string (+ count -1)))
		   (concat "-" (number-to-string count))))
	     ".midi")))

(defun LilyPond-string-all-midi ()
  "Return the midi files of the current document in ascending order."
  (let ((fnameprefix (if (eq LilyPond-command-current 'LilyPond-command-master)
			 (substring (LilyPond-get-master-file) 0 -3); suppose ".ly"
		       LilyPond-region-file-prefix))
	(allcount (count-midi-words)))
    (concat (if (> allcount 0)  ; at least one midi-score
		(concat fnameprefix ".midi "))
	    (if (> allcount 1)  ; more than one midi-score
		(concat fnameprefix "-[1-9].midi "))
	    (if (> allcount 9)  ; etc.
		(concat fnameprefix "-[1-9][0-9].midi"))
	    (if (> allcount 99) ; not first score
		(concat fnameprefix "-[1-9][0-9][0-9].midi")))))

;; This is the major configuration variable.
(defcustom LilyPond-command-alist
  ;; Should expand this to include possible keyboard shortcuts which
  ;; could then be mapped to define-key and menu.
  '(
    ("LilyPond" . ((LilyPond-lilypond-command " %s") "%s" "%l" "View"))
    ("2PS" . ((LilyPond-lilypond-command " -f ps %s") "%s" "%p" "ViewPS"))
    ("Book" . ("lilypond-book %x" "%x" "%l" "LaTeX"))
    ("LaTeX" . ("latex '\\nonstopmode\\input %l'" "%l" "%d" "ViewDVI"))

    ;; refreshes when kicked USR1
    ("View" . ((LilyPond-pdf-command " %f")))
    ("ViewPDF" . ((LilyPond-pdf-command " %f")))
    ("ViewPS" . ((LilyPond-ps-command " %p")))

    ;; The following are refreshed in LilyPond-command:
    ;; - current-midi depends on cursor position and
    ("Midi" . ("")) ;
    ;; - all-midi depends on number of midi-score.
    ("MidiAll" . (""))
    )

  "AList of commands to execute on the current document.

The key is the name of the command as it will be presented to the
user, the value is a cons of the command string handed to the shell
after being expanded, and the next command to be executed upon
success.  The expansion is done using the information found in
LilyPond-expand-list.
"
  :group 'LilyPond
  :type '(repeat (cons :tag "Command Item"
		       (string :tag "Key")
		       (cons :tag "How"
			(string :tag "Command")
			(string :tag "Next Key")))))

;; drop this?
(defcustom LilyPond-file-extension ".ly"
  "*File extension used in LilyPond sources."
  :group 'LilyPond
  :type 'string)


(defcustom LilyPond-expand-alist
  '(
    ("%s" . ".ly")
    ("%t" . ".tex")
    ("%d" . ".dvi")
    ("%f" . ".pdf")
    ("%p" . ".ps")
    ("%l" . ".tex")
    ("%x" . ".tely")
    ("%m" . ".midi")
    )

  "Alist of expansion strings for LilyPond command names."
  :group 'LilyPond
  :type '(repeat (cons :tag "Alist item"
		  (string :tag "Symbol")
		  (string :tag "Expansion"))))


(defcustom LilyPond-command-Show "View"
  "*The default command to show (view or print) a LilyPond file.
Must be the car of an entry in `LilyPond-command-alist'."
  :group 'LilyPond
  :type 'string)
  (make-variable-buffer-local 'LilyPond-command-Show)

(defcustom LilyPond-command-Print "Print"
  "The name of the Print entry in LilyPond-command-Print."
  :group 'LilyPond
  :type 'string)

(defun LilyPond-find-required-command (command file)
  "Find the first command in the chain that is needed to run
 (input file is newer than the output file)"
  (let* ((entry (cdr (assoc command LilyPond-command-alist)))
	(next-command (nth 3 entry)))
    (if (null next-command)
	command
      (let* ((src-string (nth 1 entry))
	    (input (LilyPond-command-expand src-string file))
	    (output (LilyPond-command-expand (nth 2 entry) file)))
	(if (or (file-newer-than-file-p input output)
		(and (equal "%s" src-string)
		     (not (equal (buffer-name) file))
		     (file-newer-than-file-p (buffer-name)
					     output)))
	    command
	  (LilyPond-find-required-command next-command file))))))

(defun LilyPond-command-query (name)
  "Query the user for what LilyPond command to use."
  (cond ((string-equal name LilyPond-region-file-prefix)
	 (LilyPond-check-files (concat name ".tex")
			       (list name)
			       (list LilyPond-file-extension)))
	((verify-visited-file-modtime (current-buffer))
	 (and (buffer-modified-p)
	      (y-or-n-p "Save buffer before next command? ")
	      (LilyPond-save-buffer)))
	((y-or-n-p "The command will be invoked to an already saved buffer. Revert it? ")
	 (revert-buffer t t)))

  (let* ((default (LilyPond-find-required-command LilyPond-command-next name))
	 (completion-ignore-case t)
	 (answer (or LilyPond-command-force
		     (completing-read
		      (concat "Command: (default " default ") ")
		      LilyPond-command-alist nil t nil 'LilyPond-command-history))))

    ;; If the answer is "LilyPond" it will not be expanded to "LilyPond"
    (let ((answer (car-safe (assoc answer LilyPond-command-alist))))
      (if (and answer
	       (not (string-equal answer "")))
	  answer
	default))))

(defun LilyPond-command-master ()
  "Run command on the current document."
  (interactive)
  (LilyPond-command-select-master)
  (LilyPond-command (LilyPond-command-query (LilyPond-get-master-file))
		    'LilyPond-get-master-file))

(defun LilyPond-command-lilypond ()
  "Run lilypond for the current document."
  (interactive)
  (LilyPond-command (LilyPond-command-menu "LilyPond") 'LilyPond-get-master-file)
)

(defun LilyPond-command-formatps ()
  "Format the ps output of the current document."
  (interactive)
  (LilyPond-command (LilyPond-command-menu "2PS") 'LilyPond-get-master-file)
)

(defun LilyPond-command-formatmidi ()
  "Format the midi output of the current document."
  (interactive)
  (LilyPond-command (LilyPond-command-menu "2Midi") 'LilyPond-get-master-file))

(defun LilyPond-command-view ()
  "View the output of current document."
  (interactive)
  (LilyPond-command-viewpdf))

(defun LilyPond-command-viewpdf ()
  "View the ps output of current document."
  (interactive)
  (LilyPond-command (LilyPond-command-menu "ViewPDF") 'LilyPond-get-master-file))

(defun LilyPond-command-viewps ()
  "View the ps output of current document."
  (interactive)
  (LilyPond-command (LilyPond-command-menu "ViewPS") 'LilyPond-get-master-file))

;; FIXME, this is broken
(defun LilyPond-region-file (begin end)
  (let (
	;; (dir "./")
 	(dir (LilyPond-temp-directory))
	(base LilyPond-region-file-prefix)
	(ext LilyPond-file-extension))
    (concat dir base ext)))

;;; Commands on Region work if there is an appropriate '\score'.
(defun LilyPond-command-region (begin end)
  "Run LilyPond on the current region."
  (interactive "r")
  (if (or (> begin (point-min)) (< end (point-max)))
      (LilyPond-command-select-region))
  (write-region begin end (LilyPond-region-file begin end) nil 'nomsg)
  (LilyPond-command (LilyPond-command-query
		     (LilyPond-region-file begin end))
		    '(lambda () (LilyPond-region-file begin end)))
  ;; Region may deactivate even if buffer was intact, reactivate?
  ;; Currently, also deactived regions are used.
  )

(defun LilyPond-command-buffer ()
  "Run LilyPond on buffer."
  (interactive)
  (LilyPond-command-select-buffer)
  (LilyPond-command-region (point-min) (point-max)))

(defun LilyPond-command-expand (arg file)
  (cond
   ((listp arg)
    (mapconcat (lambda (arg) (LilyPond-command-expand arg file))
	       arg
	       ""))
   ((and (symbolp arg) (boundp arg)
	 ;; Avoid self-quoting symbols
	 (not (eq (symbol-value arg) arg)))
    (LilyPond-command-expand (symbol-value arg) file))
   ((stringp arg)
    (let ((case-fold-search nil))
      (if (string-match "%" arg)
	  (let* ((b (match-beginning 0))
		 (e (+ b 2))
		 (l (split-file-name file))
		 (dir (car l))
		 (base (cadr l)))
	    (concat (substring arg 0 b)
		    (shell-quote-argument (concat dir base))
		    (LilyPond-command-expand
		     (concat
		      (let ((entry (assoc (substring arg b e)
					  LilyPond-expand-alist)))
			(if entry (cdr entry) ""))
		      (substring arg e))
		     file)))
	arg)))
   (t (error "Bad expansion `%S'" arg))))

(defun LilyPond-shell-process (name buffer command)
  (let ((old (current-buffer)))
    (switch-to-buffer-other-window buffer)
    ;; If we empty the buffer don't see messages scroll by.
    ;; (erase-buffer)

    (start-process-shell-command name buffer command)
    (switch-to-buffer-other-window old)))


(defun LilyPond-command (name file)
  "Run command NAME on the file you get by calling FILE.

FILE is a function return a file name.  It has one optional argument,
the extension to use on the file.

Use the information in LilyPond-command-alist to determine how to run the
command."

  (let ((entry (assoc name LilyPond-command-alist)))
    (if entry
	(let ((command (LilyPond-command-expand (cadr entry)
						(apply file nil)))
	      (jobs nil)
	      (job-string "no jobs"))
	  (if (member name (list "View" "ViewPS"))
	      ;; is USR1 a right signal for viewps?
	      (let ((buffer-xdvi (get-buffer-create (concat "*" name "*"))))
		;; what if XEDITOR is set to gedit or so, should we steal it?
		(if (not (getenv "XEDITOR"))
		    (setenv "XEDITOR" "emacsclient --no-wait +%l:%c %f"))
		(if LilyPond-kick-xdvi
		  (let ((process-xdvi (get-buffer-process buffer-xdvi)))
		    (if process-xdvi
			(signal-process (process-id process-xdvi) 'SIGUSR1)
		      (LilyPond-shell-process name buffer-xdvi command)))
		  (LilyPond-shell-process name buffer-xdvi command)))
	    (progn
	      (if (string-equal name "Midi")
		  (progn
		    (setq command (concat LilyPond-midi-command " " (LilyPond-string-current-midi)))
		    (if (LilyPond-kill-midi)
			(setq job-string nil)))) ; either stop or start playing
	      (if (string-equal name "MidiAll")
		  (progn
		    (setq command (concat LilyPond-all-midi-command " " (LilyPond-string-all-midi)))
		    (LilyPond-kill-midi))) ; stop and start playing
	      (if (and (member name (list "Midi" "MidiAll")) job-string)
		  (if (file-newer-than-file-p
		       (LilyPond-get-master-file)
		       (concat (substring (LilyPond-get-master-file) 0 -3) ".midi"))
		      (if (y-or-n-p "Midi older than source. Reformat midi?")
			  (progn
			    (LilyPond-command-formatmidi)
			    (while (LilyPond-running)
			      (message "Starts playing midi once it is built.")
			      (sit-for 0 100))))))
	      (if (member name (list "LilyPond" "TeX" "2Midi" "2PS"
				     "Book" "LaTeX"))
		  (if (setq jobs (LilyPond-running))
		      (progn
			(setq job-string "Process") ; could also suggest compiling after process has ended
			(while jobs
			  (setq job-string (concat job-string " \"" (pop jobs) "\"")))
			(setq job-string (concat job-string " is already running; kill it to proceed "))
			(if (y-or-n-p job-string)
			    (progn
			      (setq job-string "no jobs")
			      (LilyPond-kill-jobs)
			      (while (LilyPond-running)
				(sit-for 0 100)))
			  (setq job-string nil)))))

	      (setq LilyPond-command-next
		    (let* ((entry (assoc name LilyPond-command-alist))
			   (next-command (nth 3 (cdr entry))))
		      (or next-command
			  LilyPond-command-default)))

	      (if (string-equal job-string "no jobs")
		  (LilyPond-compile-file command name))))))))

(defun LilyPond-mark-active ()
  "Check if there is an active mark."
  (and transient-mark-mode
       (if (string-match "XEmacs\\|Lucid" emacs-version) (mark) mark-active)))

(defun LilyPond-temp-directory ()
  "Temporary file directory for Commands on Region."
  (interactive)
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (concat (temp-directory) "/")
    temporary-file-directory))

;;; Keymap

(defvar LilyPond-mode-map ()
  "Keymap used in `LilyPond-mode' buffers.")

;; Note:  if you make changes to the map, you must do
;;    M-x set-variable LilyPond-mode-map nil
;;    M-x eval-buffer
;;    M-x LilyPond-mode
;; to let the changest take effect

(if LilyPond-mode-map
    ()
  (setq LilyPond-mode-map (make-sparse-keymap))
  ;; Put keys to LilyPond-command-alist and fetch them from there somehow.
  (define-key LilyPond-mode-map "\C-c\C-l" 'LilyPond-command-lilypond)
  (define-key LilyPond-mode-map "\C-c\C-r" 'LilyPond-command-region)
  (define-key LilyPond-mode-map "\C-c\C-b" 'LilyPond-command-buffer)
  (define-key LilyPond-mode-map "\C-c\C-k" 'LilyPond-kill-jobs)
  (define-key LilyPond-mode-map "\C-c\C-c" 'LilyPond-command-master)
  (define-key LilyPond-mode-map "\C-cm" 'LilyPond-command-formatmidi)
  (define-key LilyPond-mode-map "\C-c\C-f" 'LilyPond-command-formatps)
  (define-key LilyPond-mode-map "\C-c\C-s" 'LilyPond-command-view)
  (define-key LilyPond-mode-map "\C-c\C-p" 'LilyPond-command-viewps)
  (define-key LilyPond-mode-map [(control c) return] 'LilyPond-command-current-midi)
  (define-key LilyPond-mode-map [(control c) (control return)] 'LilyPond-command-all-midi)
  (define-key LilyPond-mode-map "\C-x\C-s" 'LilyPond-save-buffer)
  (define-key LilyPond-mode-map "\C-cb" 'LilyPond-what-beat)
  (define-key LilyPond-mode-map "\C-cf" 'font-lock-fontify-buffer)
  (define-key LilyPond-mode-map "\C-ci" 'LilyPond-insert-tag-current)
  ;; the following will should be overridden by LilyPond Quick Insert Mode
  (define-key LilyPond-mode-map "\C-cq" 'LilyPond-quick-insert-mode)
  (define-key LilyPond-mode-map "\C-c;" 'LilyPond-comment-region)
  (define-key LilyPond-mode-map ")" 'LilyPond-electric-close-paren)
  (define-key LilyPond-mode-map ">" 'LilyPond-electric-close-paren)
  (define-key LilyPond-mode-map "}" 'LilyPond-electric-close-paren)
  (define-key LilyPond-mode-map "]" 'LilyPond-electric-close-paren)
  (define-key LilyPond-mode-map "|" 'LilyPond-electric-bar)
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (define-key LilyPond-mode-map [iso-left-tab] 'LilyPond-autocompletion)
    (define-key LilyPond-mode-map [(shift iso-lefttab)] 'LilyPond-autocompletion))
  (define-key LilyPond-mode-map "\C-c\t" 'LilyPond-info-index-search)
  )

;;; Menu Support

;;; This mode was originally LilyPond-quick-note-insert by Heikki Junes.
;;; The original version has been junked since CVS-1.97,
;;; in order to merge the efforts done by Nicolas Sceaux.
;;; LilyPond Quick Insert Mode is a major mode, toggled by C-c q.
(defun LilyPond-quick-insert-mode ()
  "Insert notes with fewer key strokes by using a simple keyboard piano."
  (interactive)
  (progn
    (message "Invoke (C-c q) from keyboard. If you still see this message,") (sit-for 5 0)
    (message "then you have not installed LilyPond Quick Insert Mode (lyqi).") (sit-for 5 0)
    (message "Download lyqi from http://nicolas.sceaux.free.fr/lilypond/lyqi.html,") (sit-for 5 0)
    (message "see installation instructions from lyqi's README -file.") (sit-for 5 0)
    (message "You need also eieio (Enhanced Integration of Emacs Interpreted Objects).") (sit-for 5 0)
    (message "Download eieio from http://cedet.sourceforge.net/eieio.shtml,") (sit-for 5 0)
    (message "see installation instructions from eieio's INSTALL -file.") (sit-for 5 0)
    (message "")
    ))

(defun LilyPond-pre-word-search ()
  "Fetch the alphabetic characters and \\ in front of the cursor."
  (let ((pre "")
	(prelen 0)
	(ch (char-before (- (point) 0))))
    (while (and ch (or (and (>= ch 65) (<= ch 90))  ; not bolp, A-Z
		       (and (>= ch 97) (<= ch 122)) ; a-z
		       (= ch 92)))                  ; \\
      (setq pre (concat (char-to-string ch) pre))
      (setq prelen (+ prelen 1))
      (setq ch (char-before (- (point) prelen))))
    pre))

(defun LilyPond-post-word-search ()
  "Fetch the alphabetic characters behind the cursor."
  (let ((post "")
	(postlen 0)
	(ch (char-after (+ (point) 0))))
    (while (and ch (or (and (>= ch 65) (<= ch 90))    ; not eolp, A-Z
		       (and (>= ch 97) (<= ch 122)))) ; a-z
      (setq post (concat post (char-to-string ch)))
      (setq postlen (+ postlen 1))
      (setq ch (char-after (+ (point) postlen))))
    post))

(defun LilyPond-autocompletion ()
  "Show completions in mini-buffer for the given word."
  (interactive)
  (let ((pre (LilyPond-pre-word-search))
	(post (LilyPond-post-word-search))
	(compsstr ""))
    ;; insert try-completion and show all-completions
    (if (> (length pre) 0)
	(progn
	  (setq trycomp (try-completion pre (LilyPond-add-dictionary-word ())))
	  (if (char-or-string-p trycomp)
	      (if (string-equal (concat pre post) trycomp)
		  (goto-char (+ (point) (length post)))
		(progn
		  (delete-region (point) (+ (point) (length post)))
		  (insert (substring trycomp (length pre) nil))))
	    (progn
	      (delete-region (point) (+ (point) (length post)))
	      (font-lock-fontify-buffer))) ; only inserting fontifies

	(setq complist (all-completions pre (LilyPond-add-dictionary-word ())))
	(while (> (length complist) 0)
	  (setq compsstr (concat compsstr "\"" (car complist) "\" "))
	  (setq complist (cdr complist)))
	(message compsstr)
	(sit-for 0 100)))))

(defun LilyPond-info ()
  "Launch Info for lilypond."
  (interactive)
  (info "lilypond-notation"))

(defun LilyPond-music-glossary-info ()
  "Launch Info for music-glossary."
  (interactive)
  (info "music-glossary"))

(defun LilyPond-internals-info ()
  "Launch Info for lilypond-internals."
  (interactive)
  (info "lilypond-internals"))

(defun LilyPond-info-index-search ()
  "In `*info*'-buffer, launch `info lilypond --index-search word-under-cursor'"
  (interactive)
  (let ((str (concat (LilyPond-pre-word-search) (LilyPond-post-word-search))))
    (if (and (> (length str) 0)
	     (string-equal (substring str 0 1) "\\"))
	(setq str (substring str 1 nil)))
    (LilyPond-info)
    (Info-index str)))

(defun LilyPond-insert-tag-current (&optional word)
  "Set the current tag to be inserted."
  (interactive)
  (if word
      (setq LilyPond-insert-tag-current word))
  (if (memq LilyPond-insert-tag-current LilyPond-menu-keywords)
      (LilyPond-insert-tag)
    (message "No tag was selected from LilyPond->Insert tag-menu.")))

(defun LilyPond-insert-tag ()
  "Insert syntax for given tag. The definitions are in LilyPond-words-filename."
  (interactive)
  (setq b (find-file-noselect (LilyPond-words-filename) t t))
  (let ((word LilyPond-insert-tag-current)
	(found nil)
	(p nil)
	(query nil)
        (m (set-marker (make-marker) 1 (get-buffer b)))
        (distance (if (LilyPond-mark-active)
		      (abs (- (mark-marker) (point-marker))) 0))
       )
   ;; find the place first
   (if (LilyPond-mark-active)
       (goto-char (min (mark-marker) (point-marker))))
   (while (and (not found) (> (get-buffer-size b) (marker-position m)))
    (setq copy (car (copy-alist (list (eval (symbol-name (read m)))))))
    (if (string-equal word copy) (setq found t)))
   (if found (insert word))
   (if (> (get-buffer-size b) (marker-position m))
       (setq copy (car (copy-alist (list (eval (symbol-name (read m))))))))
   (if (not (string-equal "-" copy))
       (setq found nil))
   (while (and found (> (get-buffer-size b) (marker-position m)))
    ;; find next symbol
    (setq copy (car (copy-alist (list (eval (symbol-name (read m)))))))
    ;; check whether it is the word, or the word has been found
    (cond
     ((string-equal "-" copy) (setq found nil))
     ((string-equal "%" copy) (insert " " (read-string "Give Arguments: ")))
     ((string-equal "_" copy)
      (progn
       (setq p (point))
       (goto-char (+ p distance))))
     ((string-equal "\?" copy) (setq query t))
     ((string-equal "\!" copy) (setq query nil))
     ((string-equal "\\n" copy)
      (if (not query)
       (progn (LilyPond-indent-line) (insert "\n") (LilyPond-indent-line))))
     ((string-equal "{" copy)
      (if (not query)
	  (progn (insert " { "))))
     ((string-equal "}" copy)
      (if (not query)
       (progn (insert " } ") (setq query nil) )))
     ((not query)
      (insert copy))
     (query
      (if (y-or-n-p (concat "Proceed with `" copy "'? "))
       (progn (insert copy) (setq query nil))))
   ))
   (if p (goto-char p))
   (kill-buffer b))
)

(defun LilyPond-command-menu-entry (entry)
  ;; Return LilyPond-command-alist ENTRY as a menu item.
  (let ((name (car entry)))
    (cond ((and (string-equal name LilyPond-command-Print)
		LilyPond-printer-list)
	   (let ((command LilyPond-print-command)
		 (lookup 1))
	     (append (list LilyPond-command-Print)
		     (mapcar 'LilyPond-command-menu-printer-entry
			     LilyPond-printer-list))))
	  (t
	   (vector name (list 'LilyPond-command-menu name) t)))))


(easy-menu-define LilyPond-command-menu
  LilyPond-mode-map
  "Menu used in LilyPond mode."
  (append '("Command")
	  '(("Command on"
	     [ "Master File" LilyPond-command-select-master
	       :keys "C-c C-c" :style radio
	       :selected (eq LilyPond-command-current 'LilyPond-command-master) ]
	     [ "Buffer" LilyPond-command-select-buffer
	       :keys "C-c C-b" :style radio
	       :selected (eq LilyPond-command-current 'LilyPond-command-buffer) ]
	     [ "Region" LilyPond-command-select-region
	       :keys "C-c C-r" :style radio
	       :selected (eq LilyPond-command-current 'LilyPond-command-region) ]))
;;;	  (let ((file 'LilyPond-command-on-current))
;;;	    (mapcar 'LilyPond-command-menu-entry LilyPond-command-alist))
;;; Some kind of mapping which includes :keys might be more elegant
;;; Put keys to LilyPond-command-alist and fetch them from there somehow.
	  '([ "LilyPond" LilyPond-command-lilypond t])
	  '([ "2PS" LilyPond-command-formatps t])
	  '([ "2Midi" LilyPond-command-formatmidi t])
	  '([ "Book" (LilyPond-command (LilyPond-command-menu "Book") 'LilyPond-get-master-file) ])
	  '([ "LaTeX" (LilyPond-command (LilyPond-command-menu "LaTeX") 'LilyPond-get-master-file) ])
	  '([ "Kill jobs" LilyPond-kill-jobs t])
	  '("-----")
	  '([ "View" LilyPond-command-view t])
	  '([ "ViewPS" LilyPond-command-viewps t])
	  '("-----")
	  '([ "Midi (toggle)" LilyPond-command-current-midi t])
	  '([ "Midi all" LilyPond-command-all-midi t])
	  ))

(defun LilyPond-menu-keywords-item (arg)
  "Make vector for LilyPond-mode-keywords."
  (vector arg (list 'LilyPond-insert-tag-current arg) :style 'radio :selected (list 'eq 'LilyPond-insert-tag-current arg)))

(defun LilyPond-menu-keywords ()
  "Make Insert Tag menu.

The Insert Tag -menu is split into parts if it is long enough."

  (let ((li (mapcar 'LilyPond-menu-keywords-item LilyPond-menu-keywords))
	(w (round (sqrt (length LilyPond-menu-keywords))))
	(split '())
	(imin 0) imax lw rw)
    (while (< imin (length LilyPond-menu-keywords))
      (setq imax (- (min (+ imin w) (length LilyPond-menu-keywords)) 1))
      (setq lw (nth imin LilyPond-menu-keywords))
      (setq rw (nth imax LilyPond-menu-keywords))
      (add-to-list 'split
         (let ((l (list (concat (substring lw 0 (min 7 (length lw)))
				" ... "
				(substring rw 0 (min 7 (length rw)))))))
	   (while (<= imin imax)
	     (add-to-list 'l (nth imin li))
	     (setq imin (1+ imin)))
	   (reverse l))))
    (if (> (length LilyPond-menu-keywords) 12) (reverse split) li)))

;;; LilyPond-mode-menu should not be interactive, via "M-x LilyPond-<Tab>"
(easy-menu-define LilyPond-mode-menu
  LilyPond-mode-map
  "Menu used in LilyPond mode."
  (append '("LilyPond")
	  '(["Add index menu" LilyPond-add-imenu-menu])
	  (list (cons "Insert tag"
                (cons ["Previously selected" LilyPond-insert-tag-current t]
                (cons "-----"
		      (LilyPond-menu-keywords)))))
	  '(("Miscellaneous"
	     ["Autocompletion"   LilyPond-autocompletion t]
	     ["(Un)comment Region" LilyPond-comment-region t]
	     ["Refontify buffer" font-lock-fontify-buffer t]
	     "-----"
	     ["Quick Insert Mode"  LilyPond-quick-insert-mode :keys "C-c q"]
 	     ))
	  '(("Info"
	     ["LilyPond" LilyPond-info t]
	     ["LilyPond index-search" LilyPond-info-index-search t]
	     ["Music Glossary" LilyPond-music-glossary-info t]
	     ["LilyPond internals" LilyPond-internals-info t]
	     ))
	  ))

(defconst LilyPond-imenu-generic-re "^\\([a-zA-Z]+\\) *="
  "Regexp matching Identifier definitions.")

(defvar LilyPond-imenu-generic-expression
  (list (list nil LilyPond-imenu-generic-re 1))
  "Expression for imenu")

(defun LilyPond-command-select-master ()
  (interactive)
  (message "Next command will be on the master file")
  (setq LilyPond-command-current 'LilyPond-command-master))

(defun LilyPond-command-select-buffer ()
  (interactive)
  (message "Next command will be on the buffer")
  (setq LilyPond-command-current 'LilyPond-command-buffer))

(defun LilyPond-command-select-region ()
  (interactive)
  (message "Next command will be on the region")
  (setq LilyPond-command-current 'LilyPond-command-region))

(defun LilyPond-command-menu (name)
  ;; Execute LilyPond-command-alist NAME from a menu.
  (let ((LilyPond-command-force name))
    (if (eq LilyPond-command-current 'LilyPond-command-region)
	(if (eq (mark t) nil)
	    (progn (message "The mark is not set now") (sit-for 0 500))
	  (progn (if (not (not (LilyPond-mark-active)))
		     (progn (message "Region is not active, using region between inactive mark and current point.") (sit-for 0 500)))
		 (LilyPond-command-region (mark t) (point))))
      (funcall LilyPond-command-current))))

(defun LilyPond-add-imenu-menu ()
  (interactive)
  "Add an imenu menu to the menubar."
  (if (not LilyPond-imenu)
      (progn
	(imenu-add-to-menubar "Index")
	(redraw-frame (selected-frame))
	(setq LilyPond-imenu t))
    (message "%s" "LilyPond-imenu already exists.")))
(put 'LilyPond-add-imenu-menu 'menu-enable '(not LilyPond-imenu))

(define-derived-mode LilyPond-mode prog-mode "LilyPond-mode"
  "Major mode for editing LilyPond music files.

This mode knows about LilyPond keywords and line comments, not about
indentation or block comments.  It features easy compilation, error
finding and viewing of a LilyPond source buffer or region.

COMMANDS
\\{LilyPond-mode-map}
VARIABLES

LilyPond-command-alist\t\talist from name to command"
  ;; set up local variables
  (kill-all-local-variables)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(LilyPond-font-lock-keywords))

  ;; string and comments are fontified explicitly
  (make-local-variable 'font-lock-keywords-only)
  (setq font-lock-keywords-only t)

  ;; Multi-line font-locking needs Emacs 21.1 or newer.
  ;; For older versions there is hotkey "C-c f".
  (make-local-variable 'font-lock-multiline)
  (setq font-lock-multiline t)

  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^[ \t]*$")

  (make-local-variable 'paragraph-start)
  (setq	paragraph-start "^[ \t]*$")

  (make-local-variable 'comment-start)
  (setq comment-start "%")

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%{? *")

  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'block-comment-start)
  (setq block-comment-start "%{")

  (make-local-variable 'block-comment-end)
  (setq block-comment-end   "%}")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'LilyPond-indent-line)

  (LilyPond-mode-set-syntax-table '(?\< ?\> ?\{ ?\}))
  (setq major-mode 'LilyPond-mode)
  (setq mode-name "LilyPond")
  (setq local-abbrev-table LilyPond-mode-abbrev-table)
  (use-local-map LilyPond-mode-map)

  ;; In XEmacs imenu was synched up with: FSF 20.4
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression LilyPond-imenu-generic-expression)
  ;; (imenu-add-to-menubar "Index") ; see LilyPond-add-imenu-menu

  ;; In XEmacs one needs to use 'easy-menu-add'.
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (progn
	(easy-menu-add LilyPond-mode-menu)
	(easy-menu-add LilyPond-command-menu)))

  ;; Use Command on Region even for inactive mark (region).
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (progn
	(setq zmacs-regions nil)
	(make-local-hook 'post-command-hook)) ; XEmacs requires
    (setq mark-even-if-inactive t))

  ;; Context dependent syntax tables in LilyPond-mode
  (add-hook 'post-command-hook 'LilyPond-mode-context-set-syntax-table nil t)

  ;; Turn on paren-mode buffer-locally, i.e., in LilyPond-mode
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (progn
	(make-local-variable 'paren-mode)
	(paren-set-mode 'paren)
	(make-local-variable 'blink-matching-paren)
	(setq blink-matching-paren t)
	)
    (progn
      (make-local-variable 'blink-matching-paren-on-screen)
      (setq blink-matching-paren-on-screen t)
     ))

  ;; run the mode hook. LilyPond-mode-hook use is deprecated
  (run-hooks 'LilyPond-mode-hook))

(defun LilyPond-version ()
  "Echo the current version of `LilyPond-mode' in the minibuffer."
  (interactive)
  (message "Using `LilyPond-mode' version %s" LilyPond-version))

(load-library "lilypond-font-lock")
(load-library "lilypond-indent")
(load-library "lilypond-what-beat")

(defun LilyPond-guile ()
  (interactive)
  (require 'ilisp)
  (guile "lilyguile" (LilyPond-command-expand (cadr (assoc "LilyPond" LilyPond-command-alist))
                                              (funcall 'LilyPond-get-master-file)))
  (comint-default-send (ilisp-process) "(define-module (*anonymous-ly-0*))")
  (comint-default-send (ilisp-process) "(set! %load-path (cons \"/usr/share/ilisp/\" %load-path))")
  (comint-default-send (ilisp-process) "(use-modules (guile-user) (guile-ilisp))")
  (comint-default-send (ilisp-process) "(newline)"))

(provide 'lilypond-mode)
;;; lilypond-mode.el ends here
