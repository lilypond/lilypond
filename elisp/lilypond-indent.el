;;; lilypond-indent.el --- Auto-indentation for lilypond code
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>,
;;;;               2003--2004 Heikki Junes <hjunes@cc.hut.fi>
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
;;;
;;; Heikki Junes <hjunes@cc.hut.fi>
;;; * ond-char paren matching is handled by context dependent syntax tables
;;; * match two-char slurs '\( ... \)' and '\[ ... \]' separately.
;;; * adopt Emacs' f90-comment-region

;;; Chris Jackson <chris@fluffhouse.org.uk>
;;; some code is taken from ESS (Emacs Speaks Statistics) S-mode by A.J.Rossini <rossini@biostat.washington.edu>

;;; Variables for customising indentation style

;;; TODO:
;;;    * currently, in bracket matching one may need a non-bracket
;;;      chararacter between the bracket characters, like ( ( ) )

(defcustom LilyPond-indent-level 2
  "*Indentation of lilypond statements with respect to containing block."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-brace-offset 0
  "*Extra indentation for open braces.
Compares with other text in same context."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-angle-offset 0
  "*Extra indentation for open angled brackets.
Compares with other text in same context."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-square-offset 0
  "*Extra indentation for open square brackets.
Compares with other text in same context."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-scheme-paren-offset 0
  "*Extra indentation for open scheme parens.
Compares with other text in same context."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-close-brace-offset 0
  "*Extra indentation for closing braces."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-close-angle-offset 0
  "*Extra indentation for closing angle brackets."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-close-square-offset 0
  "*Extra indentation for closing square brackets."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-close-scheme-paren-offset 0
  "*Extra indentation for closing scheme parens."
  :group 'LilyPond
  :type 'integer)

(defcustom LilyPond-fancy-comments t
  "*Non-nil means distiguish between %, %%, and %%% for indentation."
  :group 'LilyPond
  :type 'boolean)

(defcustom LilyPond-comment-region "%%%"
  "*String inserted by \\[LilyPond-comment-region]\
 at start of each line in region."
  :group 'LilyPond
  :type 'string)

(defun LilyPond-comment-region (beg-region end-region)
  "Comment/uncomment every line in the region.
Insert LilyPond-comment-region at the beginning of every line in the region
or, if already present, remove it."
  (interactive "*r")
  (let ((end (make-marker)))
    (set-marker end end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (looking-at (regexp-quote LilyPond-comment-region))
	(delete-region (point) (match-end 0))
      (insert LilyPond-comment-region))
    (while (and  (zerop (forward-line 1))
		 (< (point) (marker-position end)))
      (if (looking-at (regexp-quote LilyPond-comment-region))
	  (delete-region (point) (match-end 0))
	(insert LilyPond-comment-region)))
    (set-marker end nil)))

(defun LilyPond-calculate-indent ()
  "Return appropriate indentation for current line as lilypond code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string"
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state)
      (setq containing-sexp (save-excursion (LilyPond-scan-containing-sexp)))
      (beginning-of-defun)
      (while (< (point) indent-point)
	(setq state (parse-partial-sexp (point) indent-point 0)))
      ;; (setq containing-sexp (car (cdr state))) is the traditional way for languages
      ;; with simpler parenthesis delimiters
      (cond ((nth 3 state) 
	     ;; point is in the middle of a string 
	     nil)
	    ((nth 4 state)
	     ;; point is in the middle of a block comment
	     (LilyPond-calculate-indent-within-blockcomment))
	    ((null containing-sexp)
	     ;; Line is at top level - no indent
	     (beginning-of-line)
	     0)
	    (t
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (LilyPond-backward-to-noncomment containing-sexp)
	     ;; Now we get the answer.
	     ;; Position following last unclosed open.
	     (goto-char containing-sexp)
	     (or
	      ;; Is line first statement after an open brace or bracket?
	      ;; If no, find that first statement and indent like it.
	      (save-excursion
		(forward-char 1)
		;; Skip over comments following open brace.
		(skip-chars-forward " \t\n")
		(cond ((looking-at "%{")
		       (while  (progn 
				 (and (not (looking-at "%}"))
				      (< (point) (point-max))))
			 (forward-line 1)
			 (skip-chars-forward " \t\n"))
		       (forward-line 1)
		       (skip-chars-forward " \t\n"))
		      ((looking-at "%")
		       (while (progn (skip-chars-forward " \t\n")
				     (looking-at "%"))
			 (forward-line 1))))
		;; The first following code counts
		;; if it is before the line we want to indent.
		(and (< (point) indent-point)
		     (current-column)))
	      ;; If no previous statement,
	      ;; indent it relative to line brace is on.
	      ;; For open brace in column zero, don't let statement
	      ;; start there too.  If LilyPond-indent-level is zero, use
	      ;; LilyPond-brace-offset instead
	      (+ (if (and (bolp) (zerop LilyPond-indent-level))
		     (cond ((= (following-char) ?{)
			    LilyPond-brace-offset)
			   ((= (following-char) ?<)
			    LilyPond-angle-offset)
			   ((= (following-char) ?\[)
			    LilyPond-square-offset)
			   ((= (following-char) ?\))
			    LilyPond-scheme-paren-offset)
			   (t
			    0))
		   LilyPond-indent-level)
		 (progn
		   (skip-chars-backward " \t")
		   (current-indentation)))))))))


(defun LilyPond-indent-line ()
  "Indent current line as lilypond code.
Return the amount the indentation changed by."
  (let ((indent (LilyPond-calculate-indent))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  (t
	   (skip-chars-forward " \t")
	   (if (and LilyPond-fancy-comments (looking-at "%%%\\|%{\\|%}"))
	       (setq indent 0))
	   (if (and LilyPond-fancy-comments
		    (looking-at "%")
		    (not (looking-at "%%\\|%{\\|%}")))
	       (setq indent comment-column)
	     (if (eq indent t) (setq indent 0))
	     (if (listp indent) (setq indent (car indent)))
	     (cond
	      ((= (following-char) ?})
	       (setq indent  (+ indent (- LilyPond-close-brace-offset LilyPond-indent-level))))
	      ((= (following-char) ?>)
	       (setq indent  (+ indent (- LilyPond-close-angle-offset LilyPond-indent-level))))
	      ((= (following-char) ?\])
	       (setq indent  (+ indent (- LilyPond-close-square-offset LilyPond-indent-level))))
	      ((and (= (following-char) ?\)) (LilyPond-inside-scheme-p))
	       (setq indent  (+ indent (- LilyPond-close-scheme-paren-offset LilyPond-indent-level))))
	      ((= (following-char) ?{)
	       (setq indent  (+ indent LilyPond-brace-offset)))
	      ((= (following-char) ?<)
	       (setq indent  (+ indent LilyPond-angle-offset)))
	      ((= (following-char) ?\[)
	       (setq indent  (+ indent LilyPond-square-offset)))
	      ((and (= (following-char) ?\() (LilyPond-inside-scheme-p))
	       (setq indent  (+ indent LilyPond-scheme-paren-offset)))
	      ))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.
      ;; Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))


(defun LilyPond-inside-comment-p ()
  "Return non-nil if point is inside a line or block comment"
  (setq this-point (point))
  (or (save-excursion (beginning-of-line)
		      (skip-chars-forward " \t")
		      (looking-at "%"))
      (save-excursion 
	;; point is in the middle of a block comment
	(setq lastopen  (save-excursion (re-search-backward "%{[ \\t]*" (point-min) t)))
	(setq lastclose (save-excursion (re-search-backward "%}[ \\t]*" (point-min) t)))
	(if (or (and (= (char-before) ?%) (= (char-after) ?{))
		(and (= (char-after)  ?%) (= (char-after (1+ (point))) ?{)))
	    (setq lastopen (save-excursion (backward-char) (point))))
	(and 
	 lastopen
	 (or (not lastclose)
	     (<= lastclose lastopen))))
      ))


(defun LilyPond-inside-string-or-comment-p ()
  "Test if point is inside a string or a comment"
  (setq this-point (point))
  (or (save-excursion (beginning-of-line)
		      (skip-chars-forward " \t")
		      (looking-at "%"))
      (save-excursion 
	(beginning-of-defun)
	(while (< (point) this-point)
	  (setq state (parse-partial-sexp (point) this-point 0)))
	(cond ((nth 3 state) 
	       ;; point is in the middle of a string 
	       t )
	      ((nth 4 state)
	       ;; point is in the middle of a block comment
	       t ) 
	      (t
	       nil)))))


(defun LilyPond-backward-over-blockcomments (lim)
  "Move point back to closest non-whitespace character not part of a block comment"
  (setq lastopen  (save-excursion (re-search-backward "%{[ \\t]*" lim t)))
  (setq lastclose (save-excursion (re-search-backward "%}[ \\t]*" lim t)))
  (if lastopen
      (if lastclose
	  (if (<= lastclose lastopen)
	      (goto-char lastopen))
	(goto-char lastopen)))
  (skip-chars-backward " %\t\n\f"))


(defun LilyPond-backward-over-linecomments (lim)
  "Move point back to the closest non-whitespace character not part of a line comment.
Argument LIM limit."
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (beginning-of-line)
      (search-forward "%" opoint 'move)
      (skip-chars-backward " \t%")
      (setq stop (or (/= (preceding-char) ?\n) (<= (point) lim)))
      (if stop (point)
	(beginning-of-line)))))


(defun LilyPond-backward-to-noncomment (lim)
  "Move point back to closest non-whitespace character not part of a comment"
  (LilyPond-backward-over-linecomments lim)
  (LilyPond-backward-over-blockcomments lim))


(defun LilyPond-calculate-indent-within-blockcomment ()
  "Return the indentation amount for line inside a block comment."
  (let (end percent-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (re-search-forward "%{[ \t]*" end t)
	   (goto-char (1+ (match-beginning 0))))
      (if (and (looking-at "[ \t]*$") (= (preceding-char) ?\%))
	  (1+ (current-column))
	(current-column)))))


;; Key:   Type of bracket (character). 
;; Value: Pair of regexps representing the corresponding open and close bracket
;; () are treated specially (need to indent in Scheme but not in music)

(defconst LilyPond-parens-regexp-alist
  `( ( ?>  .  ("\\([^\\]\\|^\\)<" . "\\([^ \\n\\t_^-]\\|[_^-][-^]\\|\\s-\\)\\s-*>"))
     ;; a b c->, a b c^> and a b c_> are not close-angle-brackets, they're accents
     ;; but a b c^-> and a b c^^> are close brackets with tenuto/marcato before them
     ;; also \> and \< are hairpins
     ;; duh .. a single '>', as in chords '<< ... >>', was not matched here
     ( ?}  .  ("{" . "}"))
     ;; ligatures  '\[ ... \]' are skipped in the following expression
     ( ?\]  .  ("\\([^\\]\\([\\][\\]\\)*\\|^\\)[[]" . "\\([^\\]\\([\\][\\]\\)*\\|^\\)[]]"))
     ( "\\]" . ("\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][[]" . "\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][]]"))
     ( "\\)" . ("\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][(]" . "\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][)]"))
     ))


(defconst LilyPond-parens-alist
  `( ( ?<  .  ?> )    
     ( ?{  .  ?} )    
     ( ?\[  .  ?\] )
     ( "\\["  .  "\\]" )
     ( ?\(  .  ?\) )
     ( "\\("  .  "\\)" )
     ))


(defun LilyPond-matching-paren (bracket-type)
  "Returns the open corresponding to the close specified by bracket-type, or vice versa"
  (cond ( (member bracket-type (mapcar 'car LilyPond-parens-alist))
	  (cdr (assoc bracket-type LilyPond-parens-alist)) )
	( (member bracket-type (mapcar 'cdr LilyPond-parens-alist))
	  (car (rassoc bracket-type LilyPond-parens-alist)) )
	nil))


(defun LilyPond-scan-containing-sexp (&optional bracket-type slur-paren-p dir)
  "Move point to the beginning of the deepest parenthesis pair enclosing point. 

If the optional argument bracket-type, a character representing a
close bracket such as ) or }, is specified, then the parenthesis pairs
searched are limited to this type.

If the optional argument slur-paren-p is non-nil, then slur
parentheses () are considered as matching pairs. Otherwise Scheme
parentheses are considered to be matching pairs, but slurs are not.
slur-paren-p defaults to nil.
"
;;; An user does not call this function directly, or by a key sequence.
  ;;  (interactive)
  (let ( (level (if (not (eq dir 1)) 1 -1))
	 (regexp-alist LilyPond-parens-regexp-alist) 
	 (oldpos (point))
	 (assoc-bracket-type (if (not (eq dir 1)) bracket-type (LilyPond-matching-paren bracket-type))))
    
    (if (LilyPond-inside-scheme-p)
	(setq paren-regexp "(\\|)")
      (if slur-paren-p
	  ;; expressional slurs  '\( ... \)' are not taken into account
	  (setq regexp-alist (cons '( ?\) . ("\\([^\\]\\([\\][\\]\\)*\\|^\\)(" . "\\([^\\]\\([\\][\\]\\)*\\|^\\))")) regexp-alist)))
      (if (member assoc-bracket-type (mapcar 'car regexp-alist))
	  (progn (setq paren-regexp (cdr (assoc assoc-bracket-type regexp-alist)))
		 (setq paren-regexp (concat (car paren-regexp) "\\|" (cdr paren-regexp))))
	(setq paren-regexp (concat (mapconcat 'car (mapcar 'cdr regexp-alist) "\\|") "\\|"
				   (mapconcat 'cdr (mapcar 'cdr regexp-alist) "\\|")))))
    ;; match concurrent one-char opening and closing slurs
    (if (and (eq dir 1)
	     (not (sequencep bracket-type))
	     (eq (char-syntax (or (char-after oldpos) 0)) ?\()
	     (not (eq (char-after oldpos) ?<)))
	;; anyway do not count open slur, since already level = -1
        (progn (forward-char 1)
	       (if (eq (following-char) 
		       (LilyPond-matching-paren (char-after oldpos)))
		   ;; matching char found, go after it and set level = 0
		   (progn (forward-char 1)
			  (setq level 0)))))
    ;; browse the code until matching slur is found, or report mismatch
    (while (and (if (not (eq dir 1)) 
		    (> level 0) 
		  (< level 0))
		;; dir tells whether to search backward or forward
		(if (not (eq dir 1))
		    (re-search-backward paren-regexp nil t)
		  (re-search-forward paren-regexp nil t))
		;; note: in case of two-char bracket only latter is compared
		(setq match (char-before (match-end 0))))
;;;      (message "%d" level) (sit-for 0 300)
      (if (not (save-excursion (goto-char (match-end 0))
			       ;; skip over strings and comments
			       (LilyPond-inside-string-or-comment-p)))
	  (if (memq match '(?} ?> ?\] ?\)))
	      ;; count closing brackets
	      (progn (setq level (1+ level))
		     ;; slurs may be close to each other, e.g.,
		     ;; a single '>' was not matched .. need to be corrected
		     (if (and (eq dir 1) (eq (char-after (match-end 0)) match))
			 (if (/= level 0)
			     (progn
			       (setq level (1+ level))
			       (forward-char 1))))
;;;		     (message "%d %c" level match) (sit-for 0 300)
		     ;; hmm..
		     (if (and (= match ?>) 
			      (looking-at ".\\s-+>\\|\\({\\|}\\|<\\|>\\|(\\|)\\|[][]\\)>"))
			 (forward-char 1)))
	    ;; count opening brackets
	    (progn (setq level (1- level))
;;;		   (message "%d %c" level match) (sit-for 0 300)
		   ;; hmm..
		   (if (and (= match ?<)
			    (looking-at ".\\s-+<\\|\\({\\|}\\|<\\|>\\|(\\|)\\|[][]\\)<"))
		       (forward-char 1))))))
    ;; jump to the matching slur
    (if (not (eq dir 1))
	(progn
	  (if (sequencep bracket-type)
	      ;; match the latter char in two-char brackets
	      (if (looking-at "..[][)(]") (forward-char 1)))
	  ;; if the following char is not already a slur
	  (if (and (not (looking-at "[)(]"))
		   ;; match the slur which follows
		   (looking-at ".[][><)(]")) (forward-char 1)))
      (backward-char 1))
    (if (= level 0) 
	(point)
      (progn (goto-char oldpos)
	     nil))))


(defun LilyPond-inside-scheme-p ()
  "Tests if point is inside embedded Scheme code"
;;; An user does not call this function directly, or by a key sequence.
  ;;  (interactive)
  (let ( (test-point (point))
	 (level 0) )
    (save-excursion 
      (if (or (and (eq (char-after (point)) ?\()
		   (save-excursion
		     (skip-chars-backward "'`")
		     (memq (char-before) '(?# ?$))))
	      (and (re-search-backward "[#$][`']?(" nil t)
		   (progn 
		     (search-forward "(")
		     (setq level 1)
		     (while (and (> level 0)
				 (re-search-forward "[()]" test-point t)
				 (setq match (char-after (match-beginning 0)))
				 (<= (point) test-point))
		       (if (= match ?\()
			   (setq level (1+ level))
			 (setq level (1- level))))
		     (> level 0))))
	  t
	nil))))


;;; Largely taken from the 'blink-matching-open' in lisp/simple.el in
;;; the Emacs distribution.

(defun LilyPond-blink-matching-paren (&optional dir)
  "Move cursor momentarily to the beginning of the sexp before
point. In lilypond files this is used for closing ), ], } and >, whereas the
builtin 'blink-matching-open' is not used. In syntax table, see
`lilypond-font-lock.el', all brackets are punctuation characters."
;;; An user does not call this function directly, or by a key sequence.
  ;;  (interactive)
  (let ( (oldpos (point))
	 (level 0) 
	 (mismatch) )
    (if (not (or (equal this-command 'LilyPond-electric-close-paren)
		 (eq dir 1)))
	(goto-char (setq oldpos (- oldpos 1))))
    ;; Test if a ligature \] or expressional slur \) was encountered
    (setq bracket-type (char-after (point)))
    (setq char-before-bracket-type nil)
    (if (memq bracket-type '(?\] ?\) ?\[ ?\())
      (progn 
	(setq np -1)
	(while (eq (char-before (- (point) (setq np (+ np 1)))) ?\\)
	  (setq char-before-bracket-type (if char-before-bracket-type nil ?\\)))
        (if (eq char-before-bracket-type ?\\)
	    (setq bracket-type (string char-before-bracket-type bracket-type)))))
    (when blink-matching-paren-distance
      (narrow-to-region
       (max (point-min) (- (point) blink-matching-paren-distance))
       (min (point-max) (+ (point) blink-matching-paren-distance))))
    (if (and (equal this-command 'LilyPond-electric-close-paren)
	     (memq bracket-type '(?> ?} ?< ?{)))
	;; < { need to be mutually balanced and nested, so search backwards for both of these bracket types 
	(LilyPond-scan-containing-sexp nil nil dir)  
      ;; whereas ( ) slurs within music don't, so only need to search for ( )
      ;; use same mechanism for [ ] slurs
      (LilyPond-scan-containing-sexp bracket-type t dir))
    (setq blinkpos (point))
    (setq mismatch
	  (or (null (LilyPond-matching-paren (char-after blinkpos)))
	      (/= (char-after oldpos)
		  (LilyPond-matching-paren (char-after blinkpos)))))
    (if mismatch (progn (setq blinkpos nil)
			(message "Mismatched parentheses")))
    (if (and blinkpos
	     (equal this-command 'LilyPond-electric-close-paren))
	(if (pos-visible-in-window-p)
	    (and blink-matching-paren-on-screen
		 (sit-for blink-matching-delay))
	  (message
	   "Matches %s"
	   ;; Show what precedes the open in its line, if anything.
	   (if (save-excursion
		 (skip-chars-backward " \t")
		 (not (bolp)))
	       (buffer-substring (progn (beginning-of-line) (point))
				 (1+ blinkpos))
	     ;; Show what follows the open in its line, if anything.
	     (if (save-excursion
		   (forward-char 1)
		   (skip-chars-forward " \t")
		   (not (eolp)))
		 (buffer-substring blinkpos
				   (progn (end-of-line) (point)))
	       ;; Otherwise show the previous nonblank line,
	       ;; if there is one.
	       (if (save-excursion
		     (skip-chars-backward "\n \t")
		     (not (bobp)))
		   (concat
		    (buffer-substring (progn
					(skip-chars-backward "\n \t")
					(beginning-of-line)
					(point))
				      (progn (end-of-line)
					     (skip-chars-backward " \t")
					     (point)))
		    ;; Replace the newline and other whitespace with `...'.
		    "..."
		    (buffer-substring blinkpos (1+ blinkpos)))
		 ;; There is nothing to show except the char itself.
		 (buffer-substring blinkpos (1+ blinkpos))))))))
    (if (not (equal this-command 'LilyPond-electric-close-paren))
	(goto-char (setq oldpos (+ oldpos 1)))
      (goto-char oldpos))
    (if (not (eq dir 1))
	blinkpos
      (+ blinkpos 1))))


(defun LilyPond-electric-close-paren ()
  "Blink on the matching open paren when a >, ), } or ] is inserted"
  (interactive)
  (let ((oldpos (point)))
    (self-insert-command 1)
    ;; Refontify buffer if a block-comment-ender '%}' is inserted
    (if (and (eq (char-before (point)) ?})
	     (eq (char-before (- (point) 1)) ?%))
	(font-lock-fontify-buffer)
      ;; Match paren if the cursor is not inside string or comment.
      (if (and blink-matching-paren
	       (not (LilyPond-inside-string-or-comment-p))
	       (save-excursion (re-search-backward 
				(concat (mapconcat 'cdr (mapcar 'cdr LilyPond-parens-regexp-alist) "\\|") "\\|)") nil t)
			       (eq oldpos (1- (match-end 0)))))
	  (progn (backward-char 1)
		 (LilyPond-blink-matching-paren)
		 (forward-char 1))))))

(defun LilyPond-scan-sexps (pos dir) 
  "This function is redefined to be used in Emacs' show-paren-function and
in XEmacs' paren-highlight."
  (LilyPond-blink-matching-paren dir))
