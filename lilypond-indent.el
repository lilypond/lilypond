;;; lilypond-indent.el --- Auto-indentation for lilypond code
;;;
;;; Chris Jackson <chris@fluffhouse.org.uk>
;;; some code is taken from ESS (Emacs Speaks Statistics) S-mode by A.J.Rossini <rossini@biostat.washington.edu>

;;; Variables for customising indentation style

(defcustom LilyPond-indent-level 4
  "*Indentation of lilypond statements with respect to containing block.")

(defcustom LilyPond-brace-offset 0
  "*Extra indentation for open braces.
Compares with other text in same context.")

(defcustom LilyPond-angle-offset 0
  "*Extra indentation for open angled brackets .
Compares with other text in same context.")

(defcustom LilyPond-scheme-paren-offset 0
  "*Extra indentation for open scheme parens .
Compares with other text in same context.")

(defcustom LilyPond-close-brace-offset 0
  "*Extra indentation for closing braces.")

(defcustom LilyPond-close-angle-offset 0
  "*Extra indentation for closing angle brackets.")

(defcustom LilyPond-close-scheme-paren-offset 0
  "*Extra indentation for closing scheme parens.")

(defcustom LilyPond-fancy-comments t
  "*Non-nil means distiguish between %, %%, and %%% for indentation.")


(defun LilyPond-calculate-indent ()
  "Return appropriate indentation for current line as lilypond code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string"
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state)
      (setq containing-sexp (save-excursion (LilyPond-beginning-of-containing-sexp)))
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
	      ((and (= (following-char) ?\)) (LilyPond-inside-scheme-p))
	       (setq indent  (+ indent (- LilyPond-close-scheme-paren-offset LilyPond-indent-level))))
	      ((= (following-char) ?{)
	       (setq indent  (+ indent LilyPond-brace-offset)))
	      ((= (following-char) ?<)
	       (setq indent  (+ indent LilyPond-angle-offset)))
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
;; Value: Pair of regexps representing the corresponding open and close bracket"
;; () are treated specially (need to indent in Scheme but not in music), and []
;; are handled by the syntax table

(defconst LilyPond-parens-regexp-alist
  `( ( ?>  .  ("[^\\]<" . "[^ \\n\\t_^-]\\s-*>\\|[_^-]\\s-*[-^]\\s-*>"))
     ;; a b c->, a b c^> and a b c_> are not close-angle-brackets, they're accents
     ;; but a b c^-> and a b c^^> are close brackets with tenuto/marcato before them
     ;; also \> and \< are hairpins
     ( ?}  .  ("{" . "}"))
     ))


(defconst LilyPond-parens-alist
  `( ( ?<  .  ?> )    
     ( ?{  .  ?} )    
     ( ?\(  .  ?\) )
     ))


(defun LilyPond-matching-paren (bracket-type)
  "Returns the open corresponding to the close specified by bracket-type, or vice versa"
  (cond ( (memq bracket-type (mapcar 'car LilyPond-parens-alist))
	  (cdr (assoc bracket-type LilyPond-parens-alist)) )
	( (memq bracket-type (mapcar 'cdr LilyPond-parens-alist))
	  (car (rassoc bracket-type LilyPond-parens-alist)) )
	nil))


(defun LilyPond-beginning-of-containing-sexp (&optional bracket-type slur-paren-p)
  "Move point to the beginning of the deepest parenthesis pair enclosing point. 

If the optional argument bracket-type, a character representing a
close bracket such as ) or }, is specified, then the parenthesis pairs
searched are limited to this type.

If the optional argument slur-paren-p is non-nil, then slur
parentheses () are considered as matching pairs. Otherwise Scheme
parentheses are considered to be matching pairs, but slurs are not.
slur-paren-p defaults to nil.
"
  (interactive)
  (let ( (level 1) 
	 (regexp-alist LilyPond-parens-regexp-alist) 
	 (oldpos (point) ) )
    (if (LilyPond-inside-scheme-p)
	(setq paren-regexp "(\\|)")
      (if slur-paren-p
	  (setq regexp-alist (cons '( ?\) . ("(" . ")")) regexp-alist)))
      (if (memq bracket-type (mapcar 'car regexp-alist))
	  (progn (setq paren-regexp (cdr (assoc bracket-type regexp-alist)))
		 (setq paren-regexp (concat (car paren-regexp) "\\|" (cdr paren-regexp))))
	(setq paren-regexp (concat (mapconcat 'car (mapcar 'cdr regexp-alist) "\\|") "\\|"
				   (mapconcat 'cdr (mapcar 'cdr regexp-alist) "\\|")))))
    (while (and (> level 0) 
		(re-search-backward paren-regexp nil t)
		(setq match (char-before (match-end 0))))
      (if (not (save-excursion (goto-char (match-end 0)) 
			       (LilyPond-inside-string-or-comment-p)))
	  (if (memq match '(?} ?> ?\)))
	      (progn (setq level (1+ level))
		     (if (and (= match ?>) 
			      (looking-at ".\\s-+>\\|\\({\\|}\\|<\\|>\\|(\\|)\\)>"))
			 (forward-char 1)))
	    (progn (setq level (1- level))
		   (if (and (= match ?<)
			    (looking-at ".\\s-+<\\|\\({\\|}\\|<\\|>\\|(\\|)\\)<"))
		       (forward-char 1))))))
    (if (looking-at ".<\\|.>") (forward-char 1))
    (if (= level 0) 
	(point)
      (progn (goto-char oldpos)
	     nil))))


(defun LilyPond-inside-scheme-p ()
  "Tests if point is inside embedded Scheme code"
  (interactive)
  (let ( (test-point (point))
	 (level 0) )
    (save-excursion 
      (if (or (and (/= (point) (point-max))
		   (= (char-after (point)) ?\()
		   (or (= (char-after (- (point) 1)) ?#)
		       (and (= (char-after (- (point) 2)) ?#)
			    (= (char-after (- (point) 1)) ?`))))
	      (and (re-search-backward "#(\\|#`(" nil t)
		   (progn 
		     (search-forward "(")
		     (setq level 1)
		     (while (and (> level 0)
				 (re-search-forward "(\\|)" test-point t)
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

(defun LilyPond-blink-matching-open (bracket-type)
  "Move cursor momentarily to the beginning of the sexp before
point. In lilypond files this is used for closing ), } and >, whereas the
builtin 'blink-matching-open' is used for closing ],  which is in
the syntax table"
  (interactive)
  (let ( (oldpos (point))
	 (level 0) 
	 (mismatch) )
    (save-restriction
      (if blink-matching-paren-distance
	  (narrow-to-region (max (point-min)
				 (- (point) blink-matching-paren-distance))
			    oldpos)))
    (if (memq bracket-type '(?> ?}))
	;; < { need to be mutually balanced and nested, so search backwards for both of these bracket types 
	(LilyPond-beginning-of-containing-sexp nil nil)  
      ;; whereas ( ) slurs within music don't, so only need to search for ( )
      (LilyPond-beginning-of-containing-sexp bracket-type t))
    (setq blinkpos (point))
    (setq mismatch
	  (or (null (LilyPond-matching-paren (char-after blinkpos)))
	      (/= (char-after oldpos)
		  (LilyPond-matching-paren (char-after blinkpos)))))
    (if mismatch (progn (setq blinkpos nil)
			(message "Mismatched parentheses")))
    (if blinkpos
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
    (goto-char oldpos)))


(defun LilyPond-electric-close-paren ()
  "Blink on the matching open paren when a > or ) is inserted"
  (interactive)
  (let ((oldpos (point)))
    (self-insert-command 1)
    (setq close-char (char-before (point)))
    (if (and blink-matching-paren
	     (not (LilyPond-inside-string-or-comment-p))
	     (save-excursion (re-search-backward 
			      (concat (mapconcat 'cdr (mapcar 'cdr LilyPond-parens-regexp-alist) "\\|") "\\|)") nil t)
			     (eq oldpos (1- (match-end 0)))))
	(progn (backward-char 1)
	       (LilyPond-blink-matching-open close-char)
	       (forward-char 1)))))


;;; TODO:
;;; emulate show-paren-mode 
