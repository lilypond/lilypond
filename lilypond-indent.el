;;; lilypond-indent.el --- Auto-indentation for lilypond code
;;;
;;; Heikki Junes <hjunes@cc.hut.fi>
;;; * introduce Lilypond-show-paren-function for Emacs and 
;;;             Lilypond-paren-highlight for XEmacs
;;; * match two-char slurs '\( ... \)' and '\[ ... \]' separately.
;;; * adopt Emacs' f90-comment-region

;;; Chris Jackson <chris@fluffhouse.org.uk>
;;; some code is taken from ESS (Emacs Speaks Statistics) S-mode by A.J.Rossini <rossini@biostat.washington.edu>

;;; Variables for customising indentation style

;;; TODO:
;;;    * currently, in bracket matching one may need a non-bracket 
;;;      chararacter between the bracket characters, like ( ( ) )
;;;    * in syntax-highlighting slurs are not always highlighted the right way
;;;      e.g. opening slurs are found found better in "#( ( ) ( ) )" than
;;;      opening slurs
;;;    * Mouse double-clicks should use LilyPond-scan-sexps for slur matching.

(defcustom LilyPond-indent-level 4
  "*Indentation of lilypond statements with respect to containing block.")

(defcustom LilyPond-brace-offset 0
  "*Extra indentation for open braces.
Compares with other text in same context.")

(defcustom LilyPond-angle-offset 0
  "*Extra indentation for open angled brackets.
Compares with other text in same context.")

(defcustom LilyPond-square-offset 0
  "*Extra indentation for open square brackets.
Compares with other text in same context.")

(defcustom LilyPond-scheme-paren-offset 0
  "*Extra indentation for open scheme parens.
Compares with other text in same context.")

(defcustom LilyPond-close-brace-offset 0
  "*Extra indentation for closing braces.")

(defcustom LilyPond-close-angle-offset 0
  "*Extra indentation for closing angle brackets.")

(defcustom LilyPond-close-square-offset 0
  "*Extra indentation for closing square brackets.")

(defcustom LilyPond-close-scheme-paren-offset 0
  "*Extra indentation for closing scheme parens.")

(defcustom LilyPond-fancy-comments t
  "*Non-nil means distiguish between %, %%, and %%% for indentation.")

(defcustom LilyPond-comment-region "%%$"
  "*String inserted by \\[LilyPond-comment-region]\
 at start of each line in region.")

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
			   ((= (following-char) ?[) 
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
	      ((= (following-char) ?])
	       (setq indent  (+ indent (- LilyPond-close-square-offset LilyPond-indent-level))))
	      ((and (= (following-char) ?\)) (LilyPond-inside-scheme-p))
	       (setq indent  (+ indent (- LilyPond-close-scheme-paren-offset LilyPond-indent-level))))
	      ((= (following-char) ?{)
	       (setq indent  (+ indent LilyPond-brace-offset)))
	      ((= (following-char) ?<)
	       (setq indent  (+ indent LilyPond-angle-offset)))
	      ((= (following-char) ?[)
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
     ( ?]  .  ("\\([^\\]\\([\\][\\]\\)*\\|^\\)[[]" . "\\([^\\]\\([\\][\\]\\)*\\|^\\)[]]"))
     ( "\\]" . ("\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][[]" . "\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][]]"))
     ( "\\)" . ("\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][(]" . "\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][)]"))
     ))


(defconst LilyPond-parens-alist
  `( ( ?<  .  ?> )    
     ( ?{  .  ?} )    
     ( ?[  .  ?] )
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
	     (eq (char-syntax (char-after oldpos)) ?\()
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
	  (if (memq match '(?} ?> ?] ?\)))
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
    (if (memq bracket-type '(?] ?\) ?[ ?\())
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

;;; From Emacs' paren.el, with minimal changes (see "LilyPond"-lines)
;; Find the place to show, if there is one,
;; and show it until input arrives.
(defun LilyPond-show-paren-function ()
  (if LilyPond-show-paren-mode
      (let (pos dir mismatch face (oldpos (point)))
	(cond ((eq (char-syntax (preceding-char)) ?\))
	       (setq dir -1))
	      ((eq (char-syntax (following-char)) ?\()
	       (setq dir 1)))
	;;
	;; Find the other end of the sexp.
	(when (and dir
		   (not (LilyPond-inside-string-or-comment-p)))
	  (save-excursion
	    (save-restriction
	      ;; Determine the range within which to look for a match.
	      (when blink-matching-paren-distance
		(narrow-to-region
		 (max (point-min) (- (point) blink-matching-paren-distance))
		 (min (point-max) (+ (point) blink-matching-paren-distance))))
	      ;; Scan across one sexp within that range.
	      ;; Errors or nil mean there is a mismatch.
	      (condition-case ()
		  (setq pos (LilyPond-scan-sexps (point) dir))
		(error (setq pos t mismatch t)))
	      ;; If found a "matching" paren, see if it is the right
	      ;; kind of paren to match the one we started at.
	      (when (integerp pos)
		(let ((beg (min pos oldpos)) (end (max pos oldpos)))
		  (when (/= (char-syntax (char-after beg)) ?\$)
		    (setq mismatch
			  (not (eq (char-before end)
				   ;; This can give nil.
				   (matching-paren (char-after beg)))))))))))
	;;
	;; Highlight the other end of the sexp, or unhighlight if none.
	(if (not pos)
	    (progn
	      ;; If not at a paren that has a match,
	      ;; turn off any previous paren highlighting.
	      (and show-paren-overlay (overlay-buffer show-paren-overlay)
		   (delete-overlay show-paren-overlay))
	      (and show-paren-overlay-1 (overlay-buffer show-paren-overlay-1)
		   (delete-overlay show-paren-overlay-1)))
	  ;;
	  ;; Use the correct face.
	  (if mismatch
	      (progn
		(if show-paren-ring-bell-on-mismatch
		    (beep))
		(setq face 'show-paren-mismatch-face))
	    (setq face 'show-paren-match-face))
	  ;;
	  ;; If matching backwards, highlight the closeparen
	  ;; before point as well as its matching open.
	  ;; If matching forward, and the openparen is unbalanced,
	  ;; highlight the paren at point to indicate misbalance.
	  ;; Otherwise, turn off any such highlighting.
	  (if (and (= dir 1) (integerp pos))
	      (when (and show-paren-overlay-1
			 (overlay-buffer show-paren-overlay-1))
		(delete-overlay show-paren-overlay-1))
	    (let ((from (if (= dir 1)
			    (point)
			  (forward-point -1)))
		  (to (if (= dir 1)
			  (forward-point 1)
			(point))))
	      (if show-paren-overlay-1
		  (move-overlay show-paren-overlay-1 from to (current-buffer))
		(setq show-paren-overlay-1 (make-overlay from to)))
	      ;; Always set the overlay face, since it varies.
	      (overlay-put show-paren-overlay-1 'priority show-paren-priority)
	      (overlay-put show-paren-overlay-1 'face face)))
	  ;;
	  ;; Turn on highlighting for the matching paren, if found.
	  ;; If it's an unmatched paren, turn off any such highlighting.
	  (unless (integerp pos)
	    (delete-overlay show-paren-overlay))
	  (let ((to (if (or (eq show-paren-style 'expression)
			    (and (eq show-paren-style 'mixed)
				 (not (pos-visible-in-window-p pos))))
			(point)
		      pos))
		(from (if (or (eq show-paren-style 'expression)
			      (and (eq show-paren-style 'mixed)
				   (not (pos-visible-in-window-p pos))))
			  pos
			(save-excursion
			  (goto-char pos)
			  (forward-point (- dir))))))
	    (if show-paren-overlay
		(move-overlay show-paren-overlay from to (current-buffer))
	      (setq show-paren-overlay (make-overlay from to))))
	  ;;
	  ;; Always set the overlay face, since it varies.
	  (overlay-put show-paren-overlay 'priority show-paren-priority)
	  (overlay-put show-paren-overlay 'face face)))
    ;; show-paren-mode is nil in this buffer.
    (and show-paren-overlay
	 (delete-overlay show-paren-overlay))
    (and show-paren-overlay-1
	 (delete-overlay show-paren-overlay-1))))

;;; From XEmacs' paren.el, with minimal changes (see "LilyPond"-lines)
;; Find the place to show, if there is one,
;; and show it until input arrives.
(defun LilyPond-paren-highlight ()
  "This highlights matching parentheses.

See the variables:
  paren-message-offscreen   use modeline when matching paren is offscreen?
  paren-ding-unmatched	    make noise when passing over mismatched parens?
  paren-mode		    'blink-paren, 'paren, or 'sexp
  blink-matching-paren-distance  maximum distance to search for parens.

and the following faces:
  paren-match, paren-mismatch, paren-blink-off"

  ;; I suppose I could check here to see if a keyboard macro is executing,
  ;; but I did a quick empirical check and couldn't tell that there was any
  ;; difference in performance

  (let ((oldpos (point))
	(pface nil)			; face for paren...nil kills the overlay
	(dir (and paren-mode
		  (not (input-pending-p))
		  (not executing-kbd-macro)
		  (cond ((eq (char-syntax (preceding-char)) ?\))
			 -1)
			((eq (char-syntax (following-char)) ?\()
			 1))))
	pos mismatch)

    (save-excursion
      (if (or (not dir)
	      (LilyPond-inside-string-or-comment-p)
	      (not (save-restriction
		     ;; Determine the range within which to look for a match.
		     (if blink-matching-paren-distance
			 (narrow-to-region
			  (max (point-min)
			       (- (point) blink-matching-paren-distance))
			  (min (point-max)
			       (+ (point) blink-matching-paren-distance))))

		     ;; Scan across one sexp within that range.
		     (condition-case nil
			 (setq pos (LilyPond-scan-sexps (point) dir))
		       ;; NOTE - if blink-matching-paren-distance is set,
		       ;; then we can have spurious unmatched parens.
		       (error (paren-maybe-ding)
			      nil)))))

	  ;; do nothing if we didn't find a matching paren...
	  nil

	;; See if the "matching" paren is the right kind of paren
	;; to match the one we started at.
	(let ((beg (min pos oldpos)) (end (max pos oldpos)))
	  (setq mismatch
		(and (/= (char-syntax (char-after beg)) ?\\)
		     (/= (char-syntax (char-after beg)) ?\$)
		     ;; XEmacs change
		     (matching-paren (char-after beg))
		     (/= (char-after (1- end))
			 (matching-paren (char-after beg)))))
	  (if (eq paren-mode 'sexp)
	      (setq paren-extent (make-extent beg end))))
	(and mismatch
	     (paren-maybe-ding))
 	(setq pface (if mismatch
			'paren-mismatch
		      'paren-match))
	(and (memq paren-mode '(blink-paren paren))
	     (setq paren-extent (make-extent (- pos dir) pos)))

	(if (and paren-message-offscreen
		 (eq dir -1)
                 (not (current-message))
		 (not (window-minibuffer-p (selected-window)))
		 (not (pos-visible-in-window-safe pos)))
            (paren-describe-match pos mismatch))
		 
	;; put the right face on the extent
	(cond (pface
	       (set-extent-face paren-extent pface) 
	       (set-extent-priority paren-extent 100) ; want this to be high
	       (and (eq paren-mode 'blink-paren)
		    (setq paren-blink-on-face pface
			  paren-n-blinks 0
			  paren-timeout-id
			  (and paren-blink-interval
			       (add-timeout paren-blink-interval
					    'paren-blink-timeout
					    nil
					    paren-blink-interval))))))
	))))

(if (not (string-match "XEmacs\\|Lucid" emacs-version))
;;; EMACS' Lilypond-show-paren-mode definition
(define-minor-mode LilyPond-show-paren-mode
  "Toggle Show Paren mode.
With prefix ARG, turn Show Paren mode on if and only if ARG is positive.
Returns the new status of Show Paren mode (non-nil means on).

When Show Paren mode is enabled, any matching parenthesis is highlighted
in `show-paren-style' after `show-paren-delay' seconds of Emacs idle time."
  :global t :group 'LilyPond-paren-showing
    ;; Turn off the usual paren-matching method
    ;; when this one is turned on.
    (if (local-variable-p 'LilyPond-show-paren-mode)
	(make-local-variable 'blink-matching-paren-on-screen)
      (kill-local-variable 'blink-matching-paren-on-screen))
    (setq blink-matching-paren-on-screen (not LilyPond-show-paren-mode))
    ;; Now enable or disable the mechanism.
    ;; First get rid of the old idle timer.
    (if show-paren-idle-timer
	(cancel-timer show-paren-idle-timer))
    (if (boundp 'LilyPond-show-paren-idle-timer)
	(cancel-timer LilyPond-show-paren-idle-timer))
    (setq LilyPond-show-paren-idle-timer nil)
    ;; If show-paren-mode is enabled in some buffer now,
    ;; set up a new timer.
    (when (memq t (mapcar (lambda (buffer)
			    (or (with-current-buffer buffer
				  show-paren-mode)
				(with-current-buffer buffer
				  LilyPond-show-paren-mode)))
			  (buffer-list)))
      (setq LilyPond-show-paren-idle-timer (run-with-idle-timer
					    show-paren-delay t
					    'LilyPond-show-paren-function)))
    (unless show-paren-mode
      (and show-paren-overlay
	   (eq (overlay-buffer show-paren-overlay) (current-buffer))
	   (delete-overlay show-paren-overlay))
      (and show-paren-overlay-1
	   (eq (overlay-buffer show-paren-overlay-1) (current-buffer))
	   (delete-overlay show-paren-overlay-1))))
;;; XEMACS' Lilypond-paren-set-mode definition
(defun LilyPond-paren-set-mode (arg &optional quiet)
  "Cycles through possible values for `paren-mode', force off with negative arg.
When called from lisp, a symbolic value for `paren-mode' can be passed directly.
See also `paren-mode' and `paren-highlight'."
  (interactive "P")
  ;; kill off the competition, er, uh, eliminate redundancy...
  (setq post-command-hook (delq 'show-paren-command-hook post-command-hook))
  (setq pre-command-hook (delq 'blink-paren-pre-command pre-command-hook))
  (setq post-command-hook (delq 'blink-paren-post-command post-command-hook))

  (let* ((paren-modes '(blink-paren paren sexp))
	 (paren-next-modes (cons nil (append paren-modes (list nil)))))
    (setq paren-mode (if (and (numberp arg) (< arg 0))
			 nil		; turn paren highlighting off
		       (cond ((and arg (symbolp arg)) arg)
			     ((and (numberp arg) (> arg 0))
			      (nth (1- arg) paren-modes))
			     ((numberp arg) nil)
			     (t (car (cdr (memq paren-mode
						paren-next-modes)))))
		       )))
  (cond (paren-mode
	 (add-hook 'post-command-hook 'LilyPond-paren-highlight)
	 (add-hook 'pre-command-hook 'paren-nuke-extent)
	 (setq blink-matching-paren nil))
	((not (local-variable-p 'paren-mode (current-buffer)))
	 (remove-hook 'post-command-hook 'LilyPond-paren-highlight)
	 (remove-hook 'pre-command-hook 'paren-nuke-extent)
	 (paren-nuke-extent)		; overkill
	 (setq blink-matching-paren t)
	 ))
  (or quiet (message "Paren mode is %s" (or paren-mode "OFF"))))
)
