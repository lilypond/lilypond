;;;; lilypond-what-beat.el --- Emacs support for checking beats and bar checks.
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2011--2022 Trevor Daniels <t.daniels@treda.co.uk>
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
;
; Features:
;
; -> Counts number of notes between last | and point. Adds durations of
; each note up, and returns result.
;
; -> Works well on notes and chords.
;
; -> Ignores most keywords, like \override
;
; -> Is aware of certain keywords which often contain parameters that
; look like notes, but should not be counted.
;  | a \key b \minor c    % b is not counted, but a and c are.
;
; -> Ignores Scheme expressions, which start with #
;
; -> Doesn't ignore the \times keyword. Intelligently handles triplets.
; 
;
; Caveats:
;
; -> Doesn't work on regions that aren't preceded by a |. This is because such
; notes are only delimited by a {, and what-beat can't distinguish a { that
; opens a set of notes from an internal { (say from a triplet)
;
; -> Doesn't work with << >>  expressions or nested {} expressions (unless
; {} is part of a keyword like \times)
;
; -> Keywords abutted against a note are not visible to what-beat, and 
; can therefore surreptitiosly sneak fake notes into what-beat.
; | c\glissando f       <- BAD:  the f gets counted, but shouldn't
; | c \glissando f      <- GOOD: the f gets ignored
;
; -> Does not look outside notes context. Derivation rules don't work:
; str = \notes { a8 b c d }
; \score { \notes { | e4 %{ gets counted }% \str %{gets ignored}%
;
; -> Does not handle repeats.
;
; -> Ignores \bar commands (and does not get confused by a | inside a \bar)
;

; Recognizes pitch & octave
(setq pitch-regex "\\([a-z]+[,']*\\|<[^>]*>\\)\\(=[,']*\\)?")
; Recognizes duration
(setq duration-regex "[ \t\n]*\\(\\(\\(128\\|6?4\\|3?2\\|16?\\|8\\)\\([.]*\\)\\)\\([ \t]*[*][ \t]*\\([0-9]+\\)\\(/\\([1-9][0-9]*\\)\\)?\\)?\\)")

; These keywords precede notes that should not be counted during beats
(setq Parm-Keywords '("key" "clef" "appoggiatura" "acciaccatura" "grace"
		      "override" "revert" "glissando"))


(defun extract-match (string match-num)
  (if (null (match-beginning match-num))
      nil
    (substring string (match-beginning match-num) (match-end match-num))))


(defun add-fractions (f1 f2)
  "Adds two fractions, both are (numerator denominator)"
  (setq result (list (+ (* (car f1) (cadr f2)) (* (car f2) (cadr f1)))
		     (* (cadr f1) (cadr f2))))
  (setq result (reduce-fraction result 2))
  (setq result (reduce-fraction result 3))
  (setq result (reduce-fraction result 5))
  (setq result (reduce-fraction result 7))
)


(defun reduce-fraction (f divisor)
  "Eliminates divisor from fraction if present"
  (while (and (= 0 (% (car result) divisor))
	      (= 0 (% (cadr result) divisor))
	      (< 1 (cadr result))
	      (< 0 (car result)))
    (setq result (list (/ (car result) divisor) (/ (cadr result) divisor))))
  result
)


(defun parse-duration (duration)
  "Returns a duration string parsed as '(numerator denominator)"
  (string-match duration-regex duration)
  (let ((result (list 1 (string-to-number (extract-match duration 2))))
	(dots (extract-match duration 4))
	(numerator (or (extract-match duration 6) "1"))
	(denominator (or (extract-match duration 8) "1")))
    (if (and (not (null dots)) (< 0 (string-width dots)))
	(dotimes (dummy (string-width dots))
	  (setq result (list (1+ (* 2 (car result))) (* 2 (cadr result))))))
    (list (* (string-to-number numerator) (car result))
	  (* (string-to-number denominator) (cadr result)))
))

(defun walk-note-duration ()
  "Returns duration of next note, moving point past note.
If point is not before a note, returns nil
If next note has no duration, returns t"
  (let ((have-pitch (looking-at pitch-regex)))
    (if have-pitch (goto-char (match-end 0)))
    (if (not (looking-at duration-regex))
	have-pitch
      (goto-char (match-end 0))
      (parse-duration (match-string 0)))))

; returns nil if not at a comment
(defun skip-comment ()
  (if (not (char-equal ?\% (following-char)))
      nil
    (progn
      (forward-char)
      (if (char-equal ?\{ (following-char))
	  (re-search-forward "}%" nil t)
	(progn
	  (skip-chars-forward "^\n")
	  (forward-char)))
      t
)))

; returns nil if not at a quotation
(defun skip-quotation ()
  (if (not (char-equal ?\" (following-char)))
      nil
    (progn
      (forward-char)
      (skip-chars-forward "^\"")
      (forward-char)
      t
)))

; returns nil if not at a sexp
(defun skip-sexp ()
  (interactive)
  (if (not (char-equal ?\# (following-char)))
      nil
    (progn
      (forward-char)
      (if (char-equal ?\' (following-char))
	  (forward-char))
      (if (not (char-equal ?\( (following-char)))
	  (skip-chars-forward "^ \t\n")
	(progn
	  (let ((paren 1))
	    (while (< 0 paren)
	      (forward-char)
	      (cond ((char-equal ?\( (following-char))
		     (setq paren (1+ paren)))
		    ((char-equal ?\) (following-char))
		     (setq paren (1- paren)))))
	    (forward-char)
	    t
))))))

(defun goto-note-begin ()
  (interactive)
  ; skip anything that is not ws. And skip any comments or quotations
  (while (or (< 0 (skip-chars-forward "^ \t\n~%#\""))
	     (skip-comment)
	     (skip-quotation)
	     (skip-sexp)))
  ; Now skip anything that isn't alphanum or \. And skip comments or quotations
  (while (or (< 0 (skip-chars-forward "^A-Za-z1-9<%}#=\""))
	     (skip-comment)
	     (skip-quotation)
	     (skip-sexp)))
  ; (skip-chars-forward "^\\") Why doesn't this work?!!
  (if (char-equal ?\\ (preceding-char))
      (backward-char))
)


(defun skip-good-keywords ()
  (if (looking-at "\\\\\\([a-z]*\\)")
      (progn
	(goto-char (match-end 0))
	(if (member (match-string 1) Parm-Keywords)
	    (progn
	      (if (looking-at "[ \t\n]*?\\([a-z0-9_]+\\|{[^}]*}\\|\"[^\"]*\"\\)")
		  (goto-char (match-end 0))
		(error "Improper regex match:")
		(error "Unknown text: %s")
))))))

(defun find-measure-start ()
  (let ((start (re-search-backward "\|" 0 t)))
    (if (null start)
	-1
      (if (looking-at "[^ \n\t]*\"")
	  (find-measure-start)
	(point)
))))

(defun get-beat ()
  (save-excursion
    (save-restriction
      (let* ((end (point))
	     (measure-start (find-measure-start))
	     (last-dur (or (re-search-backward duration-regex 0 t) -1))
	     (duration (if (= -1 last-dur) 0 (parse-duration (match-string 0))))
	     (result '(0 1)))		; 0 in fraction form
	(if (= measure-start -1)
	    (message "No | before point")
	  (goto-char (1+ measure-start))
	  (goto-note-begin)
	  (while (< (point) end)
	    (let ((new-duration (walk-note-duration)))
	      (if (null new-duration)
		  (if (not (looking-at
			    (concat "\\\\t\\(?:\\(imes\\)\\|uplet\\)[ \t]*\\([0-9]+\\)/\\([0-9]+\\)\\(?:[ \t\n]"
				    duration-regex "\\)?[ \t\n]*{")))
		      (skip-good-keywords)

					; handle \times/\tuplet specially
		    (let* ((times-p (match-beginning 1))
			   (numerator (string-to-number (match-string (if times-p 2 3))))
			   (denominator (string-to-number (match-string (if times-p 3 2)))))
		      (goto-char (match-end 0))
		      (goto-note-begin)
		      (while (and (not (looking-at "}"))
				  (< (point) end))
			(setq new-duration (walk-note-duration))
			(if (null new-duration)
			    (if (looking-at "\\\\[a-z]*[ \t]*[a-z]*")
				(goto-char (match-end 0))
			      (error "Unknown text: %S %s" result(buffer-substring (point) end))))
			(if (not (eq new-duration t))
			    (setq duration new-duration))
			(setq result (add-fractions result
						    (list (* numerator (car duration))
							  (* denominator (cadr duration)))))
			(goto-note-begin))
		      (if (< (point) end)
			  (forward-char 1)))) ; skip }

		(if (not (eq new-duration t))
		    (setq duration new-duration))
		(setq result (add-fractions result duration)))
	      (goto-note-begin)))

	  result)))))

(defun LilyPond-what-beat ()
  "Returns how much of a measure lies between last measaure '|' and point.
Recognizes chords, and triples."
  (interactive)
  (let ((beat (get-beat)))
    (message "Beat: %d/%d" (car beat) (cadr beat)))
)

(defun LilyPond-electric-bar ()
  "Indicate the number of beats in last measure when a | is inserted"
  (interactive)
  (self-insert-command 1)
  (save-excursion
    (save-restriction
      (backward-char)
      (LilyPond-what-beat)
      (forward-char)
)))


