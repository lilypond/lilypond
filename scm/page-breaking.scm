;;;; page-breaking.scm -- page breaking functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;          Han-Wen Nienhuys <hanwen@cs.uu.nl>

(use-modules (oop goops describe)
	     (oop goops))

;;; optimal page breaking

;;; This is not optimal page breaking, this is optimal distribution of
;;; lines over pages; line breaks are a given.

;;; TODO:
;;;    - user tweaking:
;;;       + \pagebreak, \nopagebreak
;;;       + #pages?
;;;    - short circut SCORE=-1 (dismiss path)
;;;    - density scoring


(define-class <optimally-broken-page-node> ()
  (prev #:init-value '() #:accessor node-prev #:init-keyword #:prev)
  (page #:init-value 0 #:accessor node-page-number #:init-keyword #:pageno)
  (penalty #:init-value 0 #:accessor node-penalty #:init-keyword #:penalty)
  (lines #:init-value 0 #:accessor node-lines #:init-keyword #:lines))

(define-method (display (node <optimally-broken-page-node>) port)
  (map (lambda (x) (display x port))
       (list
	"Page " (node-page-number node)
	" Lines: " (node-lines node)
	" Penalty " (node-penalty node)
	"\n")))

;; TODO: first-diff and last-diff are slightly arbitrary interface
;; For the future, we might want to invoke a function from PAPER-BOOK to
;; determine available height given
(define-public (ly:optimal-page-breaks
		lines paper-book text-height first-diff last-diff)
  "Return pages as a list starting with 1st page. Each page is a list
of lines.

TEXT-HEIGHT is the height of the printable area, FIRST-DIFF and
LAST-DIFF are decrements for the 1st and last page. PAPER-BOOK is
unused, at the moment."

  (define (make-node prev lines page-num penalty)
    (make <optimally-broken-page-node>
      #:prev prev
      #:lines lines
      #:pageno page-num
      #:penalty penalty))

  (define MAXPENALTY 1e9)

  (define (line-height line)
    (ly:paper-line-extent line Y))

  ;; FIXME: may need some tweaking: square, cubic
  (define (height-penalty available used)
    ;; FIXME, simplistic
    (let* ((left (- available used))
	   ;; scale-independent
	   (relative-empty (/ left available)))
      (if (negative? left)
	  ;; too full
	  MAXPENALTY
	  ;; Convexity: two half-empty pages is better than 1 completely
	  ;; empty page
	  (* (1+ relative-empty) relative-empty))))

  (define (page-height page-number last?)
    (let ((h text-height))
      (if (= page-number 1)
	  (set! h (+ h first-diff)))
      (if last?
	  (set! h (+ h last-diff)))
      h))

  (define (cumulative-height lines)
    (apply + (map line-height lines)))

  (define (get-path node done)
    "Follow NODE.PREV, and return as an ascending list of pages. DONE
is what have collected so far, and has ascending page numbers."
    (if (is-a? node <optimally-broken-page-node>)
	(get-path (node-prev node) (cons node done))
	done))

  (define (add-penalties . lst)
    (if (find negative? lst) ;; todo: rm support for this
	-1
	(apply + lst)))

  (define (walk-paths done-lines best-paths current-lines  last? current-best)
    "Return the best optimal-page-break-node that contains
CURRENT-LINES.  DONE-LINES.reversed ++ CURRENT-LINES is a consecutive
ascending range of lines, and BEST-PATHS contains the optimal breaks
corresponding to DONE-LINES.

CURRENT-BEST is the best result sofar, or #f."

    (let* ((this-page-num (if (null? best-paths)
			      1
			      (1+ (node-page-number (car best-paths)))))
	   (prev-penalty (if (null? best-paths)
			     0.0
			     (node-penalty (car best-paths))))
	   (page-height (page-height this-page-num last?))
	   (space-used (cumulative-height current-lines))
	   (this-page-penalty (height-penalty  page-height space-used))
	   (user-penalty (ly:paper-line-break-penalty (car current-lines)))
	   (total-penalty (add-penalties
			   user-penalty this-page-penalty prev-penalty))
	   (better? (or
		     (not current-best)
		     (< total-penalty (node-penalty current-best))))
	   (new-best (if better?
			 (make-node (if (null? best-paths)
					#f
					(car best-paths))
				    current-lines
				    this-page-num total-penalty)
			 current-best)))

      (if #f ;; debug
	  (display
	   (list
	    "user pen " user-penalty " prev-penalty "
	    prev-penalty "\n"
	    "better? " better? " total-penalty " total-penalty "\n"
	    "height " page-height " spc used: " space-used "\n"
	    "pen " this-page-penalty " lines: " current-lines "\n")))

      (if (and (pair? done-lines)
	       ;; if this page is too full, adding another line won't help
	       (< this-page-penalty MAXPENALTY))
	  (walk-paths (cdr done-lines) (cdr best-paths)
		      (cons (car done-lines) current-lines)
		      last? new-best)
	  new-best)))

  (define (walk-lines done best-paths todo)
    "Return the best page breaking as a single
<optimal-page-break-node> for optimally breaking TODO ++
DONE.reversed. BEST-PATHS is a list of break nodes corresponding to
DONE."
    (if (null? todo)
	(car best-paths)
	(let* ((this-line (car todo))
	       (last? (null? (cdr todo)))
	       (next (walk-paths done best-paths (list this-line) last? #f)))
	
	  (walk-lines (cons this-line done)
		      (cons next best-paths)
		      (cdr todo)))))

  (define (line-number node)
    (ly:paper-line-number (car (node-lines node))))

  (let* ((best-break-node (walk-lines '() '() lines))
	 (break-nodes (get-path best-break-node '()))
	 (break-lines (map node-lines break-nodes))
	 (break-numbers (map line-number break-nodes)))

    (if (ly:get-option 'verbose)
	(begin
	  (format (current-error-port) "breaks: ~S\n" break-numbers)
	  (force-output (current-error-port))))

    ;; TODO: if solution is bad return no breaks and revert to
    ;;       ragged bottom
    break-lines))

