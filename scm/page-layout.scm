;;; page-layout.scm -- page breaking and page layout
;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;          Han-Wen Nienhuys <hanwen@cs.uu.nl>

(use-modules (oop goops describe)
	     (oop goops))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TAGLINE
  (string-append "Engraved by LilyPond (version " (lilypond-version) ")"))

;; TODO: take <optimally-broken-page-node> iso. page-number
;; for all of these functions ?

(define-public (plain-header paper scopes page-number last?)
  "Standard header for a part: page number --outside--  and instrument--centered."

  (let* ((props (page-properties paper))
	 (pnum
	  (if (ly:output-def-lookup paper 'printpagenumber)
	      (markup #:bold (number->string page-number))
	      ""
	      ))
	 (instr (ly:modules-lookup scopes 'instrument))
	 
	 (line (list "" (if (markup? instr) instr "") pnum)))

    (if (even? page-number)
	(set! line (reverse line)))

    (if (< 1 page-number)
	(interpret-markup
	 paper props (make-fill-line-markup line))
	'())
    ))


;; TODO: add publisher ID on non-first page.
(define-public (plain-footer paper scopes page-number last?)
  "Standard footer. Empty, save for first (copyright) and last (tagline) page."
  
  (let*
      ((props (page-properties paper))
       (copyright (ly:modules-lookup scopes 'copyright))
       (tagline-var (ly:modules-lookup scopes 'tagline))
       (tagline (if (markup? tagline-var) tagline-var TAGLINE))
       (stencil #f))

    (if last?
	(set! stencil
	      (ly:stencil-combine-at-edge
	       stencil Y DOWN (interpret-markup paper props tagline)
	       0.0
	       )))

    (if (and (= 1 page-number)
	     (markup? copyright))

	(set! stencil
	      (ly:stencil-combine-at-edge
	       stencil Y DOWN (interpret-markup paper props copyright)
	       0.0
	       )))

    stencil))
  
(define (page-headfoot paper scopes number sym sepsym dir last?)
  "Create a stencil including separating space."
  (let*
      ((header-proc (ly:output-def-lookup paper sym))
       (sep (ly:output-def-lookup paper sepsym))
       (stencil (ly:make-stencil "" '(0 . 0) '(0 . 0)))
       (head-stencil
	(if (procedure? header-proc)
	    (header-proc paper scopes number last?)
	    #f)))

    (if (and (number? sep) (ly:stencil? head-stencil))
	(set! head-stencil
	      (ly:stencil-combine-at-edge
	       stencil Y  dir head-stencil
	       sep 0.0)))

    head-stencil))

(define-public (default-page-music-height paper scopes number last?)
  "Printable area for music and titles; matches default-page-make-stencil." 
  (let*
      ((h (- (ly:output-def-lookup paper 'vsize)
	     (ly:output-def-lookup paper 'top-margin)
	     (ly:output-def-lookup paper 'bottom-margin)))
       (head (page-headfoot paper scopes number 'make-header 'head-sep UP last?))
       (foot (page-headfoot paper scopes number 'make-footer 'foot-sep DOWN last?)))
    (- h (if (ly:stencil? head)
	     (interval-length (ly:stencil-extent head Y))
	     0)
       (if (ly:stencil? foot)
	   (interval-length (ly:stencil-extent foot Y))
	   0))
    ))


(define-public (default-page-make-stencil lines paper scopes number last? )
  "Construct a stencil representing the page from LINES.  "
  (let*
     ((top-margin  (ly:output-def-lookup paper 'top-margin))
      
      ;; TODO: naming vsize/hsize not analogous to TeX.
      
      (hsize (ly:output-def-lookup paper 'hsize))
      (left-margin (/ (- hsize
			 (ly:output-def-lookup paper 'linewidth)) 2))
      (vsize (ly:output-def-lookup paper 'vsize))
      (bottom-edge (- vsize
		      (ly:output-def-lookup paper 'bottom-margin)))
		     
      (head (page-headfoot paper scopes number 'make-header 'head-sep UP last?))
      (foot (page-headfoot paper scopes number 'make-footer 'foot-sep DOWN last?))
      (line-stencils (map ly:paper-system-stencil lines))
      (height-proc (ly:output-def-lookup paper 'page-music-height))
      (music-height (height-proc paper scopes number last?))
      (ragged (ly:output-def-lookup paper 'raggedbottom))
      (ragged-last   (ly:output-def-lookup paper 'raggedlastbottom))
      (ragged-bottom (or (eq? #t ragged)
			 (and last? (eq? #t ragged-last))))

      (spc-left (-  music-height
		   (apply + (map (lambda (x)
				   (interval-length (ly:stencil-extent x Y)))
			line-stencils))))
      (stretchable-lines (remove ly:paper-system-title? (cdr lines)))
      (stretch (if (or (null? stretchable-lines)
		       (> spc-left (/ music-height 2))
		       ragged-bottom)
		   0.0
		   (/ spc-left (length stretchable-lines))))

      (page-stencil (ly:make-stencil '()
		    (cons left-margin hsize)
		    (cons (- top-margin) 0)))
      (was-title #t))

    (set! page-stencil (ly:stencil-combine-at-edge
	  page-stencil Y DOWN head 0. 0.))

    (for-each
     (lambda (l)
       (set! page-stencil
	     (ly:stencil-combine-at-edge
	      page-stencil Y DOWN (ly:paper-system-stencil l)
	      (if was-title
		  0.0
		  stretch)
	      ))

       (set! was-title (ly:paper-system-title? l)))
     lines)

    (if (ly:stencil? foot)
	(set! page-stencil
	      (ly:stencil-add
	       page-stencil
	       (ly:stencil-translate
		foot
		(cons 0
		      (+ (- bottom-edge) (- (car (ly:stencil-extent foot Y)))))
		))))

    (ly:stencil-translate page-stencil (cons left-margin 0))
  ))
  



;;; optimal page breaking

;;; This is not optimal page breaking, this is optimal distribution of
;;; lines over pages; line breaks are a given.

; TODO:
;
; - density scoring
; - separate function for word-wrap style breaking?
; - raggedbottom? raggedlastbottom? 

(define-public (ly:optimal-page-breaks
		lines paper-book)
  "Return pages as a list starting with 1st page. Each page is a list
of lines. "

  (define (make-node prev lines page-num penalty)
    (make <optimally-broken-page-node>
      #:prev prev
      #:lines lines
      #:pageno page-num
      #:penalty penalty))

  (define MAXPENALTY 1e9)
  (define bookpaper (ly:paper-book-book-paper paper-book))
  (define scopes (ly:paper-book-scopes paper-book))
  (define (line-height line)
    (ly:paper-system-extent line Y))

  ;; FIXME: may need some tweaking: square, cubic
  (define (height-penalty available used)
    ;; FIXME, simplistic
    (let* ((left (- available used))
	   ;; scale-independent
	   (relative (abs (/ left available))))
      (if (negative? left)

	  ;; too full, penalise more
	  (* 10 (1+ relative) relative)
	  
	  ;; Convexity: two half-empty pages is better than 1 completely
	  ;; empty page
	  (* (1+ relative) relative))))

  (define (page-height page-number last?)
    (let
	((p (ly:output-def-lookup bookpaper 'page-music-height)))

      (if (procedure? p)
	  (p bookpaper scopes page-number last?)
	  10000)))

  
  (define (cumulative-height lines)
    (apply + (map line-height lines)))

  (define (get-path node done)
    "Follow NODE.PREV, and return as an ascending list of pages. DONE
is what have collected so far, and has ascending page numbers."
    (if (is-a? node <optimally-broken-page-node>)
	(get-path (node-prev node) (cons node done))
	done))

  (define (combine-penalties user page prev)
    (+ prev page user))

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
	   (user-penalty (ly:paper-system-break-penalty (car current-lines)))
	   (total-penalty (combine-penalties
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
    (ly:paper-system-number (car (node-lines node))))

  (let* ((best-break-node (walk-lines '() '() lines))
	 (break-nodes (get-path best-break-node '()))
	 )

    (if (ly:get-option 'verbose)
	(begin
	  (format (current-error-port) "breaks: ~S\n" (map line-number break-nodes))
	  (force-output (current-error-port))))

    
    ; create stencils.
    
    (map (lambda (node)
	   ((ly:output-def-lookup bookpaper 'page-make-stencil)
	    (node-lines node)
	    bookpaper
	    scopes
	    (node-page-number node)
	    (eq? node best-break-node)))
	 break-nodes)))


