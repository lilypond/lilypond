;;;; page-layout.scm -- page layout functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>


(define-public (page-properties paper)
  (list (append `((linewidth . ,(ly:paper-get-number
				 paper 'linewidth)))
		(ly:output-def-lookup paper 'text-font-defaults))))

(define-public (plain-header paper page-number)
  (let ((props (page-properties paper) ))
    (interpret-markup paper props
		      (markup #:fill-line
			      ("" #:bold (number->string page-number))))))

(define-public (plain-footer paper page-number)
  (let ((props (page-properties paper)))

    (interpret-markup paper props
		      (markup #:fill-line ("" (number->string page-number))))))


(define TAGLINE
  (string-append "Engraved by LilyPond (version " (lilypond-version) ")"))

(define-public (TAGLINE-or-tagline-from-header paper scopes)
  (let* ((props (page-properties paper))
	 (tagline-var (ly:modules-lookup scopes 'tagline))
	 (tagline (if (markup? tagline-var) tagline-var TAGLINE)))

    (cond ((string? tagline)
	   (if (not (equal? tagline ""))
	       (interpret-markup paper props
				 (markup #:fill-line (tagline "")))))
	  ((markup? tagline) (interpret-markup paper props tagline)))))

(define-public (copyright-from-header paper scopes)
  (let ((props (page-properties paper))
	(copyright (ly:modules-lookup scopes 'copyright)))
    
    (cond ((string? copyright)
	   (if (not (equal? copyright ""))
	       (interpret-markup paper props
				 (markup #:fill-line (copyright "")))))
	  ((markup? copyright) (interpret-markup paper props copyright)))))


;;; optimal page breaking

;;; This is not optimal page breaking, this is optimal distribution of
;;; lines over pages; line breaks are a given.

;;; TODO:
;;;    - user tweaking:
;;;       + \pagebreak, \nopagebreak
;;;       + #pages?
;;;    - short circut SCORE=-1 (dismiss path)
;;;    - density scoring


(use-modules (oop goops describe))

(define-class <break-node> ()
  (prev #:init-value '() #:accessor node-prev #:init-keyword #:prev)
  (line #:init-value 'barf #:accessor node-line #:init-keyword #:line)
  (page #:init-value 0 #:accessor node-page #:init-keyword #:page)
  (score #:init-value 0 #:accessor node-score #:init-keyword #:score)
  (height #:init-value 0 #:accessor node-height #:init-keyword #:height))

(define INFINITY 1e9)

(define (robust-paper-line-number line)
  (if (null? line) 0
      (ly:paper-line-number line)))
  
(define (robust-line-height line)
  (if (null? line) 0
      (ly:paper-line-extent line Y)))
  
(define (robust-line-number node)
  (if (null? node) 0
      (robust-paper-line-number (node-line node))))

(define (robust-break-score node)
  (let ((line (node-line node)))
    (if (null? line) 0
	(ly:paper-line-break-score line))))

(define (make-node prev line page score . height)
  (make <break-node> #:prev prev #:line line #:page page #:score score
	#:height (if (null? height) 0 (car height))))

;; max density %
(define MAX-CRAMP 0.05)

(define-public (ly:optimal-page-breaks lines
				       paper-book
				       text-height
				       first-diff last-diff)
  "DOCME"
  ;; FIXME: may need some tweaking: square, cubic
  (define (height-score available used)
    (let* ((empty (- available used))
	   (norm-empty (* empty (/ 100 available))))
      (if (< norm-empty 0)
	  (if (> (* -1 (/ empty available)) MAX-CRAMP)
	      ;; cannot fill more than MAX-CRAMP
	      -1
	      ;; overfull page is still worse by a power
	      ;; -- which means it never happens
	      ;; let's try a factor 2
	      ;;(* -1 norm-empty norm-empty norm-empty))
	      (* 2 norm-empty norm-empty))
	  (* norm-empty norm-empty))))

  (define (page-height page-number page-count)
    (let ((h text-height))
      (if (= page-number 1)
	  (set! h (+ h first-diff)))
      (if (= page-number page-count)
       (set! h (+ h last-diff)))
      h))

  (define (cumulative-height lines)
    (apply + (map robust-line-height lines)))

  (define (get-path node)
    (if (null? node) '() (cons node (get-path (node-prev node)))))

  (define (add-scores . lst)
    (if (null? (filter (lambda (x) (> 0 x)) lst)) (apply + lst) -1))

  (define (density-variance nodes)
    (define (sqr x) (* x x))
    (define (density node)
      (let ((p (page-height (node-page node) (node-page (car nodes))))
	    (h (node-height node)))
	(if (and p h) (/ h p) 0)))
    
    (let* ((height-nodes (reverse
			  ;; reverse makes for handier debugging
			  (filter (lambda (x) (> (node-height x) 0)) nodes)))
	   (densities (map density height-nodes))
	   (p-heights (map (lambda (x) (page-height (node-page x)
						    (node-page (car nodes))))
			   height-nodes))
	   (heights (map node-height height-nodes))
	   (mean (/ (apply + densities) (length densities)))
	   (diff (map (lambda (x) (- x mean)) densities))
	   (var (map sqr (map (lambda (x) (* (car p-heights) x)) diff))))
      (apply + var)))

  (define (walk-paths best node lines nodes paths)
    (let* ((height (cumulative-height lines))
	   (next-page (+ (if (null? paths) 0 (node-page (car paths))) 1))
	   (page (page-height (node-page node) next-page))
	   (hh (make-node '() (node-line node) 0 0 height))
	   (break-score (robust-break-score node))
	   (density-score (if (null? paths) 0
			      ;; TODO: find out why we need density
			      ;;       use other height-score parameters?
			      ;; See: input/test/page-breaks.ly
			      (* 1 (density-variance
				    (cons hh (get-path (car paths)))))))
	   (page-score (height-score page height))
	   (this-score (add-scores page-score break-score density-score))
	   (path-score (if (null? paths) 0 (node-score (car paths))))
	   (score (add-scores path-score this-score)))

      (if (and (>= score 0)
	       (or (<= score (node-score best))
		   (= (node-score best) -1)))
	  (begin
	    (set! (node-score best) score)
	    (set! (node-page best) next-page)
	    (set! (node-height best) height)
	    (set! (node-prev best) (car paths))))

      (if (or (null? nodes)
	      ;; short circuit
	      (and (= path-score -1)
		   (> (- (/ height page) 1) MAX-CRAMP)))
	  best
	  (walk-paths best (car nodes)
		      (cons (node-line (car paths)) lines)
		      (cdr nodes) (cdr paths)))))

  (define (walk-lines lines nodes paths)
    (if (null? (cdr lines))
	paths
	(let* ((prev (node-prev (car nodes)))
	       (this (make-node prev (car lines) 0 INFINITY))
	       (next (make-node this (cadr lines) 0 0))
	       (best (walk-paths this prev (list (node-line (car nodes)))
				 (cddr nodes) paths)))
	  (walk-lines (cdr lines) (cons next nodes) (cons best paths)))))
  
  (let* ((dummy (make-node '() '() 0 0))
	 (this (make-node dummy (car lines) 0 0))
	 (result (walk-lines lines (list this dummy) (list dummy)))
	 (path (get-path (car result)))
	 ;; CDR: junk dummy node
	 (breaks (cdr (reverse (map robust-line-number path)))))

    (if (ly:get-option 'verbose)
	(begin
	  (format (current-error-port) "breaks: ~S\n" breaks)
	  (force-output (current-error-port))))
    
    ;; TODO: if solution is bad return no breaks and revert to
    ;;       ragged bottom
    (list->vector breaks)))



;;;;;;;;;;;;;;;;;;
; titling.
(define-public (default-book-title paper scopes)
  "Generate book title from header strings."

  
  (define (get sym)
    (let ((x (ly:modules-lookup scopes sym)))
      (if (markup? x) x "")))
  (define (has sym)
    (markup?  (ly:modules-lookup scopes sym)))
  
  (let ((props (page-properties paper)))
    
    (interpret-markup
     paper props
     (make-override-markup
       '(baseline-skip . 4)
       (make-column-markup
	(append
	 (if (has 'dedication)
	     (list (markup #:fill-line
		     (#:normalsize (get 'dedication))))
	     '())
	 
	 (if (has 'title)
	    (list (markup (#:fill-line
			   (#:huge #:bigger #:bigger #:bigger #:bigger #:bold (get 'title)))))
	    '())

	 (if (or (has 'subtitle) (has 'subsubtitle))
	     (list
	      (make-override-markup
	       '(baseline-skip . 3)
	      (make-column-markup
	       (list
	       (markup #:fill-line
		       (#:large #:bigger #:bigger #:bold (get 'subtitle)))
	       (markup #:fill-line (#:bigger #:bigger #:bold (get 'subsubtitle)))
	       (markup #:override '(baseline-skip . 5)
		       #:column ("")))

	       ))
	     )
	     '())
	 
	 (list
	  (make-override-markup
	  '(baseline-skip . 2.5)
	  (make-column-markup
	    (append
	     (if (or (has 'poet) (has 'composer))
		(list (markup #:fill-line
			      (#:bigger (get 'poet) #:large #:bigger #:caps (get 'composer))))
		'())
	     (if (or (has 'texttranslator) (has 'opus))
		 (list
		  (markup 
		   #:fill-line (#:bigger (get 'texttranslator) #:bigger (get 'opus))))
		 '())
	     (if (or (has 'meter) (has 'arranger))
		 (list
		  (markup #:fill-line
			  (#:bigger (get 'meter) #:bigger (get 'arranger))))
		 '())

	     (if (has 'instrument)
		 (list ""
		       (markup #:fill-line (#:large #:bigger (get 'instrument))))
		 '())
	     (if (has 'piece)
		 (list ""
		       (markup #:fill-line (#:large #:bigger #:caps (get 'piece) "")))
		 '())
	     )))))))
     )))
	     
  
(define-public (default-user-title paper markup)
  "Generate book title from header markup."
  (if (markup? markup)
      (let ((props (page-properties paper))
	    (baseline-skip (chain-assoc-get 'baseline-skip props 2)) )
	(stack-lines DOWN 0 BASELINE-SKIP
		     (list (interpret-markup paper props markup))))))

(define-public (default-score-title paper scopes)
  "Generate score title from header strings."
  
  (define (get sym)
    (let ((x (ly:modules-lookup scopes sym)))
      (if (markup? x) x "")))
  
  (define (has sym)
    (markup? (ly:modules-lookup scopes sym)))
  
  (let ((props (page-properties paper)))
    
    (interpret-markup
     paper props
      (make-override-markup
       '(baseline-skip . 4)
       (make-column-markup
	(append
	 (if (has 'opus)
	     (list (markup #:fill-line ("" (get 'opus))))
	     '())
	 (if (has 'piece)
	     (list (markup #:fill-line (#:large #:bigger #:caps (get 'piece) "")))
	     '()))
	
	)))))
