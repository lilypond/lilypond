;;;; page-layout.scm -- page layout functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>

(define (ly:modules-lookup modules sym)
  (let ((v (module-variable (car modules) sym)))
    (if (and v (variable-bound? v) (variable-ref v))
	(variable-ref v)
	(if (module? (cdr modules)) (ly:modules-lookup (cdr modules) sym)))))

(define (page-properties paper)
  (list (append `((linewidth . ,(ly:paper-get-number
				 paper 'linewidth)))
		(ly:paper-lookup paper 'text-font-defaults))))

(define-public (book-title paper scopes)
  "Generate book title from header strings."
  
  (define (get sym)
    (let ((x (ly:modules-lookup scopes sym)))
      (if (and x (not (unspecified? x))) x "")))
  
  (let ((props (page-properties paper)))
    
    (interpret-markup
     paper props
     (markup
      #:column
      (#:override '(baseline-skip . 4)
      #:column
      (#:fill-line
       (#:normalsize (get 'dedication))
       #:fill-line
       (#:huge #:bigger #:bigger #:bigger #:bigger #:bold (get 'title))
       #:override '(baseline-skip . 3)
       #:column
       (#:fill-line
	(#:large #:bigger #:bigger #:bold (get 'subtitle))
	#:fill-line (#:bigger #:bigger #:bold (get 'subsubtitle)))
       #:override '(baseline-skip . 5)
       #:column ("")
       #:override '(baseline-skip . 2.5)
       #:column
       (#:fill-line
	(#:bigger (get 'poet) #:large #:bigger #:caps (get 'composer))
	#:fill-line (#:bigger (get 'texttranslator) #:bigger (get 'opus))
	#:fill-line
	(#:bigger (get 'meter) #:bigger (get 'arranger))
	""
	#:fill-line (#:large #:bigger (get 'instrument))
	" "
	#:fill-line (#:large #:bigger #:caps (get 'piece) ""))))))))

(define-public (user-title paper markup)
  "Generate book title from header markup."
  (if (markup? markup)
      (let ((props (page-properties paper))
	    (baseline-skip (chain-assoc-get 'baseline-skip props 2)) )
	(stack-lines DOWN 0 BASELINE-SKIP
		     (list (interpret-markup paper props markup))))))

(define-public (score-title paper scopes)
  "Generate score title from header strings."
  
  (define (get sym)
    (let ((x (ly:modules-lookup scopes sym)))
      (if (and x (not (unspecified? x))) x "")))
  
  (let ((props (page-properties paper)))
    
    (interpret-markup
     paper props
     (markup
      #:column
      (#:override '(baseline-skip . 4)
      #:column
      (#:fill-line
       ("" (get 'opus))
       #:fill-line (#:large #:bigger #:caps (get 'piece) "")))))))

(define-public (make-header paper page-number)
  (let ((props (page-properties paper) ))
    (interpret-markup paper props
		      (markup #:fill-line
			      ("" #:bold (number->string page-number))))))

(define-public (make-footer paper page-number)
  (let ((props (page-properties paper)))

    (interpret-markup paper props
		    (markup #:fill-line ("" (number->string page-number))))))


(define TAGLINE
  (string-append "Engraved by LilyPond (version " (lilypond-version) ")"))

(define-public (make-tagline paper scopes)
  (let* ((props (page-properties paper))
	 (tagline-var (ly:modules-lookup scopes 'tagline))
	 (tagline (if (markup? tagline-var) tagline-var TAGLINE)))

    (cond ((string? tagline)
	   (if (not (equal? tagline ""))
	       (interpret-markup paper props
				 (markup #:fill-line (tagline "")))))
	  ((markup? tagline) (interpret-markup paper props tagline)))))

(define-public (make-copyright paper scopes)
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
  (height #:init-value 0 #:accessor node-height #:init-keyword #:score))

(define INFINITY 1e9)

(define (line-number line)
  (if (null? line) 0
      (ly:paper-line-number line)))
  
(define (line-height line)
  (if (null? line) 0
      (ly:paper-line-height line)))
  
(define (node-line-number node)
  (if (null? node) 0
      (line-number (node-line node))))

(define (node-break-score node)
  (let ((line (node-line node)))
    (if (null? line) 0
	(ly:paper-line-break-score line))))

(define (make-node prev line page score)
  (make <break-node> #:prev prev #:line line #:page page #:score score))

(define MAX-CRAMP 0.05)

(define-public (ly:optimal-page-breaks lines book-height text-height
				       first-diff last-diff)

  ;; FIXME: may need some tweaking: square, cubic
  (define (height-score available used)
    (let* ((empty (- available used))
	   (norm-empty (* empty (/ 100 available))))
      (if (< norm-empty 0)
	  (if (> (* -1 (/ empty available)) MAX-CRAMP)
	      ;; cannot fill more than MAX-CRAMP
	      -1
	      ;; overfull page is still worse by a power
	      (* -1 norm-empty norm-empty norm-empty))
	   (* norm-empty norm-empty))))

  (define (page-height page-number page-count)
    (let ((h text-height))
      (if (= page-number 1)
	  (set! h (+ h first-diff)))
      (if (= page-number page-count)
	  ;;(> page-number (/ book-height text-height))
       (set! h (+ h last-diff)))
      h))

  (define (cumulative-height lines)
    (apply + (map line-height lines)))

  (define (get-path node)
    (if (null? node)
	'()
	(cons node (get-path (node-prev node)))))

  (define (add-scores . lst)
    (if (null? (filter (lambda (x) (> 0 x)) lst))
	(apply + lst)
	-1))

  (define (density-variance nodes)
    (define (sqr x) (* x x))
    (define (density node)
      (let ((p (page-height (node-page node) (node-page (car nodes))))
	    (h (node-height node)))
	(if (and p h) (* (- p h) (/ h 100)) 0)))
    (let* ((densities (map density nodes))
	   (mean (/ (apply + densities) (length densities)))
	   (diff (map (lambda (x) (- x mean)) densities))
	   (var (map sqr diff)))
      (if #f
	  (begin
	    (format (current-error-port) "\nDENSITIES")
	    (map describe nodes)
	    (format (current-error-port) "densities: ~S\n" densities)
	    (format (current-error-port) "mean: ~S\n" mean)
	    (format (current-error-port) "diff: ~S\n" diff)
	    (format (current-error-port) "density-var: ~S\n" var)))
      (apply + var)))

  (define (walk-paths best node lines nodes paths)
    (let* ((height (cumulative-height lines))
	   (next-page (+ (if (null? paths) 0 (node-page (car paths))) 1))
	   (page (page-height (node-page node) next-page)))
      (set! (node-height node) height)
      (let* ((break-score (node-break-score node))
	     (density-score (if (null? paths) 0
				(* 0 (density-variance
				      (get-path (car paths))))))
	     (page-score (height-score page height))
	     (this-score (add-scores page-score break-score density-score))
	     (path-score (if (null? paths) 0 (node-score (car paths))))
	     (score (add-scores path-score this-score)))
	    
	(if (and (>= score 0)
		 (not (null? lines))
		 (or (< score (node-score best))
		     (= (node-score best) -1)))
	    (begin
	      (set! (node-score best) score)
	      (set! (node-page best) next-page)
	      (set! (node-height best) height)
	      (set! (node-prev best) node)))

	(if (null? nodes)
	    best
	    (walk-paths best (car paths)
			(cons (node-line node) lines)
			(cdr nodes) (cdr paths))))))

  (define (walk-lines lines nodes paths)
    (if (null? (cdr lines))
	paths
	(let* ((prev (node-prev (car nodes)))
	       (this (make-node prev (car lines) 0 INFINITY))
	       (next (make-node this (cadr lines) 0 0)))
	  (let ((break (walk-paths this (car nodes) '() (cdr nodes) paths)))
	    (walk-lines (cdr lines) (cons next nodes) (cons break paths))))))
  
  (let* ((dummy (make-node '() '() 0 0))
	 (this (make-node dummy (car lines) 0 0))
	 (result (walk-lines lines (list this dummy) (list dummy)))
	 (path (get-path (car result)))
	 ;; CDR: junk dummy node
	 (breaks (cdr (reverse (map node-line-number path)))))

    (format (current-error-port) "ESTIMATE: ~S\n"
	    (/ book-height text-height))
    (format (current-error-port) "breaks: ~S\n" breaks)
    (force-output (current-error-port))
    (list->vector breaks)))
