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
			      ;; FIXME: font not found
			      ;; ("" #:bold (number->string page-number))))))
			      ("" (number->string page-number))))))

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


