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

	     ;; piece is done in the score-title  
;	     (if (has 'piece)
;		 (list ""
;		       (markup #:fill-line (#:large #:bigger #:caps (get 'piece) "")))
;		 '())
	     
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;NEW;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

