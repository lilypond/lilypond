;;;; titling.scm -- titling functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;          Han-Wen Nienhuys <hanwen@cs.uu.nl>

(define-public (page-properties layout)
  (list (append `((linewidth . ,(ly:paper-get-number
				 layout 'linewidth)))
		(ly:output-def-lookup layout 'text-font-defaults))))

;;;;;;;;;;;;;;;;;;
					; titling.
(define-public (default-book-title layout scopes)
  "Generate book title from header strings."


  (define (get sym)
    (let ((x (ly:modules-lookup scopes sym)))
      (if (markup? x) x "")))
  (define (has sym)
    (markup?  (ly:modules-lookup scopes sym)))

  (let ((props (page-properties layout)))

    (interpret-markup
     layout props
     (make-override-markup
      '(baseline-skip . 4)
      (make-column-markup
       (append
	(if (has 'dedication)
	    (list (markup #:fill-line
			  (#:normalsize (get 'dedication))))
	    '())
	(if (has 'title)
	    (list
	     (markup (#:fill-line
		      (#:huge #:bigger #:bigger #:bigger #:bigger #:bold
			      (get 'title)))))
	    '())
	(if (or (has 'subtitle) (has 'subsubtitle))
	    (list
	     (make-override-markup
	      '(baseline-skip . 3)
	      (make-column-markup
	       (list
		(markup #:fill-line
			(#:large #:bigger #:bigger #:bold (get 'subtitle)))
		(markup #:fill-line (#:bigger #:bigger #:bold
					      (get 'subsubtitle)))
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
			      (#:bigger (get 'poet)
					#:large #:bigger #:caps
					(get 'composer))))
		'())
	    (if (or (has 'texttranslator) (has 'opus))
		(list
		 (markup
		  #:fill-line
		  (#:bigger (get 'texttranslator) #:bigger (get 'opus))))
		'())
	    (if (or (has 'meter) (has 'arranger))
		(list
		 (markup #:fill-line
			 (#:bigger (get 'meter) #:bigger (get 'arranger))))
		'())
	    (if (has 'instrument)
		(list
		 ""
		 (markup #:fill-line (#:large #:bigger (get 'instrument))))
		'())
;;; piece is done in the score-title
;;;	     (if (has 'piece)
;;;		 (list ""
;;;		       (markup #:fill-line (#:large #:bigger #:caps (get 'piece) "")))
;;;		 '())
	    ))))))))))


(define-public (default-user-title layout markup)
  "Generate book title from header markup."
  (if (markup? markup)
      (let ((props (page-properties layout))
	    (baseline-skip (chain-assoc-get 'baseline-skip props 2)) )
	(stack-lines DOWN 0 BASELINE-SKIP
		     (list (interpret-markup layout props markup))))))

(define-public (default-score-title layout scopes)
  "Generate score title from header strings."

  (define (get sym)
    (let ((x (ly:modules-lookup scopes sym)))
      (if (markup? x) x "")))

  (define (has sym)
    (markup? (ly:modules-lookup scopes sym)))

  (let ((props (page-properties layout)))
    (interpret-markup
     layout props
     (make-override-markup
      '(baseline-skip . 4)
      (make-column-markup
       (append
	(if (has 'opus)
	    ;; opus, again?
	    '()

	    ;; todo: figure out if and what should be here? 
	    ;;(list (markup #:fill-line ("" (get 'opus))))
	    '())
	(if (has 'piece)
	    (list
	     (markup #:fill-line (#:large #:bigger (get 'piece) "")))
	    '())))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
