\bookpaper {
    
#(define-public (book-title paper scopes)
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

#(define-public (user-title paper markup)
  "Generate book title from header markup."
  (if (markup? markup)
      (let ((props (page-properties paper))
	    (baseline-skip (chain-assoc-get 'baseline-skip props 2)) )
	(stack-lines DOWN 0 BASELINE-SKIP
		     (list (interpret-markup paper props markup))))))

#(define-public (score-title paper scopes)
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


}
