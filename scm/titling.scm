;;;; titling.scm -- titling functions
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2004--2009 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;          Han-Wen Nienhuys <hanwen@xs4all.nl>

(define-public (layout-extract-page-properties layout)
  (list (append `((line-width . ,(ly:paper-get-number
				 layout 'line-width)))
		(ly:output-def-lookup layout 'text-font-defaults))))

;;;;;;;;;;;;;;;;;;

(define-public ((marked-up-headfoot what-odd what-even)
                layout scopes page-number is-last-bookpart is-bookpart-last-page)

  "Read variables WHAT-ODD, WHAT-EVEN from LAYOUT, and interpret them
as markup. The PROPS argument will include variables set in SCOPES and
page:is-bookpart-last-page, page:is-last-bookpart, page:page-number-string
and page:page-number" 

  (define (get sym)
    (ly:output-def-lookup layout sym))

  (define (interpret-in-page-env potential-markup)
    (if (markup? potential-markup)
	(let* ((alists (map ly:module->alist scopes))
	       (prefixed-alists
		(map (lambda (alist)
		       (map (lambda (entry)
			      (cons
			       (string->symbol
				(string-append
				 "header:"
				 (symbol->string (car entry))))
			       (cdr entry)))
			    alist))
		     alists))
	       (pgnum-alist
		(list
		 (cons 'header:tagline
		       (ly:modules-lookup scopes 'tagline
					  (ly:output-def-lookup layout 'tagline)))
		 (cons 'page:is-last-bookpart is-last-bookpart)
		 (cons 'page:is-bookpart-last-page is-bookpart-last-page)
		 (cons 'page:page-number-string
		       (number->string page-number))
		 (cons 'page:page-number page-number)))
	       (props (append
		       (list pgnum-alist)
		       prefixed-alists
		       (layout-extract-page-properties layout))))
	  (interpret-markup layout props potential-markup))

	empty-stencil))

  (interpret-in-page-env
   (if (and (even? page-number)
	    (markup? (get what-even)))
       (get what-even)
       (get what-odd))))

(define-public ((marked-up-title what) layout scopes)
  "Read variables WHAT from SCOPES, and interpret it as markup. The
PROPS argument will include variables set in SCOPES (prefixed with
`header:'
"
  
  (define (get sym)
    (let ((x (ly:modules-lookup scopes sym)))
      (if (markup? x) x #f)))

  (let* ((alists (map ly:module->alist scopes))
	 (prefixed-alist
	  (map (lambda (alist)
		 (map (lambda (entry)
			(cons
			 (string->symbol
			  (string-append
			   "header:"
			   (symbol->string (car entry))))
			 (cdr entry)))
		      alist))
	       alists))
	 (props (append prefixed-alist
			(layout-extract-page-properties layout)))

	 (markup (ly:output-def-lookup layout what)))

    (if (markup? markup)
	(interpret-markup layout props markup)
	(ly:make-stencil '() '(1 . -1) '(1 . -1)))))
