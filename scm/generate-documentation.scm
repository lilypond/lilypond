
;;;; 
;
; This file generates documentation for the backend of lilypond.
;
;;;;


(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)
  ))))

;;; TODO

(define (wordwrap string)
  ""
  )
  
(define (self-evaluating? x)
  (or (number? x) (string? x) (procedure? x) (boolean? x))
  )


(define (htmlfy x)
  (let*
      ((x1 (regexp-substitute/global #f ">" x 'pre "&gt;" 'post))
       (x2 (regexp-substitute/global #f "<" x1 'pre "&lt;" 'post))
       )
    x2))

(define (scm->string val)
  (string-append
   (if (self-evaluating? val) "" "'")
   (htmlfy 
    (call-with-output-string (lambda (port) (display val port))))
  ))

(define (document-property prop desc)
  (let ((handle (assoc (car prop) desc)))
    (string-append
     "\n<li><code>" (symbol->string (car prop)) "</code> (" (type-name (cadr prop)) ") -- "
     (caddr prop)
     "<br>default value:  <code>"
     (if (pair? handle)
	 (scm->string (cdr handle))
	 "not set"
	 )
     "</code>\n"
  )
  ))

(define (document-interface interface elt-description)
  (let* ((name (car interface))
	 (desc (cadr interface))
	 (props (caddr interface))
	 (docs (map (lambda (x) (document-property x elt-description))
		    props))
	 )

    (string-append
     "<hr>"
     "<h2>Interface: " (symbol->string name) "</h2>\n"
     desc
     "<hr>\n<ul>"
     (apply string-append docs)
     "</ul>"
     )
    ))

;
; generate HTML, return filename.
;
(define (document-element description)
  (let* ((metah (assoc 'meta description))
	 (meta (if (pair? metah)
		   (cdr metah)
		   '((properties . ()) (name . "huh?"))
		   ))
	 
	 (name (cdr (assoc 'name meta)))
	 (ifaces (cdr (assoc 'interface-descriptions meta)))
	 (ifacedoc (map (lambda (x) (document-interface x description))
				(reverse ifaces)))
	 (outname  (string-append name ".html"))
	 (out (open-output-file outname))
	 )
    (display (string-append "Writing " outname " ... \n") (current-error-port))
    (display
     (string-append "<title>LilyPond Element " name " </title>"
		    "<h1>" name "</h1>"
		    (apply string-append ifacedoc))
     out)
    outname
    )
  )

(define (document-elements elts)
  (let* ((files (map (lambda (x) (document-element (cdr x)))
		    elts))
	(outname  (string-append "backend.html"))
	(out (open-output-file outname))
	(l (map (lambda (x) (string-append
			     "<li><a href=" x ">" x "</a>\n"))
		files))
	)

	(display
	 (string-append
	  "<title>LilyPond backend documentation</title>"
	  "<h1>LilyPond backend documentation</h1>"
	  "<ul>"
	  (apply string-append l)
	  "</ul>"
	)
	 out
	 )
   ))

; (display (document-interface stem-interface '()))
; (define b (cdr (assoc 'Beam all-element-descriptions)))
;(display b)

;(document-element  b)

(document-elements all-element-descriptions)

