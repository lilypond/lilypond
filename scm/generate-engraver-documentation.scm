(eval-string (ly-gulp-file "translator-description.scm"))

(define (document-trans-property prop-desc)
   (string-append "<li><code>" (car prop-desc) "</code>"
		  " (" (type-name (cadr prop-desc)) "):"
		  (caddr prop-desc)
		  )
   )

(define (document-engraver engraver-name)
  
  (let*
      (
       (eg (assoc (string->symbol engraver-name) engraver-description-alist))
       (engraver-descr (if (eq? eg #f) '() (cdr eg)))
       )

    
    (if (eq? eg #f)
	(string-append "<hr>Engraver " engraver-name ", not documented.\n")
	(string-append
	 "<hr><h2><code>" (car engraver-descr) "</code></h2>\n"
	 "<h3>Description</h3>"
	 (cadr engraver-descr)
	 "<p>This engraver creates the following elements:\n "
	 (human-listify (map urlfy (caddr engraver-descr)))
	 "<ul>\n"
	 (apply string-append 
		(map (lambda (x) (document-trans-property x))
		     (car (cdddr engraver-descr)))
		)
	 "</ul>\n"	 
	 )
	
	)
    )
  )

(define (urlfy x)
  (string-append "<a href=" x ".html>" x "</a>"))

(define (human-listify l)
  (cond
   ((null? l) "none")
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (string-append (car l) " and " (cadr l)))
   (else (string-append (car l) ", " (human-listify (cdr l))))
   ))




(define (context-doc-string context-desc)
  (let*
      (
       (nm (cdr (assoc 'type-name context-desc)))
       (accepts (cdr (assoc 'accepts context-desc)))
       (consists (append
		  (cdr (assoc 'consists context-desc))
		  (cdr (assoc 'end-consists  context-desc))
		  ))
       )
    
    (string-append 
     "<h1>" nm "</h1>\n"
     "accepts:\n"
     (human-listify (map urlfy accepts))
     "<hr>\n"
     (apply string-append 
	    (map document-engraver consists)
	    )
     )
    )
  )

;; FIXME element ChordNames overwrites context ChordNames.
(define (document-context context-desc)
    (let*
	(
	 (name (cdr (assoc 'type-name context-desc)))
	 (docstr (context-doc-string context-desc))
	 (outname (string-append name ".html"))
	 (out (open-output-file outname))
	 )

      (display (string-append "Writing " outname " ... \n") (current-error-port))
      (display
       (string-append "<title>LilyPond Context " name " </title>"
		      docstr)
       out
       )
     outname)
    )



(define (document-paper paper-alist)
  (let*
      (
       (ufiles (map (lambda (x) (document-context  x )) paper-alist))
       (files (sort ufiles string<?))
       (outname  (string-append "translation.html"))
       (out (open-output-file outname))
       (l (map (lambda (x) (string-append
			    "<li><a href=" x ">" x "</a>\n"))
	       files))
       )
    (write files)
    (display
     (string-append
      "<title>LilyPond music translation documentation</title>"
      "<h1>LilyPond music translation documentation</h1>"
      "<ul>"
      (apply string-append l)
      "</ul>"
      )
     out
     )
   )
  )

; (display (document-engraver 'Stem_engraver))

(document-paper (My_lily_parser::paper_description))

;(display (human-listify '("a" "b" "c"))) 
