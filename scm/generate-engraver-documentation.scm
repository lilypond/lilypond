(eval-string (ly-gulp-file "translator-description.scm"))

(define (document-trans-property prop-desc)
   (string-append "<li><code>" (car prop-desc) "</code>"
		  " (" (type-name (cadr prop-desc)) "):"
		  (caddr prop-desc)
		  )
   )

(define (document-engraver engraver-descr)
 
  (let* (
	 (props (car (cdddr engraver-descr)))
	 (name (car engraver-descr))
	 (desc (cadr engraver-descr))
	 (objs (caddr engraver-descr))
	 )
    (string-append
     "<hr><h2>" name "</h2><p>\n"
     desc
     "<p>"
     (if (null? props)
	 ""
	 (string-append
	  "<h3>Properties</h3>\n<ul>\n"
	  (apply string-append 
		 (map (lambda (x) (document-trans-property x)) props)
		 )
	  "</ul>\n")
	 )
     (if  (null? objs)
	  ""
	  (string-append
	   "This engraver creates \n "
	   (human-listify (map urlfy objs))
	   " objects")
	  )
     )
    )
  )


(define (document-engraver-by-name name)
  (let*
      (
       (eg (assoc (string->symbol name) engraver-description-alist))
       )

    (if (eq? eg #f)
	(string-append "Engraver " name ", not documented.\n")
	(document-engraver (cdr eg))
 	)
))

(define (context-doc-string context-desc)
  (let*
      (
       (name (cdr (assoc 'type-name context-desc)))
       (desc-handle (assoc (string->symbol name) context-description-alist))
       (desc (if (pair? desc-handle)  (cdr desc-handle) ""))
       
       (accepts (cdr (assoc 'accepts context-desc)))
       (consists (append
		  (list (cdr (assoc 'group-type context-desc)))
		  (cdr (assoc 'consists context-desc))
		  (cdr (assoc 'end-consists  context-desc))
		  ))
       )
    
    (string-append 
     "<h1>Context " name "</h1>\n"
     desc
     
     (if (null? accepts)
	 "This context is a `bottom' context; it can not contain other contexts."
	 (string-append
	  name " can contain \n"
	  (human-listify (map urlfy accepts))
	  ))
     "<p>This context is built from the following engravers\n"
     (apply string-append 
	    (map document-engraver-by-name consists)
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

      (writing-wip outname)
      (display
       (string-append "<title>LilyPond Context " name " </title>"
		      docstr)
       out
       )
     outname)
    )



(define (document-paper paper-alist)
;  (write paper-alist)
  (let*
      (
       (names (sort (map car paper-alist) string<?))
       (contexts (map cdr paper-alist))
       (files (map document-context contexts))
       (outname  (string-append "contexts.html"))
       (out (open-output-file outname))
       (l (map (lambda (x) (string-append
			    "<li>" (urlfy x)))
		       names))
       )

    (display
     (string-append
      "<title>LilyPond interpretation context documentation</title>"
      "<h1>LilyPond interpretation context documentation</h1>"
      "<ul>"
      (apply string-append l)
      "</ul>"
      )
     out
     )
   )
  )

(define (document-engraver-separately desc)
  (let* (
	 (name (car desc))
	 (outname (string-append name ".html"))
	 (out (open-output-file outname))
	 (doc (document-engraver desc))
	 )

    (writing-wip outname)
    (display doc out)
    outname
    ))

(define (document-all-engravers)
  (let*
      (
       (descs  (map cdr engraver-description-alist))
       (names  (map car engraver-description-alist))
       (fnames (map document-engraver-separately descs))
       (outname  "engravers.html")
       (out (open-output-file outname))
       )

    (display 
    (string-append
     "<title>All LilyPond engravers</title>"
     "<h1>All LilyPond engravers</h1>"
     "<ul>"
     (apply string-append
	    (map (lambda (x) (string-append "<li>" x))
		 (map urlfy names)))
     "</ul>"
     ) out)
  ))

; (display (document-engraver 'Stem_engraver))



;(display (human-listify '("a" "b" "c"))) 

(document-paper (My_lily_parser::paper_description))
(document-all-engravers)
