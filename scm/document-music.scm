;;;; document-markup.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

(define (music-props-doc)
  (make <texi-node>
    #:name "Music properties"
    #:desc "All music properties, including descriptions."
    #:text
    (let* ((ps (sort (map symbol->string all-music-properties) string<?))
	   (descs (map (lambda (prop)
			 (property->texi 'music (string->symbol prop)))
		       ps))
	   (texi (description-list->texi descs #f)))
      texi)))

(define music-types->names (make-vector 61 '()))
(filter-map (lambda (entry)
	      (let* ((class (ly:camel-case->lisp-identifier (car entry)))
		     (classes (ly:make-event-class class)))
		(if classes
		    (map
		     (lambda (cl)
		       (hashq-set! music-types->names cl
				   (cons (car entry)
					 (hashq-ref music-types->names cl '()))))
		     classes)
		    #f)))
	
	    music-descriptions)

(define (strip-description x)
  (cons (symbol->string (car x))
	""))

(define (music-type-doc entry)
  (let* ((accept-list (human-listify
		       (map ref-ify
			    (map symbol->string
				 (map ly:translator-name
				      (filter
				       (lambda (x)
					 (engraver-accepts-music-type? (car entry) x))
				       all-engravers-list)))))))
    (make <texi-node>
      #:name (symbol->string (car entry))
      #:text
      (string-append
       "\nMusic event type @code{"
       (symbol->string (car entry))
       "} is in music objects of type "
       (human-listify
	(sort
	 (map (lambda (x) (ref-ify (symbol->string x)))
	      (cdr entry)) string<?))
       "."

       "\n\n"
       (if (equal? accept-list "none")
	   "Not accepted by any engraver or performer"
	   (string-append
	    "Accepted by: "
	    accept-list))
       "."))))

(define (music-types-doc)
  (make <texi-node>
    #:name "Music classes"
    #:children
    (map music-type-doc
	 (sort
	  (hash-table->alist music-types->names) alist<?))))

(define (music-doc-str obj)
  (let* ((namesym  (car obj))
	 (props (cdr obj))
	 (class (ly:camel-case->lisp-identifier namesym))
	 (classes (ly:make-event-class class))
	 (accept-list (if classes
			  (human-listify
			   (map ref-ify
				(map symbol->string
				     (map ly:translator-name
					  (filter
					   (lambda (x)
					     (engraver-accepts-music-types? classes x))
					   all-engravers-list)))))
			  ""))
	 (event-texi (if classes
			 (string-append
			  "\n\nEvent classes:\n"
			  (human-listify
			   (map ref-ify (map symbol->string classes)))
			  "."

			  "\n\n"
			  (if (equal? accept-list "none")
			      "Not accepted by any engraver or performer"
			      (string-append
			       "Accepted by: "
			       accept-list))
			  ".")
			 "")))

    (string-append
     (object-property namesym 'music-description)
     event-texi
     "\n\nProperties:\n"
     (description-list->texi
      (map
       (lambda (x) (property->texi 'music x props))
       (map car props))
      #t))))

(define (music-object-doc obj)
  (make <texi-node>
    #:name (symbol->string (car obj))
    #:text (music-doc-str obj)))

(define (music-expressions-doc)
  (make <texi-node>
    #:name "Music expressions"
    #:desc "Objects that represent music."
    #:children
    (map music-object-doc music-descriptions)))

(define (music-doc-node)
  (make <texi-node>
    #:name "Music definitions"
    #:desc "Definition of the input data structures."
    #:children
    (list
     (music-expressions-doc)
     (music-types-doc)
     (music-props-doc))))
