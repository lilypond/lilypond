
(define (music-props-doc)
  (make <texi-node>
    #:name "Music properties"
    #:desc "All music properties, including descriptions"
    #:text
  (let* (
	 (ps (sort (map symbol->string all-music-properties) string<?))
	 (descs (map (lambda (prop)
		       (property->texi 'music (string->symbol prop)))
		     ps))
	 (texi (description-list->texi descs))
	 )
    texi)
  ))

(define music-types->names (make-vector 61 '()))
(map (lambda (entry)
       (let*
	   (
	    (types (cdr (assoc 'types (cdr entry) )))
	    )
	 (map (lambda (type)
		(hashq-set! music-types->names type
			    (cons (car entry)
				  (hashq-ref music-types->names type '())))
			    
		) types)
	 
	 ))
  music-descriptions)


(define (hash-table->alist t)
  "Convert table t to list"
  (apply append
	 (vector->list t)
  ))

(define (strip-description x)
    (cons (symbol->string (car x))
	  ""))

(define (music-type-doc entry)
  (make <texi-node>
    #:name  (symbol->string (car entry))
    #:text 
    (string-append
     "\nMusic event type @code{"
     (symbol->string (car entry))
     "} is in Music objects of type "
     (human-listify
      (sort
       (map (lambda (x) (ref-ify (symbol->string x)))
	     (cdr entry)) string<?))

     "\n\nAccepted by: "
     (human-listify
      (map ref-ify
      (map ly:translator-name
	   (filter-list
	    (lambda (x) (engraver-accepts-music-type? (car entry) x)) all-engravers-list))))
     "\n\n"
     )))

(define (music-types-doc)
  (make <texi-node>
    #:name "Music classes"
    #:children 
    (map music-type-doc
	 (sort
	  (hash-table->alist music-types->names) alist<?))
    ))

(define (music-doc-str obj)
  (let*
      (
       (namesym  (car obj))
       (props (cdr obj))
       (types (cdr (assoc  'types props)))
       )
    
    (string-append
     (object-property namesym 'music-description)
     "\n\nMusic types:\n"
     (human-listify (map ref-ify (map symbol->string types)))
     "\n\n"
     "\n\nAccepted by: "
     (human-listify
      (map ref-ify
      (map ly:translator-name
	   (filter-list
	    (lambda (x) (engraver-accepts-music-types? types x)) all-engravers-list))))
     "\n\nProperties: \n"
     (description-list->texi
      (map
       (lambda (x) (document-property x 'music props))
       (map car props)))
     
     )
    ))

(define (music-object-doc obj)
  (make <texi-node>
    #:name (symbol->string (car obj))
    #:text (music-doc-str obj)
    ))

(define (music-expressions-doc)
  (make <texi-node>
    #:name "Music expressions"
    #:desc "Objects that represent music."
    #:children
     (map music-object-doc music-descriptions)
  ))
  
(define (music-doc-node)
  (make <texi-node>
    #:name "Music definitions"
    #:desc "Definition of the Input data structures"
    #:children
    (list
     (music-expressions-doc)
     (music-types-doc)
     (music-props-doc))
    ))

  
  

