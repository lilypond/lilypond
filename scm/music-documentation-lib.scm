

(define (music-property->texi sym)
  (let* ((name (symbol->string sym))
	(type (object-property sym 'music-type?))
	(typename (type-name type))
	(desc (object-property sym 'music-doc)))

    (cons (string-append "@code{" name "} "
		       "(" typename ")"
		       ": "
		       )
	  desc)))

(define (document-music name)
  (let* (
       (ps (sort (map symbol->string all-music-properties) string<?))
       (descs (map (lambda (prop)
		     (music-property->texi (string->symbol prop)))
		   ps))
       (texi (description-list->texi descs))
       )
    
    (string-append
     (node name)
     (texi-section 1 name #f)
     texi)
  ))
  
  
