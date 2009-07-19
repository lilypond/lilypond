;;;; document-translation.scm -- Functions for engraver documentation
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;;
;;;; (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

(define (engraver-makes-grob? name-symbol grav)
  (memq name-symbol (assoc 'grobs-created (ly:translator-description grav))))

(define (engraver-accepts-music-type? name-symbol grav)
  (memq name-symbol (assoc 'events-accepted (ly:translator-description grav))))

(define (engraver-accepts-music-types? types grav)
  (if (null? types)
      #f
      (or
       (engraver-accepts-music-type? (car types) grav)
       (engraver-accepts-music-types? (cdr types) grav))))

(define (engraver-doc-string engraver in-which-contexts)
  (let* ((propsr (cdr (assoc 'properties-read (ly:translator-description engraver))))
	 (propsw (cdr (assoc 'properties-written (ly:translator-description engraver))))
	 (accepted  (cdr (assoc 'events-accepted (ly:translator-description engraver))))
	 (name-sym  (ly:translator-name engraver))
	 (name-str (symbol->string name-sym))
	 (desc (cdr (assoc 'description (ly:translator-description engraver))))
	 (grobs (engraver-grobs engraver)))

    (string-append
     desc
     "\n\n"
     (if (pair? accepted)
	 (string-append
	  "Music types accepted:\n\n"
	  (human-listify
	   (map ref-ify (sort (map symbol->string accepted) ly:string-ci<?))))
	 "")
     "\n\n"
     (if (pair? propsr)
	 (string-append
	  "Properties (read)"
	  (description-list->texi
	   (map (lambda (x) (property->texi 'translation x '()))
	        (sort propsr ly:symbol-ci<?))
	   #t))
	 "")

     (if (null? propsw)
	 ""
	 (string-append
	  "Properties (write)"
	  (description-list->texi
	   (map (lambda (x) (property->texi 'translation x '()))
	        (sort propsw ly:symbol-ci<?))
	   #t)))
     (if  (null? grobs)
	  ""
	  (string-append
	   "\n\nThis engraver creates the following layout object(s):\n\n"
	   (human-listify (map ref-ify (uniq-list (sort grobs ly:string-ci<?))))
	   "."))

     "\n\n"

     (if in-which-contexts
	 (let* ((layout-alist (ly:output-description $defaultlayout))
		(context-description-alist (map cdr layout-alist))
		(contexts
		 (apply append
			(map
			 (lambda (x)
			   (let* ((context (cdr (assoc 'context-name x)))
				  (group (assq-ref x 'group-type))
				  (consists (append
					     (if group
						 (list group)
						 '())
					     (cdr (assoc 'consists x)))))
			     (if (member name-sym consists)
				 (list context)
				 '())))
			 context-description-alist)))
		(context-list (human-listify (map ref-ify
						  (sort
						   (map symbol->string contexts)
						   ly:string-ci<?)))))
	   (string-append
	    "@code{" name-str "} "
	    (if (equal? context-list "none")
		"is not part of any context"
		(string-append
		 "is part of the following context(s): "
		 context-list))
	    "."))
	 ""))))

;; First level Engraver description
(define (engraver-doc grav)
  (make <texi-node>
    #:name (symbol->string (ly:translator-name grav))
    #:text (engraver-doc-string grav #t)))

;; Second level, part of Context description
(define name->engraver-table (make-vector 61 '()))
(map
 (lambda (x)
   (hash-set! name->engraver-table (ly:translator-name x) x))
 (ly:get-all-translators))

(define (find-engraver-by-name name)
  "NAME is a symbol."
  (hash-ref name->engraver-table name #f))

(define (document-engraver-by-name name)
  "NAME is a symbol."

  (let* ((eg (find-engraver-by-name name)))

    (cons (string-append "@code{" (ref-ify (symbol->string name)) "}")
	  (engraver-doc-string eg #f))))

(define (document-property-operation op)
  (let ((tag (car op))
	(context-sym (cadr op))
	(args (cddr op))
	)

    (cond
     ((equal?  tag 'push)
      (let*
	  ((value (car args))
	   (path (cdr args)))

      (string-append
       "@item Set "
       (format "grob-property @code{~a} "
	       (string-join (map symbol->string path) " "))
       (format "in @ref{~a} to ~a."
	       context-sym (scm->texi value))
       "\n")))
     ((equal? (object-property context-sym 'is-grob?) #t) "")
     ((equal? tag 'assign)
      (format "@item Set translator property @code{~a} to ~a.\n"
	      context-sym
	      (scm->texi (car args))))
     )))


(define (context-doc context-desc)
  (let* ((name-sym (cdr (assoc 'context-name context-desc)))
	 (name (symbol->string name-sym))
	 (aliases (map symbol->string (cdr (assoc 'aliases context-desc))))
	 (desc-handle (assoc 'description context-desc))
	 (desc (if (and  (pair? desc-handle) (string? (cdr desc-handle)))
		   (cdr desc-handle) "(not documented)"))
	
	 (accepts (cdr (assoc 'accepts context-desc)))
	 (consists (cdr (assoc 'consists context-desc)))
	 (props (cdr (assoc 'property-ops context-desc)))
	 (grobs  (context-grobs context-desc))
	 (grob-refs (map ref-ify (sort grobs ly:string-ci<?))))

    (make <texi-node>
      #:name name
      #:text
      (string-append
       desc
       (if (pair? aliases)
	   (string-append
	    "\n\nThis context also accepts commands for the following context(s):\n\n"
	    (human-listify (sort aliases ly:string-ci<?))
	    ".")
	   "")

       "\n\nThis context creates the following layout object(s):\n\n"
       (human-listify (uniq-list grob-refs))
       "."

       (if (and (pair? props) (not (null? props)))
	   (let ((str (apply string-append
		             (sort (map document-property-operation props)
			           ly:string-ci<?))))
	     (if (string-null? str)
		 ""
		 (string-append
		  "\n\nThis context sets the following properties:\n\n"
		  "@itemize @bullet\n"
		  str
		  "@end itemize\n")))
	   "")

       (if (null? accepts)
	   "\n\nThis context is a `bottom' context; it cannot contain other contexts."
	   (string-append
	    "\n\nContext "
	    name
	    " can contain\n"
	    (human-listify (map ref-ify (sort (map symbol->string accepts)
					      ly:string-ci<?)))
	    "."))

       (if (null? consists)
	   ""
	   (string-append
	    "\n\nThis context is built from the following engraver(s):"
	    (description-list->texi
	     (map document-engraver-by-name (sort consists ly:symbol-ci<?))
	     #t)))))))

(define (engraver-grobs grav)
  (let* ((eg (if (symbol? grav)
		 (find-engraver-by-name grav)
		 grav)))
    (if (eq? eg #f)
	'()
	(map symbol->string (cdr (assoc 'grobs-created (ly:translator-description eg)))))))

(define (context-grobs context-desc)
  (let* ((group (assq-ref context-desc 'group-type))
	 (consists (append
		    (if group
			(list group)
			'())
		    (cdr (assoc 'consists context-desc))))
	 (grobs  (apply append
			(map engraver-grobs consists))))
    grobs))

(define (all-contexts-doc)
  (let* ((layout-alist
	  (sort (ly:output-description $defaultlayout)
		(lambda (x y) (ly:symbol-ci<? (car x) (car y)))))
	 (names (sort (map symbol->string (map car layout-alist)) ly:string-ci<?))
	 (contexts (map cdr layout-alist)))

    (make <texi-node>
      #:name "Contexts"
      #:desc "Complete descriptions of all contexts."
      #:children
      (map context-doc contexts))))

(define all-engravers-list  (ly:get-all-translators))
(set! all-engravers-list
      (sort all-engravers-list
	    (lambda (a b) (ly:string-ci<? (symbol->string (ly:translator-name a))
				    (symbol->string (ly:translator-name b))))))

(define (all-engravers-doc)
  (make <texi-node>
    #:name "Engravers and Performers"
    #:desc "All separate engravers and performers."
    #:text "See @ruser{Modifying context plug-ins}."
    #:children
    (map engraver-doc all-engravers-list)))

(define (translation-properties-doc-string lst)
  (let* ((ps (sort (map symbol->string lst) ly:string-ci<?))
	 (sortedsyms (map string->symbol ps))
	 (propdescs
	  (map
	   (lambda (x) (property->texi 'translation  x '()))
	   sortedsyms))
	 (texi (description-list->texi propdescs #f)))
    texi))

(define (translation-doc-node)
  (make <texi-node>
    #:name "Translation"
    #:desc "From music to layout."
    #:children
    (list
     (all-contexts-doc)
     (all-engravers-doc)
     (make <texi-node>
       #:name "Tunable context properties"
       #:desc "All tunable context properties."
       #:text (translation-properties-doc-string
	       all-user-translation-properties))

     (make <texi-node>
       #:name "Internal context properties"
       #:desc "All internal context properties."
       #:text (translation-properties-doc-string
	       all-internal-translation-properties)))))
