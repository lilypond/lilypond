
;;; engraver-doumentation-lib.scm -- Functions for engraver documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>


(define (engraver-makes-grob? name-symbol grav)
  (memq name-symbol (assoc 'grobs-created (ly-translator-description grav)))
  )

(define (engraver-accepts-music-type? name-symbol grav)
  (memq name-symbol (assoc 'events-accepted (ly-translator-description grav)))

  )

(define (engraver-accepts-music-types? types grav)
  (if (null? types)
      #f
      (or
       (engraver-accepts-music-type? (car types) grav)
       (engraver-accepts-music-types? (cdr types) grav)))
  )

(define (engraver-doc-string engraver)
  (let* (
	 (propsr (cdr (assoc 'properties-read (ly-translator-description engraver))))
	 (propsw (cdr (assoc 'properties-written (ly-translator-description engraver))))
	 (accepted  (cdr (assoc 'events-accepted (ly-translator-description engraver)))) 
	 (name (ly-translator-name engraver))
	 (name-sym (string->symbol name))
	 (desc (cdr (assoc 'description (ly-translator-description engraver))))
	 (grobs (engraver-grobs engraver))
	 )

    (string-append
     desc
     "\n\n"
     (if (pair? accepted)
	 (string-append
	  "Music types accepted:\n\n"
	  (human-listify
	   (map (lambda (x)
		  (string-append
		   "@ref{"
		  (symbol->string x)
		  "}")) accepted)
	   ))
	  "")
     "\n\n"
     (if (pair? propsr)
	 (string-append
	  "Properties (read)"
	  (description-list->texi
	   (map (lambda (x) (document-property x 'translation #f)) propsr)))
	 "")
     
     (if (null? propsw)
	 ""
	 (string-append
	 "Properties (write)" 
	  (description-list->texi
	   (map (lambda (x) (document-property x 'translation #f)) propsw))))
     (if  (null? grobs)
	  ""
	  (string-append
	   "This engraver creates the following grobs: \n "
	   (human-listify (map ref-ify (uniq-list (sort  grobs string<? ))))
	   ".")
	  )

     "\n\n"

     (let* ((paper-alist (My_lily_parser::paper_description))
	    (context-description-alist (map cdr paper-alist))
	    (contexts
	     (apply append
		    (map (lambda (x)
			   (let ((context (cdr (assoc 'type-name x)))
				 (consists (append
					    (list (cdr (assoc 'group-type x)))
					    (cdr (assoc 'consists x))
					    (cdr (assoc 'end-consists x)))))

			     (if (member name consists)
				 (list context)
				 '())))
			 context-description-alist))))
       (string-append
	name " is part of contexts: "
	(human-listify (map ref-ify (map context-name contexts))))))))




;; First level Engraver description
(define (engraver-doc grav)
  (make <texi-node>
    #:name (ly-translator-name grav)
    #:text (engraver-doc-string grav)
    ))

;; Second level, part of Context description

(define name->engraver-table (make-vector 61 '()))
(map
 (lambda (x)
   (hash-set! name->engraver-table (ly-translator-name x) x))
 (ly-get-all-translators))

(define (find-engraver-by-name name)
  (hash-ref name->engraver-table name #f))

(define (document-engraver-by-name name)
  (let*
      (
       (eg (find-engraver-by-name name ))
       )

    (if (eq? eg #f)
	(string-append "Engraver " name ", not documented.\n")
	(engraver-doc-string eg)
 	)
    ))

(define (document-property-operation op)
  (let
      ((tag (car op))
       (body (cdr op))
       (sym (cadr op))
       )

  (cond
   ((equal?  tag 'push)
    (string-append
     "@item "
     (if (null? (cddr body))
	 "Revert "
	 "Set "
	 )
     "grob-property "
     (symbol->string (cadr body))
     " in " (symbol->string sym)
     (if (not (null? (cddr body)))
	 (string-append " to " (scm->texi (cadr (cdr body))))
	 )
    "\n"
     )

    )
   ((equal? (object-property sym 'is-grob?) #t) "")
   ((equal? (car op) 'assign)
    (string-append
     "@item Set translator property "
     (symbol->string (car body))
     " to "
     (scm->texi (cadr body))
     "\n"
     )
     )
   )
  ))


(define (context-doc context-desc)
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
       (props (cdr (assoc 'property-ops context-desc)))
       (grobs  (context-grobs context-desc))
       (grob-refs (map (lambda (x) (ref-ify x)) grobs))
       )
    (make <texi-node>
      #:name name
      #:text
      (string-append 
       desc
       "\n\nThis context creates the following grobs: \n\n"
       (human-listify (uniq-list (sort grob-refs string<? )))
       "."
       (if (pair? props)
	   (string-append
	    "\n\nThis context sets the following properties:\n"
	    "@itemize @bullet\n"
	    (apply string-append (map document-property-operation props))
	    "@end itemize\n"
	    )
	   ""
	   )
       
       (if (null? accepts)
	   "\n\nThis context is a `bottom' context; it can not contain other contexts."
	   (string-append
	    "\n\nContext "
	    name " can contain \n"
	    (human-listify (map ref-ify (map context-name accepts)))))
       
       "\n\nThis context is built from the following engravers: "
       (apply string-append 
	      (map document-engraver-by-name consists)))
       )))

(define (engraver-grobs  grav)
  (let* (
	 (eg (if (string? grav)
		 (find-engraver-by-name grav)
		 grav))
	     
	     )

    (if (eq? eg #f)
	'()
	(map symbol->string (cdr (assoc 'grobs-created (ly-translator-description eg))))
 	)
  ))

(define (context-grobs context-desc)
  (let* (
	 (consists (append
		    (list (cdr (assoc 'group-type context-desc)))
		    (cdr (assoc 'consists context-desc))
		    (cdr (assoc 'end-consists  context-desc))
		    ))
	 (grobs  (apply append
		  (map engraver-grobs consists))
	 )
	 )
    grobs
    ))

(define (symbol<? l r)
  (string<? (symbol->string l) (symbol->string r)))

(define (all-contexts-doc)
  (let* (
	 (paper-alist
	  (sort (My_lily_parser::paper_description)
		(lambda (x y) (symbol<? (car x) (car y)))))
	 (names (sort (map symbol->string (map car paper-alist)) string<?))
	 (contexts (map cdr paper-alist))
	 )

    (make <texi-node>
      #:name "Contexts"
      #:desc "Complete descriptions of all contexts"
      #:children
      (map context-doc contexts)
      )
    ))


(define all-engravers-list  (ly-get-all-translators))
(set! all-engravers-list
      (sort all-engravers-list
	    (lambda (a b) (string<? (ly-translator-name a)
				    (ly-translator-name b)))))

(define (all-engravers-doc)
  (make <texi-node>
    #:name "Engravers"
    #:desc "All separate engravers"
    #:children
    (map engraver-doc all-engravers-list)))

(define (all-translation-properties-doc)
  
  (let*
      (
       (ps (sort (map symbol->string all-translation-properties) string<?))
       (sortedsyms (map string->symbol ps))
       (propdescs
	(map
	 (lambda (x) (document-property x 'translation #f))
	 sortedsyms))
       (texi (description-list->texi propdescs))
       )

    (make <texi-node>
      #:name "Translation properties"
      #:desc "All translation properties"
      #:text texi)
    ))


;(dump-node (all-contexts-doc) (current-output-port) 0 )

(define (translation-doc-node)
  (make <texi-node>
    #:name "Translation"
    #:desc "From music to layout"
    #:children
    (list
     (all-contexts-doc)
     (all-engravers-doc)
     (all-translation-properties-doc)
     )
  ))
