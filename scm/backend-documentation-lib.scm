;;; backend-documentation-lib.scm -- Functions for backend documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>


;;; This file generates documentation for the backend of lilypond.

;; alist of property descriptions


;;;;;; TODO: sort out symbol vs. string stuff.
;;;;;; TODO: use flatten write iso. string-append; might be possible to fold
;;;;;; in symbol->string integrally.

(define (interface-doc-string interface grob-description)
  (let* ((name (car interface))
	 (desc (cadr interface))
	 (props (sort (caddr interface) symbol<?))
	 (docfunc (lambda (pr)
		    (document-property
		     pr 'backend grob-description )))
	 (propdocs (map docfunc props)))

     desc
     (description-list->texi propdocs)))

;; First level Interface description
(define (interface-doc interface)
  (let ((name (symbol->string (car interface))))
    (make <texi-node>
      #:name name
      #:text (interface-doc-string (cdr interface) #f))))

;; First level grob description
(define (grob-doc description)
  (let*
      (
       (metah (assoc 'meta description))
       
       (meta (cdr metah))
       (name (cdr (assoc 'name meta)))
       (ifaces (map lookup-interface (cdr (assoc 'interfaces meta))))
       (ifacedoc (map (lambda (iface)
			(interface-doc-string iface description))
		      (reverse ifaces)))
       (engravers (filter-list
		   (lambda (x) (engraver-makes-grob? name x)) all-engravers-list))
       (namestr (symbol->string name))
       (engraver-names (map ly-translator-name engravers))
       )

    (make <texi-node>
      #:name namestr
      #:text
      (string-append
       namestr " grobs are created by: "
       (human-listify (map ref-ify
			   (map engraver-name engraver-names)))
       (apply string-append ifacedoc)
       ))
    ))

(define (engraver-makes-grob? name-symbol grav)
  (memq name-symbol (assoc 'grobs-created (ly-translator-description grav)))
  )

(define (all-grobs-doc)
  (make <texi-node>
    #:name "All Graphical objects"
    #:desc "Description and defaults for all Grobs"
    #:children
    (map (lambda (x) (grob-doc (cdr x)))  all-grob-descriptions)))

(define interface-description-alist
  (hash-fold
   (lambda (key val prior)
     (cons (cons key val)  prior)
     )
   '() (ly-all-grob-interfaces)))

(set! interface-description-alist (sort interface-description-alist alist<?))


;;;;;;;;;; check for dangling backend properties.
(define (mark-interface-properties entry)
  (map (lambda (x) (set-object-property! x  'iface-marked #t)) (caddr (cdr entry)))
  )

(map mark-interface-properties interface-description-alist)

(define (check-dangling-properties prop)
  (if (not (object-property prop 'iface-marked))
      (error  "\nDangling property: " prop))
  )

(map check-dangling-properties all-backend-properties)

;;;;;;;;;;;;;;;;

(define (lookup-interface name)
  (let*  (
	  (entry  (hashq-ref (ly-all-grob-interfaces) name #f))
	  )

    (if (equal? entry #f)
	(error "Unknown interface" name))
    
    entry
))

(define (all-interfaces-doc)
  (make <texi-node>
    #:name "Graphical Object Interfaces"
    #:desc "Building blocks of graphical objects"
    #:children
    (map interface-doc interface-description-alist)
    ))

(define (all-backend-properties-doc)
  (let*
      (
       (ps (sort (map symbol->string all-backend-properties) string<?))
       (descs (map (lambda (prop)
		     (document-property (string->symbol prop) 'backend #f))
		   ps))
       (texi (description-list->texi descs))
       )
    (make <texi-node>
      #:name "backend properties"
      #:desc "all the properties in use as grob properties"
      #:text texi)
  ))

;(dump-node (grob-doc (cdadr all-grob-descriptions))  (current-output-port) 0 )
(define (backend-doc-node)
  (make <texi-node>
    #:name "Backend"
    #:desc "Reference for the layout engine"
    #:children
    (list
     (all-grobs-doc)
     (all-interfaces-doc)
     (all-backend-properties-doc)
     )
  ))
