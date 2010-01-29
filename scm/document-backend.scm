;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;; Jan Nieuwenhuizen <janneke@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(define (sort-grob-properties x)
  ;; force 'meta to the end of each prop-list
  (let ((meta (assoc 'meta x)))
    (append (sort (assoc-remove! x 'meta) ly:alist-ci<?)
            (list meta))))

;; properly sort all grobs, properties, and interfaces
;; within the all-grob-descriptions alist
(map
  (lambda (x)
    (let* ((props      (assoc-ref all-grob-descriptions (car x)))
           (meta       (assoc-ref props 'meta))
           (interfaces (assoc-ref meta 'interfaces)))
      (set! all-grob-descriptions
        (sort (assoc-set! all-grob-descriptions (car x)
               (sort-grob-properties
                (assoc-set! props 'meta
                 (assoc-set! meta 'interfaces
                  (sort interfaces ly:symbol-ci<?)))))
              ly:alist-ci<?))))
  all-grob-descriptions)

(define (interface-doc-string interface grob-description)
  (let* ((name (car interface))
	 (desc (cadr interface))
	 (props (caddr interface))
	 (docfunc (lambda (pr)
		    (property->texi
		     'backend pr grob-description)))
	 (iprops (filter (lambda (x) (object-property x 'backend-internal))
			 props))
	 (uprops (filter
		  (lambda (x) (not (object-property x 'backend-internal)))
		  props))
	 (user-propdocs (map docfunc uprops))
	 (internal-propdocs (map docfunc iprops)))

    (string-append
     desc

     (if (pair? uprops)
	 (string-append
	  "\n\n@subsubheading User settable properties:\n"
	  (description-list->texi user-propdocs #t))
	 "")

     (if (pair? iprops)
	 (string-append
	  "\n\n@subsubheading Internal properties:\n"
	  (description-list->texi internal-propdocs #t))
	 ""))))

(define iface->grob-table (make-vector 61 '()))
;; extract ifaces, and put grob into the hash table.
(map
 (lambda (x)
   (let* ((meta (assoc-get 'meta (cdr x)))
	  (ifaces (assoc-get 'interfaces meta)))

     (map (lambda (iface)
	    (hashq-set!
	     iface->grob-table iface
	     (cons (car x)
		   (hashq-ref iface->grob-table iface '()))))
	  ifaces)))
 all-grob-descriptions)

;; First level Interface description
(define (interface-doc interface)
  (let* ((name (symbol->string (car interface)))
	 (interface-list (human-listify
			  (map ref-ify
			       (sort
				(map symbol->string
				     (hashq-ref iface->grob-table
						(car interface)
						'()))
				ly:string-ci<?)))))
    (make <texi-node>
      #:name name
      #:text (string-append
	      (interface-doc-string (cdr interface) '())
	      "\n\n"
	      "This grob interface "
	      (if (equal? interface-list "none")
		  "is not used in any graphical object"
		  (string-append
		   "is used in the following graphical object(s): "
		   interface-list))
	      "."))))

(define (grob-alist->texi alist)
  (let* ((uprops (filter (lambda (x) (not (object-property x 'backend-internal)))
			 (map car alist))))

    (description-list->texi
     (map (lambda (y) (property->texi 'backend y alist))
	  uprops)
     #t)))

(define (grob-doc description)
  "Given a property alist DESCRIPTION, make a documentation
node."

  (let* ((meta (assoc-get 'meta description))
	 (name (assoc-get 'name meta))
	 ;;       (bla (display name))
	 (ifaces (map lookup-interface (assoc-get 'interfaces meta)))
	 (ifacedoc (map ref-ify
			(sort
			 (map (lambda (iface)
				(if (pair? iface)
				    (symbol->string (car iface))
				    (ly:error (_ "pair expected in doc ~s") name)))
			      ifaces)
			 ly:string-ci<?)))
	 (engravers (filter
		     (lambda (x) (engraver-makes-grob? name x))
		     all-engravers-list))
	 (namestr (symbol->string name))
	 (engraver-names (map symbol->string
			      (map ly:translator-name engravers)))
	 (engraver-list (human-listify
			 (map ref-ify
			      (map engraver-name engraver-names)))))

    (make <texi-node>
      #:name namestr
      #:text
      (string-append
       namestr " objects "
       (if (equal? engraver-list "none")
	   "are not created by any engraver"
	   (string-append
	    "are created by: "
	    engraver-list))
       "."

       "\n\nStandard settings:\n\n"
       (grob-alist->texi description)
       "\n\nThis object supports the following interface(s):\n"
       (human-listify ifacedoc)
       "."))))

(define (all-grobs-doc)
  (make <texi-node>
    #:name "All layout objects"
    #:desc "Description and defaults for all graphical objects (grobs)."
    #:children
    (map (lambda (x) (grob-doc (cdr x)))  all-grob-descriptions)))

(define interface-description-alist
  (hash-fold
   (lambda (key val prior)
     (cons (cons key val)  prior))
   '() (ly:all-grob-interfaces)))

(set! interface-description-alist
  (sort interface-description-alist ly:alist-ci<?))

;;;;;;;;;; check for dangling backend properties.
(define (mark-interface-properties entry)
  (map (lambda (x) (set-object-property! x 'iface-marked #t))
       (caddr (cdr entry))))

(map mark-interface-properties interface-description-alist)

(define (check-dangling-properties prop)
  (if (not (object-property prop 'iface-marked))
      (ly:error (string-append "define-grob-properties.scm: "
		(_ "cannot find interface for property: ~S")) prop)))

(map check-dangling-properties all-backend-properties)

;;;;;;;;;;;;;;;;

(define (lookup-interface name)
  (let* ((entry (hashq-ref (ly:all-grob-interfaces) name #f)))
    (if entry
	entry
	(ly:error (_ "unknown Grob interface: ~S") name))))

(define (all-interfaces-doc)
  (make <texi-node>
    #:name "Graphical Object Interfaces"
    #:desc "Building blocks of graphical objects."
    #:children
    (map interface-doc interface-description-alist)))

(define (backend-properties-doc-string lst)
  (let* ((ps (sort (map symbol->string lst) ly:string-ci<?))
	 (descs (map (lambda (prop)
		       (property->texi 'backend (string->symbol prop) '())) ps))
	 (texi (description-list->texi descs #f)))
    texi))

;;(dump-node (grob-doc (cdadr all-grob-descriptions)) (current-output-port) 0 )
(define (backend-doc-node)
  (make <texi-node>
    #:name "Backend"
    #:desc "Reference for the layout engine."
    #:children
    (list
     (all-grobs-doc)
     (all-interfaces-doc)
     (make <texi-node>
       #:name "User backend properties"
       #:desc "All tunable properties in a big list."
       #:text (backend-properties-doc-string all-user-grob-properties))
     (make <texi-node>
       #:name "Internal backend properties"
       #:desc "All internal layout properties in a big list."
       #:text (backend-properties-doc-string all-internal-grob-properties)))))
