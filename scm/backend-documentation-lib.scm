
;;; backend-documentation-lib.scm -- Functions for backend documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>


;;; This file generates documentation for the backend of lilypond.


;; alist of property descriptions
(define (document-element-property prop desc)
  (let ((handle (assoc (car prop) desc)))
    (cons
     (string-append
      "@code{" (symbol->string (car prop)) "} "
      "(" (type-name (cadr prop)) "):")
     (string-append
      (caddr prop)
      "\ndefault value: @code{"
      (if (pair? handle)
	  (scm->string (cdr handle))
	  "not set" )
      "}"))))

;; First level Interface description
;; Second level, part of element description
(define (document-interface level interface element-description)
  (let* ((name (car interface))
	 (desc (cadr interface))
	 (props (caddr interface))
	 (docs (map (lambda (x)
		      (document-element-property x element-description))
		    props)))

    (string-append
     (section level (string-append (interface-name (symbol->string name))))
     desc
     (description-list docs))))

;; First level Interface description
(define (document-separate-interface interface)
  (let ((name (car interface)))
    (string-append
     (node (interface-name name))
     (document-interface 2 interface '()))))

;; First level element description
(define (document-element iname description)
  (display (string-append "\nProcessing " iname " ... ") (current-error-port))
  (let* ((metah (assoc 'meta description))
	 
	 (meta (if (pair? metah)
		   (cdr metah)
		   '((properties . ()) (name . "huh?"))
		   ))
	 
	 (name (cdr (assoc 'name meta)))
	 (ifaces (cdr (assoc 'interface-descriptions meta)))
	 (ifacedoc (map (lambda (x) (document-interface 3 x description))
			(reverse ifaces))))
    
    (string-append
     (node (element-name name))
     (section 2 (element-name name))
     "\n"

     (let* ((element (string->symbol name))
	    (engravers
	     (apply append
		    (map (lambda (x)
			   (let ((engraver (car x))
				 (objs (cadddr x)))
			     (if (member element objs)
				 (list engraver)
				 '())))
			 engraver-description-alist))))
       (string-append
	name " elements are created by: "
	(human-listify (map reffy (map engraver-name engravers)))))

     (apply string-append ifacedoc))))
     

(define (document-all-elements name)
  (let* ((doc (apply string-append
		     (map (lambda (x) (document-element (car x) (cdr x)))
			  all-element-descriptions)))
	 (names (map car all-element-descriptions)))

    (string-append
     (texi-node-menu name (map (lambda (x) (cons (element-name x) ""))
			       names))
     doc)))

;; testin.. -- how to do this
(eval-string (ly-gulp-file "interface.scm"))
(define interface-description-alist
  `(
    (general-element . ,general-element-interface)
    (beam . ,beam-interface)
    (clef . ,clef-interface)
    (slur . ,slur-interface)
    ))
	      
(define (document-all-interfaces name)
  (string-append
   (texi-node-menu name (map (lambda (x) (cons (interface-name x) ""))
			     (map cadr interface-description-alist)))
   (apply string-append
	  (map document-separate-interface
	       (map cdr interface-description-alist)))))


