;;; backend-documentation-lib.scm -- Functions for backend documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>


;;; This file generates documentation for the backend of lilypond.

;; alist of property descriptions


(define (backend-property->texi sym)
  (let* ((name (symbol->string sym))
	(type (object-property sym 'backend-type?))
	(typename (type-name type))
	(desc (object-property sym 'backend-doc)))

    (cons (string-append "@code{" name "} "
		       "(" typename ")"
		       ": "

; index gets too messy
;		       "@vindex " name "\n"


		       )
	  desc)))

(define (document-grob-property sym grob-description )
  (let* ((handle (assoc sym grob-description))
	 (defval (if (eq? handle #f)
		     "(unset)"
		   (scm->texi (cdr handle))))
	 (propdoc (backend-property->texi sym)))
	 
    (cons (car propdoc) (string-append (cdr propdoc)
					   "\nDefault value: "
					   defval))))

(define (document-interface where interface grob-description)
  (let* ((level (if (eq? where 'grob) 3 2))
	 (name (car interface))
	 (desc (cadr interface))
	 (props (caddr interface))
	 (docfunc (lambda (x)
		    (document-grob-property
		     x grob-description )))
	 (docs (map docfunc props)))

    (string-append
     (texi-section level
		   (string-append (interface-name (symbol->string name)))
		   (eq? where 'grob)) ;gur.
     desc
     (description-list->texi docs))))

;; First level Interface description
(define (document-separate-interface interface)
  (let ((name (symbol->string (car interface))))
    (processing name)
    (string-append
     (node (interface-name name))
     (document-interface 'self interface '()))))

;; First level grob description
(define (document-grob iname description)
  (processing iname)
  (let* ((metah (assoc 'meta description))
	 
	 (meta (if (pair? metah)
		   (cdr metah)
		   '((properties . ()) (name . "huh?"))
		   ))
	 
	 (name (cdr (assoc 'name meta)))
	 (ifaces (cdr (assoc 'interface-descriptions meta)))
	 (ifacedoc (map (lambda (x)
			  (document-interface 'grob x description))
			(reverse ifaces))))

    (string-append
     (node (grob-name name))
     (texi-section 2 (grob-name name) #f)
     "\n"

     (let* ((grob (string->symbol name))
	    (engravers
	     (apply append
		    (map (lambda (x)
			   (let ((engraver (car x))
				 (objs (cadddr x)))
			     (if (member grob objs)
				 (list engraver)
				 '())))
			 engraver-description-alist))))
       (string-append
	name " grobs are created by: "
	(human-listify (map ref-ify
			    (map engraver-name
				 (map symbol->string engravers))))))

     (apply string-append ifacedoc))))
     

(define (document-all-grobs name)
  (let* ((doc (apply string-append
		     (map (lambda (x)
			    (document-grob (symbol->string (car x)) (cdr x)))
			  all-grob-descriptions)))
	 (names (map symbol->string (map car all-grob-descriptions))))

    (string-append
     (texi-node-menu name (map (lambda (x) (cons (grob-name x) ""))
			       names))
     doc)))

;; ugh, this works standalone, but not anymore with lily
(if (not (defined? 'standalone))
    (begin

      (debug-enable 'backtrace)

      (load "standalone.scm")

      (define (number-pair?  x)
	(and (pair? x) (number? (car x)) (number? (cdr x))))
      
      (define (ly-grob? x) #f)
      (define (ly-input-location? x) #f)
      (define (dir? x) #f)
      (define (moment? x) #f)
      ))

(use-modules (ice-9 string-fun))

(if standalone
  (begin
    (display "(define (list-interface-names) '") 
    (write (ugh-standalone-list-interface-names))
    (display ")")
    (exit 0)))


(define interface-description-alist
  (map (lambda (x) (cons (string->symbol x) (eval-string x)))
	     (interface-names)))

(set! interface-description-alist (sort interface-description-alist alist<?))

(define (document-all-interfaces name)
  (string-append
   (texi-node-menu name (map (lambda (x)
			       (cons (interface-name (symbol->string x)) ""))
			     (map cadr interface-description-alist)))
   (apply string-append
	  (map document-separate-interface
	       (map cdr interface-description-alist)))))

(define (document-all-backend-properties name)
  (let*
      (
       (ps (sort (map symbol->string all-backend-properties) string<?))
       (descs (map (lambda (prop)
		     (backend-property->texi (string->symbol prop)))
		   ps))
       (texi (description-list->texi descs))
       )
    
    (string-append
     (node name)
     (texi-section 1 name #f)
     texi)
  )
  )
  
