;;; backend-documentation-lib.scm -- Functions for backend documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>


;;; This file generates documentation for the backend of lilypond.

;; alist of property descriptions


(define (document-element-property property-def element-description only-doc-if-set)
  "
"
  (let* (
	(handle (assoc (car property-def) element-description))
	(def-val-str (if (eq? handle #f)
			 "not set"
			 (scm->texi (cdr handle))))
				
	(name (symbol->string (car property-def)))
	(type (type-name (cadr property-def)))
	(desc (caddr property-def))
	)

    (if (and  (eq? handle #f) only-doc-if-set)
	'("" . "")
	(cons (string-append "@code{" name "} "
		       "(" type ")"
		       ":" )
	      (string-append desc
			     "\nDefault value: "
			     def-val-str))
    ))
  )

(define (document-interface where interface element-description)
  "

"
  (let* ((level (if (eq? where 'element) 3 2))
	 (name (car interface))
	 (desc (cadr interface))
	 (props (caddr interface))
	 (docfun  (lambda (x)
		    (document-element-property
		     x element-description (eq? where 'element))))
	 (docs (map docfun props))
	 )

    (string-append
     (texi-section level (string-append (interface-name (symbol->string name))) (eq? where 'element)) ;gur.
     desc
     
     (description-list->texi docs)
     )))

;; First level Interface description
(define (document-separate-interface interface)
  (let ((name (car interface)))
    (processing name)
    (string-append
     (node (interface-name name))
     (document-interface 'self interface '()))))

;; First level element description
(define (document-element iname description)
  (processing iname)
  (let* ((metah (assoc 'meta description))
	 
	 (meta (if (pair? metah)
		   (cdr metah)
		   '((properties . ()) (name . "huh?"))
		   ))
	 
	 (name (cdr (assoc 'name meta)))
	 (ifaces (cdr (assoc 'interface-descriptions meta)))
	 (ifacedoc (map (lambda (x) (document-interface 'element x description))
			(reverse ifaces))))
    
    (string-append
     (node (element-name name))
     (texi-section 2 (element-name name) #f)
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
(define xinterface-description-alist
  `(
    (general-element . ,general-element-interface)
    (beam . ,beam-interface)
    (clef . ,clef-interface)
    (slur . ,slur-interface)
    ))

;; burp, need these for running outside of LilyPond
(if #f
    (begin

      (debug-enable 'backtrace)

      (define (number-pair?  x)
	(and (pair? x) (number? (car x)) (number? (cdr x))))
      
      (define (ly-gulp-file x) "")
      (define (ly-element? x) #f)
      (define (ly-input-location? x) #f)
      (define (dir? x) #f)
      (define (moment? x) #f)
      (load "lily.scm")))

(use-modules (ice-9 string-fun))

(define interface-file-str (string-append (ly-gulp-file "interface.scm") "\n(define "))
(define (list-interface-names)
  (let* ((text interface-file-str)
	 (r (make-regexp 
	     "\n[(](define *([a-z-]*-interface)*)*[^\n]*"))
	 (t (regexp-substitute/global #f r text 2 " " 'post))
	 (ugh (regexp-substitute/global #f "#f *" t 'pre 'post))
	 (l (separate-fields-discarding-char #\  ugh list)))
    (reverse (cdr (reverse l)))))

(eval (ly-gulp-file "interface.scm"))

(define interface-description-alist
  (map (lambda (x) (cons (string->symbol x) (eval-string x)))
	     (list-interface-names)))

(define (document-all-interfaces name)
  (string-append
   (texi-node-menu name (map (lambda (x) (cons (interface-name x) ""))
			     (map cadr interface-description-alist)))
   (apply string-append
	  (map document-separate-interface
	       (map cdr interface-description-alist)))))


