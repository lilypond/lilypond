
;;; backend-documentation-lib.scm -- Functions for backend documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>


;;; This file generates documentation for the backend of lilypond.

;; alist of property descriptions
;; when called by First level Interface description, desc == '()
;; CDR "not set" is only used for Second level Element description
(define (document-element-property prop desc)
  (let ((handle (assoc (car prop) desc)))
    (cons
     (string-append "@code{" (symbol->string (car prop)) "} "
		    "(" (type-name (cadr prop)) ")"
		    (if (equal? desc '()) "" ":"))
     (string-append (if (equal? desc '())
			(caddr prop)
			(if (pair? handle)
			    (string-append (caddr prop)
					   "\ndefault value: @code{"
					   (scm->string (cdr handle))
					   "}")
			    "not set"))))))

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
     
     (description-list
      ;; filter-out entries with CDR "not set"
      (apply append
	     (map (lambda  (x)
		    (if (string-match "not set" (cdr x)) '() (list x)))
		  docs))))))

;; First level Interface description
(define (document-separate-interface interface)
  (let ((name (car interface)))
    (processing name)
    (string-append
     (node (interface-name name))
     (document-interface 2 interface '()))))

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
(define (list-interface-names)
  (let* ((text (string-append (ly-gulp-file "interface.scm") "\n(define "))
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


