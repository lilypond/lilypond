
;;; engraver-documentation-lib.scm -- Functions for engraver documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>


(eval-string (ly-gulp-file "translator-description.scm"))

;; alist of translater descriptions
(define (document-translator-property prop-desc)
   (cons
    (string-append
     "@code{" (car prop-desc) "} "
     "(" (type-name (cadr prop-desc)) "):")
    (caddr prop-desc)))

;; First level Engraver description and
;; second level Context description
(define (document-engraver level engraver-descr)
 
  (let* (
	 (props (car (cdddr engraver-descr)))
	 (name (car engraver-descr))
	 (desc (cadr engraver-descr))
	 (objs (caddr engraver-descr))
	 )

    (string-append
     (section level (engraver-name name))
     desc
     "\n\n"
     (if (null? props)
	 ""
	 (string-append
	  (section (+ level 1) "Properties")
	  (description-list
	   (map (lambda (x) (document-translator-property x)) props))))
     (if  (null? objs)
	  ""
	  (string-append
	   "This engraver creates \n "
	   (human-listify (map reffy (map element-name objs)))
	   " elements.")
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
	(human-listify (map reffy (map context-name contexts))))))))


;; First level Engraver description
(define (document-separate-engraver top description)
  (let ((name (car description)))
    (string-append
     (node (engraver-name name))
     (document-engraver 2 description))))

;; Second level, part of Context description
(define (document-engraver-by-name name)
  (let*
      (
       (eg (assoc (string->symbol name) engraver-description-alist))
       )

    (if (eq? eg #f)
	(string-append "Engraver " name ", not documented.\n")
	(document-engraver 3 (cdr eg))
 	)
    ))

(define (context-doc-string context-desc)
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
       )
    
    (string-append 
     desc
     
     (if (null? accepts)
	 "This context is a `bottom' context; it can not contain other contexts."
	 (string-append
	  name " can contain \n"
	  (human-listify (map reffy (map context-name accepts)))))
     
     "\n\nThis context is built from the following engravers: "
     (if no-copies
	 (human-listify (map reffy (map engraver-name consists)))
	 (apply string-append 
		(map document-engraver-by-name consists))))))


;; First level Context description
(define (document-context top context-desc)
  (let ((name (cdr (assoc 'type-name context-desc)))
	(doc (context-doc-string context-desc)))
    
    (string-append
     (node (context-name name))
     (section 2 (context-name name))
      doc)))

(define (document-paper name)
  (let* ((paper-alist (My_lily_parser::paper_description))
	 (names (sort (map car paper-alist) string<?))
	 (contexts (map cdr paper-alist))
	 (doc (apply string-append
		     (map (lambda (x) (document-context name x)) contexts))))
    
    (string-append
     (texi-node-menu name (map (lambda (x) (cons (context-name x) ""))
			       names))
     doc)))

(define (document-all-engravers name)
  (let* ((descs (map cdr engraver-description-alist))
	 (names (map car engraver-description-alist))
	 (doc (apply string-append
		     (map (lambda (x) (document-separate-engraver name x))
			  descs))))
    
    (string-append
     (texi-node-menu name (map (lambda (x) (cons (engraver-name x) ""))
			       names))
     doc)))

