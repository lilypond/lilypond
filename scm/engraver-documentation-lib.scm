
;;; engraver-documentation-lib.scm -- Functions for engraver documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>


(eval-string (ly-gulp-file "translator-description.scm"))

;; alist of translater descriptions
(define (document-translator-property sym)
   (cons
    (string-append
     "@code{" (symbol->string sym) "} "
     "(" (type-name (object-property sym 'translation-type?)) "):")
    (object-property sym 'translation-doc)))

;; First level Engraver description and
;; second level Context description
(define (document-engraver where engraver-descr)
  (let* (
	 (level (if (eq? where 'context) 3 2))
	 (props (car (cdddr engraver-descr)))
	 (name (car engraver-descr))
	 (name-sym (string->symbol name))
	 (desc (cadr engraver-descr))
	 (objs (map symbol->string (caddr engraver-descr)))
	 )

    (string-append
     (texi-section level (engraver-name name) (eq? where 'context))
     desc
     "\n\n"
     (if (null? props)
	 ""
	 (string-append
	  (texi-section (+ level 1) "Properties" #f)
	  (description-list->texi
	   (map (lambda (x) (document-translator-property x)) props))))
     (if  (null? objs)
	  ""
	  (string-append
	   "This engraver creates the following grobs: \n "
	   (human-listify (map ref-ify (uniq-list (sort  objs string<? ))))
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
(define (document-separate-engraver top description)
  (let ((name (car description)))
    (processing name)
    (string-append
     (node (engraver-name name))
     (document-engraver 'self description))))

;; Second level, part of Context description
(define (document-engraver-by-name name)
  (let*
      (
       (eg (assoc (string->symbol name) engraver-description-alist))
       )

    (if (eq? eg #f)
	(string-append "Engraver " name ", not documented.\n")
	(document-engraver 'context (cdr eg))
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
       (grobs  (context-grobs context-desc))
       (grob-refs (map (lambda (x) (ref-ify x)) grobs))
       )
    
    (string-append 
     desc
     "\n\nThis context creates the following grobs: \n\n"
     (human-listify (uniq-list (sort grob-refs string<? )))
     "."
     
     (if (null? accepts)
	 "This context is a `bottom' context; it can not contain other contexts."
	 (string-append
	  "\n\nContext "
	  name " can contain \n"
	  (human-listify (map ref-ify (map context-name accepts)))))
     
     "\n\nThis context is built from the following engravers: "
     (if no-copies
	 (human-listify (map ref-ify (map engraver-name consists)))
	 (apply string-append 
		(map document-engraver-by-name consists))))))

(define (engraver-grobs  name)
  (let* (
	 (eg (assoc (string->symbol name) engraver-description-alist))
      )

    (if (eq? eg #f)
	'()
	(map symbol->string (caddr (cdr eg)))
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


;; First level Context description
(define (document-context top context-desc)
  (let ((name (cdr (assoc 'type-name context-desc)))
	(doc (context-doc-string context-desc)))
    (processing name)
    (string-append
     (node (context-name name))
     (texi-section 2 (context-name name) #f)
      doc)))

(define (symbol<? l r)
  (string<? (symbol->string l) (symbol->string r)))

(define (document-paper name)
  (let* ((paper-alist
	  (sort (My_lily_parser::paper_description)
		(lambda (x y) (symbol<? (car x) (car y)))))
	 (names (sort (map symbol->string (map car paper-alist)) string<?))
	 (contexts (map cdr paper-alist))
	 (doc (apply string-append
		     (map (lambda (x) (document-context name x)) contexts))))
    
    (string-append
     (texi-node-menu name (map (lambda (x) (cons (context-name x) ""))
			       names))
     doc)))

(define (document-all-engravers name)
  (let* ((descs (map cdr engraver-description-alist))
	 (names (map symbol->string (map car engraver-description-alist)))
	 (doc (apply string-append
		     (map (lambda (x) (document-separate-engraver name x))
			  descs))))
    (string-append
     (texi-node-menu name (map (lambda (x) (cons (engraver-name x) ""))
			       names))
     doc)))

(define (document-all-engraver-properties name)
  (let* ((ps (sort (map symbol->string all-translation-properties) string<?))
	 (sortedsyms (map string->symbol ps))
	 (propdescs (map document-translator-property sortedsyms))
	 (texi (description-list->texi propdescs)))
     
  (string-append
	  (node name)
	  (texi-section 1 name #f)
	  texi)))
