;;;
;;; documentation-lib.scm -- Assorted Functions for generated documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>

(use-modules (oop goops))

(define-class <texi-node> ()
  (children #:init-value '() #:accessor node-children #:init-keyword #:children)
  (text #:init-value "" #:accessor node-text #:init-keyword #:text)
  (name #:init-value "" #:accessor node-name #:init-keyword #:name)
  (description #:init-value "" #:accessor node-desc #:init-keyword #:desc)
  )

(define (menu-entry x)
  (cons
   (node-name x)
   (node-desc x))
  )

(define (dump-node node port level)
  (display
   (string-append
    "\n@html"
    "\n<hr>"
    "\n@end html\n@node "
    (node-name node)
    "\n\n"
    (texi-section-command level) " "
    (node-name node)
    "\n\n"
    (node-text node)
    "\n\n"
    (if (pair? (node-children node))
	(texi-menu
	 (map (lambda (x) (menu-entry x) )
	      (node-children node)))
	 ""))
   port)
  (map (lambda (x) (dump-node x port (+ 1 level)))
        (node-children node))
  )

(define (processing name)
  (display (string-append "\nProcessing " name " ... ") (current-error-port)))

(define (self-evaluating? x)
  (or (number? x) (string? x) (procedure? x) (boolean? x)))

(define (texify x)
  x)

(define (scm->texi x)
  (string-append "@code{" (texify (scm->string x)) "}")
  )


;;
;; don't confuse users with #<procedure .. > syntax. 
;; 
(define (scm->string val)
  (if (and (procedure? val) (symbol? (procedure-name val)))
      (symbol->string (procedure-name val))
      (string-append
       (if (self-evaluating? val) "" "'")
       (call-with-output-string (lambda (port) (display val port)))
       )))


(define (texi-section-command level)
  (cdr (assoc level '(
    ;; Hmm, texinfo doesn't have ``part''
    (0 . "@top")
    (1 . "@unnumbered")
    (2 . "@unnumberedsec")
    (3 . "@unnumberedsubsec")
    (4 . "@unnumberedsubsubsec")
    (5 . "@unnumberedsubsubsec")
    ))))

(define (one-item->texi label-desc-pair)
  "Document one (LABEL . DESC); return empty string if LABEL is empty string. 
"
  (if (eq? (car label-desc-pair) "")
      ""
      (string-append "\n@item " (car label-desc-pair) "\n" (cdr label-desc-pair))
  ))


(define (description-list->texi items-alist)
  "Document ITEMS-ALIST in a table. entries contain (item-label
. string-to-use)
"
  (string-append
   "\n@table @asis\n"
   (apply string-append (map one-item->texi items-alist))
   "\n@end table\n"))

(define (texi-menu items-alist)
  "Generate what is between @menu and @end menu."
  (let
      (
       (maxwid (apply max (map (lambda (x) (string-length (car x)))
			       items-alist)))
       )
    

    
  (string-append
  "\n@menu"
  (apply string-append
	 (map (lambda (x)
		(string-append
		(pad-string-to 
		 (string-append "\n* " (car x) ":: ")
		 (+ maxwid 8)
		 )
		(cdr x))
		)
	      items-alist))
  "\n@end menu\n"
  ;; Menus don't appear in html, so we make a list ourselves
  "\n@ignore\n"
  "\n@ifhtml\n"
  (description-list->texi (map (lambda (x) (cons (ref-ify (car x)) (cdr x)))
			 items-alist))
  "\n@end ifhtml\n"
  "\n@end ignore\n")))

  


(define (texi-file-head name file-name top)
  (string-append
   "\\input texinfo @c -*-texinfo-*-"
   "\n@setfilename " file-name ".info"
   "\n@settitle " name
   "\n@dircategory GNU music project"
   "\n@direntry"
   ;; prepend GNU for dir, must be unique
   "\n* GNU " name ": (" file-name ").          " name "."
   "\n@end direntry"
   ))


(define (context-name name)
  name)

(define (engraver-name name)
  name)

(define (grob-name name)
  (if (symbol? name)
      (symbol->string name)
      name))

(define (interface-name name)
  name)

(define (ref-ify x)
  "Add ref to X"
  (string-append "@ref{" x "}"))

(define (human-listify l)
  "Produce a textual enumeration from L, a list of strings"
  
  (cond
   ((null? l) "none")
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (string-append (car l) " and " (cadr l)))
   (else (string-append (car l) ", " (human-listify (cdr l))))
   ))

(define (writing-wip x)
  (display (string-append "\nWriting " x " ... ") (current-error-port)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property  stuff.

(define (property->texi where sym)
  "Document SYM for WHERE (which can be translation, backend, music)"
  (let* (
	 (name (symbol->string sym))
	 (type?-name (string->symbol
		      (string-append (symbol->string where) "-type?")))
	 (doc-name (string->symbol		    
		    (string-append (symbol->string where) "-doc")))
	 (type (object-property sym type?-name))
	 (typename (type-name type))
	 (desc (object-property sym doc-name)))

    (if (eq? desc #f)
	(error "No description for property ~S" sym)
	)
    (cons
     (string-append "@code{" name "} "
		    "(" typename ")")
     desc)
     
    ))

(define (document-property-value sym alist)
  "Extract value for SYM from ALIST, return as texi string"
  (let* ((handle (assoc sym alist)))
    (if (eq? handle #f)
	"(unset)"
	(scm->texi (cdr handle)))))


(define (backend-property->texi sym)
  (property->texi 'backend sym))

(define (document-property sym where alist)
  "Document SYM. If GROB-DESCRIPTION is not #f, it's an alist
containing default values."
  (let*
      ((without (property->texi where sym))
       (rv

    (cons (car without)
	  (if (eq? alist #f)
	      (cdr without)
	      (string-append
	       (cdr without)
	       "\nDefault value: "
	       (document-property-value sym alist)))))

       )
;    (display rv)
    rv  ))
