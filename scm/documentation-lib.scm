;;;;
;;;; documentation-lib.scm -- Assorted Functions for generated documentation
;;;;
;;;; source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

(use-modules (oop goops)
	     (srfi srfi-13)
	     (srfi srfi-1))

(define-class <texi-node> ()
  (appendix #:init-value #f #:accessor appendix? #:init-keyword #:appendix)
  (children #:init-value '() #:accessor node-children #:init-keyword #:children)
  (text #:init-value "" #:accessor node-text #:init-keyword #:text)
  (name #:init-value "" #:accessor node-name #:init-keyword #:name)
  (description #:init-value "" #:accessor node-desc #:init-keyword #:desc))

(define (menu-entry x)
  (cons
   (node-name x)
   (node-desc x)))

(define* (dump-node node port level)
  (display
   (string-append
    "\n@node "
    (if (= level 0) "Top" (node-name node))
    "\n"
    (if (appendix? node)
        (texi-appendix-section-command level)
        (texi-section-command level))
    " "
    (node-name node)
    "\n\n"
    (node-text node)
    "\n\n"
    (if (pair? (node-children node))
	(texi-menu
	 (map (lambda (x) (menu-entry x))
	      (node-children node)))
	""))
   port)
  (map (lambda (x) (dump-node x port (+ 1 level)))
       (node-children node)))

(define (processing name)
  (ly:message (_ "Processing ~S...") name))

(define (self-evaluating? x)
  (or (number? x) (string? x) (procedure? x) (boolean? x)))

(define (texify x)
  x)

(define (scm->texi x)
  (string-append "@code{" (texify (scm->string x)) "}"))



(define (texi-section-command level)
  (cdr (assoc level '(
		      ;; Hmm, texinfo doesn't have ``part''
		      (0 . "@top")
		      (1 . "@chapter")
		      (2 . "@section")
		      (3 . "@subsection")
		      (4 . "@unnumberedsubsubsec")
		      (5 . "@unnumberedsubsubsec")))))

(define (texi-appendix-section-command level)
  (cdr (assoc level '((0 . "@top")
		      (1 . "@appendix")
		      (2 . "@appendixsec")
		      (3 . "@appendixsubsec")
		      (4 . "@appendixsubsubsec")
		      (5 . "@appendixsubsubsec")))))

(define (one-item->texi label-desc-pair)
  "Document one (LABEL . DESC); return empty string if LABEL is empty string."
  (if (eq? (car label-desc-pair) "")
      ""
      (string-append "\n@item " (car label-desc-pair) "\n" (cdr label-desc-pair))))


(define (description-list->texi items-alist quote?)
  "Document ITEMS-ALIST in a table; entries contain (item-label .
string-to-use).  If QUOTE? is #t, embed table in a @quotation environment."
  (string-append
   "\n"
   (if quote? "@quotation\n" "")
   "@table @asis\n"
   (apply string-append (map one-item->texi items-alist))
   "\n"
   "@end table\n"
   (if quote? "@end quotation\n" "")))

(define (texi-menu items-alist)
  "Generate what is between @menu and @end menu."
  (let ((maxwid
	 (apply max (map (lambda (x) (string-length (car x))) items-alist))))
    
    (string-append
     "\n@menu"
     (apply string-append
	    (map (lambda (x)
		   (string-append
		    (string-pad-right 
		     (string-append "\n* " (car x) ":: ")
		     (+ maxwid 8))
		    (cdr x)))
		 items-alist))
     "\n@end menu\n"
     ;; Menus don't appear in html, so we make a list ourselves
     "\n@ignore\n"
     "\n@ifhtml\n"
     (description-list->texi (map (lambda (x) (cons (ref-ify (car x)) (cdr x)))
				  items-alist)
			     #t)
     "\n@end ifhtml\n"
     "\n@end ignore\n")))

(define (texi-file-head name file-name top)
  (string-append
   "\\input texinfo @c -*-texinfo-*-"
   "\n@setfilename " file-name ".info"
   "\n@settitle " name
   "\n@dircategory LilyPond"
   "\n@direntry"
   ;; prepend GNU for dir, must be unique
   "\n* GNU " name ": (" file-name ").          " name "."
   "\n@end direntry\n"
   "@documentlanguage en\n"
   "@documentencoding utf-8\n"))

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
  "Return @ref{X}. If mapping ref-ify to a list that needs to be sorted,
   sort the list first."
  (string-append "@ref{" x "}"))

(define (human-listify lst)
  "Produce a textual enumeration from LST, a list of strings"
  
  (cond
   ((null? lst) "none")
   ((null? (cdr lst)) (car lst))
   ((null? (cddr lst)) (string-append (car lst) " and " (cadr lst)))
   (else (string-append (car lst) ", " (human-listify (cdr lst))))))

(define (writing-wip x)
  (ly:message (_ "Writing ~S...") x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; property  stuff.

(define (property->texi where sym . rest)
  "Document SYM for WHERE (which can be translation, backend, music),
with init values from ALIST (1st optional argument)
"
  (let* ((name (symbol->string sym))
	 (alist (if (pair? rest) (car rest) '()))
	 (type?-name (string->symbol
		      (string-append (symbol->string where) "-type?")))
	 (doc-name (string->symbol		    
		    (string-append (symbol->string where) "-doc")))
	 (type (object-property sym type?-name))
	 (typename (type-name type))
	 (desc (object-property sym doc-name))
	 (handle (assoc sym alist)))

    (if (eq? desc #f)
	(ly:error (_ "cannot find description for property ~S (~S)") sym where))
    
    (cons
     (string-append "@code{" name "} "
		    "(" typename ")"
		    (if handle
			(string-append
			 ":\n\n"
			 (scm->texi (cdr handle))
			 "\n\n")
			""))
     desc)))

