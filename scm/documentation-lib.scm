;;
;;; documentation-lib.scm -- Assorted Functions for generated documentation
;;;
;;; source file of the GNU LilyPond music typesetter
;;; 
;;; (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
;;; Jan Nieuwenhuizen <janneke@gnu.org>

(define (uniqued-alist  alist acc)
  (if (null? alist) acc
      (if (assoc (caar alist) acc)
	  (uniqued-alist (cdr alist) acc)
	  (uniqued-alist (cdr alist) (cons (car alist) acc)
  ))))


(define (alist<? x y)
  (string<? (symbol->string (car x))
	    (symbol->string (car y))))

(define (processing name)
  (display (string-append "\nProcessing " name " ... ") (current-error-port)))

(define (self-evaluating? x)
  (or (number? x) (string? x) (procedure? x) (boolean? x)))

(define (texify x)
  x)
;;  (let*
;;     ((x1 (regexp-substitute/global #f "\([^@]\){" x 'pre "\1@{" 'post))
;;      ((x2 (regexp-substitute/global #f "\([^@]\){" x 'pre "\1@{" 'post))
;;      ((x3 (regexp-substitute/global #f "\([^@]\)@" x 'pre "\1@@" 'post))
;;       )
;;    x2))



(define (scm->texi x)
  (string-append "@code{" (texify (scm->string x)) "}")
  )

(define (scm->string val)
  (string-append
   (if (self-evaluating? val) "" "'")
   (call-with-output-string (lambda (port) (display val port)))
  ))

(define (node name)
  (string-append
   "\n@html"
   "\n<hr>"
   "\n@end html"
   "\n@node " name ",,,"))

(define texi-section-alist
  '(
    ;; Hmm, texinfo doesn't have ``part''
    (0 . "@top")
    (1 . "@unnumbered")
    (2 . "@unnumberedsec")
    (3 . "@unnumberedsubsec")
    (4 . "@unnumberedsubsubsec")
    (5 . "@unnumberedsubsubsec")
    ))
    
(define (texi-section level name ref)
  "texi sectioning command (lower LEVEL means more significant).
Add a ref if REF is set
"
     
  (string-append
   "\n" (cdr (assoc level texi-section-alist)) " "
   (if ref
       (string-append "@ref{" name "}") 
       name)
   "\n"))


(define (one-item->texi label-desc-pair)
  "Document one (LABEL . DESC); return empty string if LABEL is empty string. 
"
  (if (eq? (car label-desc-pair) "")
      ""
      (string-append "\n@item " (car label-desc-pair) "\n" (cdr label-desc-pair))
  ))


(define (description-list->texi items-alist)
  "Document ITEMS-ALIST in a table. entries contain (item-label . string-to-use)
"
  (string-append
   "\n@table @samp\n"
   (apply string-append (map one-item->texi items-alist))
   "\n@end table\n"))

(define (texi-menu items-alist)
  (string-append
  "\n@menu"
  (apply string-append
	 (map (lambda (x) (string-append "\n* " (car x) ":: " (cdr x)))
	      items-alist))
  "\n@end menu\n"
  ;; Menus don't appear in html, so we make a list ourselves
  "\n@ignore\n"
  "\n@ifhtml\n"
  (description-list->texi (map (lambda (x) (cons (reffy (car x)) (cdr x)))
			 items-alist))
  "\n@end ifhtml\n"
  "\n@end ignore\n"))

  
(define (texi-node-menu name items-alist)
  (string-append
   (node name)
   (texi-section 1 name #f)
   (texi-menu items-alist)))

(define (texi-file-head name file-name top items-alist)
  (string-append
   "\\input texinfo @c -*-texinfo-*-"
   "\n@setfilename " file-name ".info"
   "\n@settitle " name
   "\n@dircategory GNU music project"
   "\n@direntry"
   ;; prepend GNU for dir, must be unique
   "\n* GNU " name " (" file-name ").           " name "."
   "\n@end direntry"
   (node "Top") top
   "\n@top"
   (texi-section 1 name #f)
   (texi-menu items-alist)
   "\n@contents"
   ))

(define (context-name name)
  (string-append "Context " name))

(define (engraver-name name)
  name)

(define (grob-name name)
  (string-append "Grob " name))

(define (interface-name name)
  name)

(define (reffy x)
  (string-append "@ref{" x "}"))

(define (human-listify l)
  (cond
   ((null? l) "none")
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (string-append (car l) " and " (cadr l)))
   (else (string-append (car l) ", " (human-listify (cdr l))))
   ))

(define (writing-wip x)
  (display (string-append "\nWriting " x " ... ") (current-error-port)))
