;;;
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

(define (scm->string val)
  (string-append
   (if (self-evaluating? val) "" "'")
   (texify 
    (call-with-output-string (lambda (port) (display val port))))
  ))

(define (node name)
  (string-append
   "\n@html"
   "\n<hr>"
   "\n@end html"
   "\n@node " name ",,,"))

(define section-alist
  '(
    ;; Hmm, texinfo doesn't have ``part''
    (0 . "@top")
    (1 . "@unnumbered")
    (2 . "@unnumberedsec")
    (3 . "@unnumberedsubsec")
    (4 . "@unnumberedsubsubsec")
    (5 . "@unnumberedsubsubsec")
    ))
    
(define (section level name)
  (string-append "\n" (cdr (assoc level section-alist)) " " name "\n"))
   
(define (description-list items-alist)
  (string-append
   "\n@table @samp\n"
   (apply string-append
	  (map (lambda (x) (string-append "\n@item " (car x) "\n" (cdr x)))
	       items-alist))
   "\n@end table\n"))

(define (texi-menu items-alist)
  (string-append
  "\n@menu"
  (apply string-append
	 (map (lambda (x) (string-append "\n* " (car x) ":: " (cdr x)))
	      items-alist))
  "\n@end menu\n"
  ;; Menus don't appear in html, so we make a list ourselves
  "\n@ifhtml\n"
  (description-list (map (lambda (x) (cons (reffy (car x)) (cdr x)))
			 items-alist))
  "\n@end ifhtml\n"))

  
(define (texi-node-menu name items-alist)
  (string-append
   (node name)
   (section 1 name)
   (texi-menu items-alist)))

(define (texi-file-head name file-name top items-alist)
  (string-append
   "\input texinfo @c -*-texinfo-*-\n"
   "@settitle " name
   "\n@setfilename " file-name ".info"
   (node "Top") top
   "\n@top"
   (section 1 name)
   (texi-menu items-alist)))

(define (context-name name)
  (string-append "Context " name))

(define (engraver-name name)
  name)

(define (element-name name)
  (string-append "Element " name))

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
