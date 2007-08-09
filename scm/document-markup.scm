;;;; document-markup.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>

(define (doc-markup-function func)
  (let* ((doc-str  (procedure-documentation func))
	 (f-name (symbol->string (procedure-name  func)))
	 (c-name (regexp-substitute/global #f "-markup$" f-name  'pre "" 'post))
	 (sig (object-property func 'markup-signature))
	 (arg-names
	  (map symbol->string 
	       (cddr (cadr (procedure-source func)))))
	 
	 (sig-type-names (map type-name sig))
	 (signature (zip arg-names  sig-type-names))
	 (signature-str
	  (string-join
	   (map (lambda (x) (string-append
			     "@var{" (car x) "} ("  (cadr x) ")" ))
		(zip arg-names  sig-type-names))
	   " " )))
    
    (string-append
     "\n\n@item @code{\\" c-name "} " signature-str
     
     "\n@findex " f-name "\n"
     "\n@cindex @code{" c-name "}\n"
     
     (if (string? doc-str)
	 doc-str
	 ""))))

(define (markup-function<? a b)
  (string<? (symbol->string (procedure-name a)) (symbol->string (procedure-name b))))

(define (markup-doc-string)
  (string-append
   
   "@table @asis"
   (apply string-append
	  
	  (map doc-markup-function
	       (sort markup-function-list markup-function<?)))
   "\n@end table"))

(define (markup-doc-node)
  (make <texi-node>
    #:name "Markup functions"
    #:desc "Definitions of the markup functions."
    #:text (markup-doc-string)))
