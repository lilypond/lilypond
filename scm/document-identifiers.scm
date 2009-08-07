;;
;; document-identifiers.scm -- implement documentation
;; of identifiers from .ly file.
;;
;; source file of the GNU LilyPond music typesetter
;;
;; (c) 2006 Han-Wen Nienhuys <hanwen@lilypond.org>
;;

(use-modules (ice-9 format))

(define (document-music-function music-func-pair)
  (let*
      ((name-sym (car music-func-pair))
       (music-func (cdr music-func-pair))
       (func (ly:music-function-extract music-func))
       (arg-names
	(map symbol->string
	     (cddr (cadr (procedure-source func)))))
       (doc (procedure-documentation func))
       (sign (object-property func 'music-function-signature))
       (type-names (map type-name sign))

       (signature-str
	(string-join
	 (map (lambda (x) (format "@var{~a} (~a)"
				  (car x)
				  (cadr x)))
	      (zip arg-names type-names)))))
    (format
     "@item @code{~a}~a~a
@findex ~a
~a
"
     name-sym (if (equal? "" signature-str) "" " - ") signature-str
     name-sym
     (if doc doc "(undocumented; fixme)"))))


(define (document-object obj-pair)
  (cond
   ((ly:music-function? (cdr obj-pair))
    (document-music-function obj-pair))
   (else
    #f)))


(define (identifier<? a b)
  (ly:string-ci<?
   (symbol->string (car a))
   (symbol->string (car b))))


(define-public (identifiers-doc-string)
  (format
   "@table @asis
~a
@end table
"
   (string-join
    (filter
     identity
     (map
      document-object
      (sort
       (ly:module->alist (current-module))
       identifier<?)))
    "")))
