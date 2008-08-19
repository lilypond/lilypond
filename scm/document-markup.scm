;;;; document-markup.scm -- part of generated backend documentation
;;;;
;;;;  source file of the GNU LilyPond music typesetter
;;;; 
;;;; (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>


(define (doc-markup-function-properties func)
  (let ((properties (hashq-ref markup-functions-properties func))
        (prop-strings (list)))
    (for-each (lambda (prop-spec)
                (set! prop-strings
                      (if (list? prop-spec)
                          ;; either (prop value) or (prop)
                          (cons (if (null? (cdr prop-spec))
                                    (format #f "@item @code{~a}\n" (car prop-spec))
                                    (format #f "@item @code{~a} (~a)\n"
                                            (car prop-spec)
                                            (let ((default (cadr prop-spec)))
                                              (if (and (list? default)
                                                       (null? default))
                                                  "'()"
                                                  default))))
                                prop-strings)
                          ;; a markup command: get its properties
                          ;; FIXME: avoid cyclical references
                          (append (doc-markup-function-properties prop-spec)
                                  prop-strings))))
              (or properties (list)))
    prop-strings))

(define (doc-markup-function func)
  (let* ((doc-str  (procedure-documentation func))
         (f-name (symbol->string (procedure-name  func)))
         (c-name (regexp-substitute/global #f "-markup(-list)?$" f-name  'pre "" 'post))
         (sig (object-property func 'markup-signature))
         (arg-names (let ((arg-list (cadr (procedure-source func))))
                      (if (list? arg-list)
                          (map symbol->string (cddr arg-list))
                          (make-list (length sig) "arg"))))
         (sig-type-names (map type-name sig))
         (signature-str
          (string-join
           (map (lambda (x) (string-append
                             "@var{" (car x) "} ("  (cadr x) ")" ))
                (zip arg-names  sig-type-names))
           " " )))
    
    (string-append
     "\n\n@item @code{\\" c-name "} " signature-str
     "\n@funindex \\" c-name "\n"
     "\n@cindex \\" c-name "\n"    
     (if (string? doc-str)
         doc-str
         "")
     (let ((prop-strings (doc-markup-function-properties func)))
       (if (null? prop-strings)
           "\n"
           (string-append "\n\n\nUsed properties:\n@itemize\n"
                          (apply string-append prop-strings)
                          "@end itemize\n"))))))

(define (markup-function<? a b)
  (string<? (symbol->string (procedure-name a)) (symbol->string (procedure-name b))))
 
(define (markup-category-doc-node category)
  (let* ((category-string (symbol->string category))
         (category-name (string-capitalize (regexp-substitute/global #f
                                        "-" category-string 'pre " " 'post)))
        (markup-functions (hashq-ref markup-functions-by-category
                                          category)))
    (make <texi-node>
      #:name category-name
      #:desc ""
      #:text (string-append
              "@table @asis"
              (apply string-append
                     (map doc-markup-function
                          (sort markup-functions markup-function<?)))
              "\n@end table"))))

(define (markup-list-doc-string)
  (string-append
   "@table @asis"
   (apply string-append
          (map doc-markup-function
               (sort markup-list-function-list markup-function<?)))
   "\n@end table"))

(define (markup-doc-node)
  (make <texi-node>
    #:name "Text markup commands"
    #:desc ""
    #:text "The following commands can all be used inside @code{\\markup @{ @}}."
    #:children (let* (;; when a new category is defined, update `ordered-categories'
                      (ordered-categories '(font align graphic music instrument-specific-markup other))
                      (raw-categories (hash-fold (lambda (category functions categories)
                                                   (cons category categories))
                                                 (list)
                                                 markup-functions-by-category))
                      (categories (append ordered-categories
                                          (filter (lambda (cat)
                                                    (not (memq cat ordered-categories)))
                                                  raw-categories))))
                 (map markup-category-doc-node categories))))

(define (markup-list-doc-node)
  (make <texi-node>
    #:name "Text markup list commands"
    #:desc ""
    #:text (string-append
            "The following commands can all be used with @code{\\markuplines}.\n"
            (markup-list-doc-string))))
