
(define (doc-markup-function func)
  (let*
      (
       (doc-str  (procedure-documentation func) )
       (f-name (symbol->string (procedure-name  func)))
       (sig (object-property func 'markup-signature))
       (sig-str (string-join (map type-name sig) " "))
       )
    
    
       
  (string-append
   "\n\n@b{"
   f-name
   "}\n\n@findex " f-name "\n"
   "\n\n@i{Argument types}: " sig-str
   (if (string? doc-str)
       (string-append
	"\n\n@i{Description}: \n\n"
	doc-str)
       "")
   
   
  )))

(define (markup-function<? a b)
  (string<? (symbol->string (procedure-name a)) (symbol->string (procedure-name b))))

(define (markup-doc-node)
  (make <texi-node>
    #:name "Markup functions"
    #:desc "Definitions of the markup functions."


    #:text (apply string-append
		  
		  "A @code{\\markup} mode command, eg. @code{bold}, is
coupled with a Scheme function (@code{bold-markup}) implementing the
formatting.  For use in Scheme, a function @code{make-bold-markup} is
also defined, which constructs a Markup expression.

This chapter describes all of the @code{...-markup} functions.

"

		  (map doc-markup-function
		       (sort markup-function-list markup-function<?) ))
    ))


