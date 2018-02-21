\version "2.21.0"

\header {
  texidoc = "@code{\\rest-by-number} and @code{\\rest} support
all rest styles."
}

showRestStyles =
#(define-scheme-function ()()
   (make-override-markup
     (cons 'baseline-skip 7)
     (make-column-markup
       (map
         (lambda (style)
           (make-line-markup
             (list
               (make-pad-to-box-markup
                 '(0 . 20) '(0 . 0)
                 (symbol->string style))
               (make-override-markup
                 (cons 'line-width 60)
                 (make-override-markup
                   (cons 'style style)
                   (make-fill-line-markup
                     (map
                       (lambda (dur-log)
                         (make-rest-by-number-markup
                          dur-log 0))
                       '(-3 -2 -1 0 1 2 3 4 5 6 7 8 9 10))))))))
         '(default
           mensural
           neomensural
           classical
           baroque
           altdefault
           petrucci
           blackpetrucci
           semipetrucci
           kievan)))))

\showRestStyles
