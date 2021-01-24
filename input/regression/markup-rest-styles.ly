\version "2.21.0"

\header {
  texidoc = "The markup function @code{\\rest} supports all rest styles."
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
                       (lambda (duration) (make-rest-markup duration))
                       (map
                         (lambda (i) (ly:make-duration i 0))
                         (iota 14 -3 1)))))))))
         ;; `z' is not an established style, we've added it to the list of
         ;; styles, in order to get the "rest.2z" glyph printed as well.
         '(default
           mensural
           neomensural
           classical
           baroque
           altdefault
           petrucci
           blackpetrucci
           semipetrucci
           kievan
           z)))))

\showRestStyles
