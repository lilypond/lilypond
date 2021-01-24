\version "2.21.0"

\header {
  texidoc = "The rest markup function works for a variety of style, dot and
duration settings.  Printing symbols for @code{MultiMeasureRest} is supported."
}

%% \rest uses internally either \rest-by-number or \multi-measure-rest-by-number
%% thus those internal commands are tested here under the hood as well.

styles =
#'(default
   mensural
   neomensural
   classical
   baroque
   altdefault
   petrucci
   blackpetrucci
   semipetrucci
   kievan)

showSimpleRest =
#(define-scheme-function (ledgers styles dot-count)
   ((number-list? '(-1 0 1)) symbol-list? index?)
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
                         (make-override-markup
                           (cons 'ledgers ledgers)
                           (make-fill-line-markup
                             (map
                               (lambda (duration) (make-rest-markup duration))
                               (map
                                 (lambda (i) (ly:make-duration i dot-count))
                                 (iota 14 -3 1))))))))))
         styles))))

showMultiMeasureRests =
#(define-scheme-function (styles scale-factors) (symbol-list? number-list?)
   (make-override-markup
     (cons 'baseline-skip 6)
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
                             (lambda (dur-scale)
                               (make-line-markup
                                 (list
                                   (make-override-markup
                                     (cons 'multi-measure-rest #t)
                                     (make-rest-markup
                                       (ly:make-duration 1 0 dur-scale))))))
                             scale-factors)))))))
         styles))))


%% `z' is not an established style, we've added it to the list of styles, in
%% order to get the "rest.2z" glyph printed as well.
\markup
  \column {
    \line { \bold "Simple Rests" by rest-markup }
    \vspace #1
    \showSimpleRest #(append styles '(z)) #1
  }

\markup
  \column {
    \line {
      \bold "MultiMeasureRests" by rest-markup: church-rests and line-style
    }
    \vspace #1
    \underline "church-rests"
    \vspace #1
    \showMultiMeasureRests \styles #(iota 10 1 1)
    \vspace #1
    \underline "line-style"
    \vspace #1
    \showMultiMeasureRests #'(default) #'(11 18 45)
  }
