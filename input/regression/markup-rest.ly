\version "2.17.9"

\header {
  texidoc = "The rest markup function works for a variety of style, dot and
duration settings."
}

showSimpleRest =
#(define-scheme-function (parser location dots) (string?)
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
                             (lambda (duration)
                                     (make-rest-markup
                                       (if (string? duration)
                                           duration
                                           (string-append
                                             (number->string (expt 2 duration))
                                             dots))))
                             (append
                               '("maxima" "longa" "breve")
                               (iota 8)))))))))
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

showMultiMeasureRests =
#(define-scheme-function (parser location)()
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
                       (cons 'line-width 80)
                       (make-override-markup
                         (cons 'style style)
                         (make-fill-line-markup
                           (map
                             (lambda (duration)
                               (make-line-markup
                                 (list
                                   (make-override-markup
                                      (cons 'multi-measure-rest #t)
                                      (make-rest-markup
                                         (number->string duration))))))
                             (cdr (iota 13)))))))))
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

\markup \column { \bold "Simple Rests" \vspace #0.1 }

\showSimpleRest #"."

\markup \column { \vspace #0.1 \bold "MultiMeasureRests" \vspace #0.1 }

\showMultiMeasureRests
