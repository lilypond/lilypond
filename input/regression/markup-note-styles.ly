\version "2.13.10"

\header {
  texidoc = "@code{\\note-by-number} and @code{\\note} support
all note head styles."
}

#(define-markup-command (show-note-styles layout props) ()
   (interpret-markup layout props
                     (make-column-markup
                      (map
                       (lambda (style)
                         (make-line-markup
                          (list
                           (make-pad-to-box-markup '(0 . 20) '(0 . 0)
                                                   (symbol->string style))
                           (make-override-markup
                            (cons 'line-width 60)
                            (make-override-markup
                             (cons 'style style)
                             (make-fill-line-markup
                              (map
                               (lambda (dur-log)
                                 (make-note-by-number-markup
                                  dur-log 0 UP))
                               '(-3 -2 -1 0 1 2))))))))
                       '(default altdefault
                          baroque neomensural
                          mensural petrucci
                          harmonic harmonic-black
                          harmonic-mixed diamond
                          cross xcircle
                          triangle slash)))))

\markup {
  \override #'(baseline-skip . 6)
  \show-note-styles
}
