\version "2.21.0"

\header {
  texidoc = "The @code{'style} property from grobs such as
@code{TimeSignature} and @code{TextSpanner} does not affect
the default note head style for @code{\\note} and
@code{\\note-by-number}."
}

\relative c' {
  \override Staff.TimeSignature.stencil =
    #(lambda (grob)
       (grob-interpret-markup grob
                              (markup #:override '(baseline-skip . 0)
                                      #:column (#:number "2" #:note (ly:make-duration 1 0) DOWN))))
  \override TextSpanner.bound-details.left.text =
    \markup { \note {16} #UP }
  c1\startTextSpan
  c1\stopTextSpan
}
