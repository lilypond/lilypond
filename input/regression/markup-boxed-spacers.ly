\version "2.25.23"

\header {
  texidoc = "
Applying the @code{\\box} markup command to @code{\\hspace} and
@code{\\vspace} works without producing a warning.  Contrary to non-spacing
objects, the extents of the spacing command is not changed.

The markups in the first line contain the letter@tie{}x, followed by a boxed
@code{\\hspace} with the shown amount, and the letter@tie{}n; the second
line does the same but uses @code{\\vspace} instead.

The third line uses @code{\\column}, containing the letter@tie{}x, followed
by a boxed @code{\\hspace} with the shown amount, and the letter@tie{}n; the
fourth line does the same but uses @code{\\vspace} instead.

The vertically suprising positioning of the @code{\\vspace} boxes in the
last line is a consequence of how the @code{\\column} markup command is
implemented.
"
}

HH =
#(define-music-function (s) (number?)
   #{ d''1^\markup { #(number->string s) }
          _\markup \override #'(box-padding . 0.5)
             \concat { \with-color #green x
                       \with-color #darkred \box \hspace #s
                       \with-color #blue n } #})

HV =
#(define-music-function (s) (number?)
   #{ d''1^\markup { #(number->string s) }
          _\markup \override #'(box-padding . 0.5)
             \concat { \with-color #green x
                       \with-color #darkred \box \vspace #s
                       \with-color #blue n } #})

VH =
#(define-music-function (s) (number?)
   #{ d''1^\markup { #(number->string s) }
          _\markup \override #'((box-padding . 0.5)
                                (baseline-skip . 0))
             \column { \with-color #green x
                       \with-color #darkred \box \hspace #s
                       \with-color #blue n } #})

VV =
#(define-music-function (s) (number?)
   #{ d''1^\markup { #(number->string s) }
          _\markup \override #'((box-padding . 0.5)
                                (baseline-skip . 0))
             \column { \with-color #green x
                       \with-color #darkred \box \vspace #s
                       \with-color #blue n } #})

\markup \override #'(baseline-skip . 13) \column {
  \score { {
    \HH #-2
    \HH #-1
    \HH #-0.1
    \HH #0
    \HH #0.1
    \HH #1
    \HH #2
  } }
  \score { {
    \HV #-1
    \HV #-0.5
    \HV #-0.1
    \HV #0
    \HV #0.1
    \HV #0.5
    \HV #1
  } }
  \score { {
    \VH #-2
    \VH #-1
    \VH #-0.1
    \VH #0
    \VH #0.1
    \VH #1
    \VH #2
  } }
  \score { {
    \VV #-1
    \VV #-0.5
    \VV #-0.1
    \VV #0
    \VV #0.1
    \VV #0.5
    \VV #1
  } }
  \vspace #1
}
