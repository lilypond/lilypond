\version "2.23.14"

\header {
  texidoc = "Straight flags scale properly at different staff sizes for
@code{TextScript} and music.

The test uses extreme staff size values, scaled to approximately the same
size for better comparison."
}

staffSize = #(define-music-function (new-size) (number?)
  #{
    \set fontSize = #new-size
    \override StaffSymbol.staff-space = #(magstep new-size)
    \override StaffSymbol.thickness = #(magstep new-size)
  #})

notes = {
  \stemDown a'16^\markup \note-by-number #4 #0 #-1
  \stemUp c''16_\markup \note-by-number #4 #0 #1
}

mus = {
  \omit Staff.Clef
  \omit Staff.TimeSignature
  \autoBeamOff

  \override TextScript.flag-style = #'modern-straight-flag
  \override Flag.stencil = #modern-straight-flag
  \notes
  \override TextScript.flag-style = #'old-straight-flag
  \override Flag.stencil = #old-straight-flag
  \notes
  \override TextScript.flag-style = #'flat-flag
  \override Flag.stencil = #flat-flag
  \notes
}

\markup \scale #'(10 . 10)
  \score { \new Staff \with { \staffSize #-16 } \mus }
\markup \scale #'(1.5 . 1.5)
  \score { \new Staff \with { \staffSize #0 } \mus }
\markup \scale #'(0.235 . 0.235)
  \score { \new Staff \with { \staffSize #16 } \mus }

\paper {
  indent = 0
}
