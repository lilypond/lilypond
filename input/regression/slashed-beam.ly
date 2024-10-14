\version "2.25.21"

\header {
  texidoc = "A slash may be added to a beam using @code{beam::slashed-stencil}.
Relying on the @code{details}-subproperty @code{slash-side} the slash is added
at left (default) or at right side of the beam."
}

mus = {
  \cadenzaOn
  c'16^[ a' c'']
  c''^[ a' c']
  c'_[ a' c'']
  c''_[ a' c']
  %% kneed-beams
  \override Beam.auto-knee-gap = 1
  c'''16[  c32]
  c16[  c'''32 c16]
  c'''16[ c32 c''']
  c'''16[  c c'''32]
  c16[  c'''64 c]
  c16[  c''' c64]
  c16[ c c c''']
  c[ c c''' c''']
  c[ c''' c''' c''']
  c'''[ c''' c''' c]
  c'''[ c''' c c]
  c'''[ c c c]
}

\score {
  {
    \override Beam.stencil = #beam::slashed-stencil
    \mus
    \break
    \override Beam.details.slash-side = #RIGHT
    \mus
    \bar "|."
  }
  \layout {
    indent = 0
    \context {
      \Staff
      \omit TimeSignature
    }
  }
}
