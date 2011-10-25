\version "2.15.15"

\header {
  lsrtags = "rhythms, tweaks-and-overrides"

  texidoc = "
Alternative styles of flag on eighth and shorter notes can be displayed
by overriding the @code{stencil} property of @code{Flag}.  Valid values
are @code{modern-straight-flag} and @code{old-straight-flag}.

"
  doctitle = "Using alternative flag styles"
}

testnotes = {
  \autoBeamOff
  c8 d16 c32 d64 \acciaccatura { c8 } d64 r4
}

\relative c' {
  \time 2/4
  \testnotes

  \override Flag #'stencil = #modern-straight-flag
  \testnotes

  \override Flag #'stencil = #old-straight-flag
  \testnotes

  \revert Flag #'stencil
  \testnotes
}

