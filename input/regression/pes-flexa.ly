\header {
  texidoc = "The commands @code{\\pes}, @code{\\flexa}, and @code{\\~} are
identical, setting the grob property @code{pes-or-flexa} to @code{#t}."
}

\version "2.27.1"

\new VaticanaScore {
  \new VaticanaVoice {
    \[ g \pes a \] s
    \[ g \~ a \] s
    \[ a \flexa g \] s
    \[ a \~ g \]
  }
}

\layout {
  line-width = 5\cm
}
