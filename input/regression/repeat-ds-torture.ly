\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This tests mark formatting and placement for three
sequential @code{\\repeat segno} sections, each with three alternative
endings, with some used for multiple volte."
}

\layout {
  ragged-right = ##t
}

piece = \new Voice \fixed c' {
  \sectionLabel "Commencement"
  s1_"A"
  \repeat segno 7 {
    s1_"B"
    \alternative {
      \volta 1,2 s1_"C"
      \volta 3 s1_"D"
      \volta 5,7,6,4 s1_"E"
    }
  }
  \repeat segno 3 {
    s1_"F"
    \alternative {
      s1_"G"
      s1_"H"
      %% There is no mark conflict here because the coda mark is
      %% suppressed for any alternative with zero duration.
      << \section \sectionLabel "Bridge" s4*0 >>
    }
  }
  s1_"I"
  \repeat segno 3 {
    s1_"J"
    \alternative {
      s1_"K"
      s1_"L"
      <>
    }
  }
  \section \sectionLabel "Fade-out" s1_"M"
  \fine
}

\new Score \with {
  \override JumpScript.direction = #UP % make a mess (spatially)
  \override JumpScript.color = "#500" % improve reviewability
} {
  \new Staff \with { instrumentName = "default" } \piece
}

\new Score \with {
  dalSegnoTextFormatter = #format-dal-segno-text-brief
} {
  \new Staff \with { instrumentName = "brief" } \piece
}
