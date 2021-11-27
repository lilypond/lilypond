\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="Segno and coda marks created automatically by
@code{\\repeat segno} can be manually overridden with
@code{\\segnoMark} and @code{\\codaMark}.  A double segno and double
coda sign should appear."
}

piece = \new Voice \fixed c' {
  f1
  \repeat segno 2 {
    \segnoMark 2
    g1
    \alternative {
      { \codaMark 2 a1 }
      <>
    }
  }
  \sectionLabel "Coda"
  b1
}

\score { \piece }
\score { \unfoldRepeats \piece }
