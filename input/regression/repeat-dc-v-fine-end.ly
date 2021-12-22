\version "2.23.6"

\header {
  texidoc="By default, @code{\\fine} does not create a @emph{Fine}
instruction at the written end of the music, so there is no conflict
when a simultaneous @emph{D.C.} instruction must appear there."
}

#(ly:set-option 'warning-as-error #t)

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    s1
  }
  \fine
}

\score { \piece }
\score { \unfoldRepeats \piece }
