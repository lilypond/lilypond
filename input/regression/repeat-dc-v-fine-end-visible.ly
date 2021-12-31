\version "2.23.6"

\header {
  texidoc="By default, @code{\\fine} does not create a @emph{Fine}
instruction at the written end of the music, but this can be changed
with the @code{finalFineTextVisibility} context property.  There is no
warning when a simultaneous @emph{D.C.} instruction must appear
there."
}

#(ly:set-option 'warning-as-error #t)

piece = \new Voice \fixed c' {
  \repeat segno 2 {
    s1
  }
  \once \set Score.finalFineTextVisibility = ##t
  \fine
}

\score { \piece }
\score { \unfoldRepeats \piece }
