\version "2.23.14"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="This test exercises bar lines that are overridden in
various built-in @code{Staff} contexts.  Each @code{Staff} is in a
separate @code{\\score}."
}

\include "bar-line-staff-override-music.ily"
\include "divisions-staff-override-staves.ily"

\score { \staffA }
\score { \staffB }
\score { \staffC }
\score { \staffD }
\score { \staffE }
\score { \staffF }
