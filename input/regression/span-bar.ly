#(ly:set-option 'old-relative)
\version "1.9.1"
\header {
texidoc = "Span bars draw only in between staff bar lines, so setting those to transparent shows bar lines between systems only.
"
}

\score {
 \notes \relative c' \context StaffGroup = groupie <
 \context Staff = SB { a1 a1 a1}
  \context Lyrics = LB \lyrics <
   { bla1 die bla }
   { foo bar foo }
  >
  \context Staff = SC { f1 f1 f1}
 >
 \paper {
  \translator {
   \StaffContext
   BarLine \override #'transparent = ##t
  }
  raggedright =##t 
 }
}

