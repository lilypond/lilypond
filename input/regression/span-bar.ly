\header {
texidoc = "Span bars draw only in between staff bar lines, so setting those to transparent shows bar lines between systems only.
"
}

\score {
 \notes \relative c' \context StaffGroup = groupie <
  \context Staff = SA { c1 c1 c1}
  \context Lyrics = LA \lyrics <
   { bla1 die bla }
  >
  \context Staff = SB { a1 a1 a1}
  \context Lyrics = LB \lyrics <
   { bla1 die bla }
   { foo bar foo }
  >
  \context Staff = SC { f1 f1 f1}
  \context Lyrics = LC \lyrics <
   { bla1 die bla }
   { foo bar foo }
   { foo bar foo }
  >
  \context Staff = SD { d1 d1 d1}
  \context Lyrics = LD \lyrics <
   { bla1 die bla }
   { foo bar foo }
   { foo bar foo }
   { foo bar foo }
  >
 >
 \paper {
  \translator {
   \StaffContext
   BarLine \override #'transparent = ##t
  }
 }
}
