\version "2.1.26"
\header {
texidoc = "Span bars are drawn only between staff bar lines. By setting 
bar lines to transparent, they are shown only between systems.
"
}

\score {
 \notes \relative c' \new StaffGroup <<
 \new Staff { a1 a1 a1}
  \new Lyrics \lyrics <<
   { bla1 die bla }
   { foo bar foo }
  >>
  \new Staff { f1 f1 f1}
 >>
 \paper {
  \translator {
   \StaffContext
   \override BarLine #'transparent = ##t
  }
  raggedright =##t 
 }
}

