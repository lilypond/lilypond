\version "2.1.30"
\header {
texidoc = "Span bars are drawn only between staff bar lines. By setting 
bar lines to transparent, they are shown only between systems.

Setting @code{SpanBar} transparent does the removes the barlines
between systems.
 
"
}

\score {
 \notes \relative c' \new StaffGroup <<
 \new Staff {
     \override Score.BarLine #'transparent = ##t
     a1 a1
     \revert Score.BarLine #'transparent = ##t
     \overr Score.SpanBar #'transparent = ##t
     a1 a1}
  \new Lyrics \lyrics <<
   { bla1 die bla }
   { foo bar foo }
  >>
  \new Staff {
      f1 f1 f1 f1}
 >>
 \paper {
  \context {
   \StaffContext
  }
  raggedright =##t 
 }
}

