\version "2.1.29"
\header { texidoc = "@cindex Bar line lyric only
You can move @code{Bar_engraver} and @code{Span_bar_engraver} to 
a different engraving context, if you want, for example, bar lines 
on lyrics. "
}

\score {
\notes \relative c' \context ChoirStaff <<
 \new Staff { c1 c1 c1}
 \context Lyrics \lyrics <<
  { bla1 die bla }
  { foo bar foo }
  { foo bar foo }  
 >>
 \new Staff { c1 c1 c1} 
 >>


\paper  {
	raggedright = ##t
\context {
  \LyricsContext
  \consists Bar_engraver
  % need procedure, since lyrics doesn't have a staff_sym engraver.
  \override BarLine #'bar-size-procedure = #(lambda (x) 3.0)
}
\context {
  \LyricsContext
  \consists "Span_bar_engraver"
}
\context{
  \ChoirStaffContext
  \remove "Span_bar_engraver"
}
\context {
  \StaffContext
  \remove "Bar_engraver"
}
}}

