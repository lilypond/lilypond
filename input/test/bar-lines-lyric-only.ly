\version "1.9.2"
\header { texidoc = "@cindex Bar line lyric only
You can move around @code{Bar_engraver} and
@code{Span_bar_engraver} if you want bar lines on lyrics. "
}

\score {
\notes \relative c' \context ChoirStaff <
 \newcontext Staff { c1 c1 c1}
 \context Lyrics \lyrics <
  { bla1 die bla }
  { foo bar foo }
  { foo bar foo }  
 >
 \newcontext Staff { c1 c1 c1} 
 >


\paper  {
	raggedright = ##t
\translator {
  \LyricsVoiceContext
  \consists Bar_engraver
  % need procedure, since lyrics doesn't have a staff_sym engraver.
  BarLine \override #'bar-size-procedure = #(lambda (x) 3.0)
}
\translator {
  \LyricsContext
  \consists "Span_bar_engraver"
}
\translator{
  \ChoirStaffContext
  \remove "Span_bar_engraver"
}
\translator {
  \StaffContext
  \remove "Bar_engraver"
}
}}

