\header {

texidoc = "You can move around Bar_engraver and
Span_bar_engraver if you want bar lines on lyrics."  }


\score {
\notes \relative c' \context StaffGroup = groupie <
 \context Staff = SA { c1 c1 c1}
 \context Lyrics \lyrics <
  { bla1 die bla }
  { foo bar foo }
  { foo bar foo }  
 >
 \context Staff = SB { c1 c1 c1} 

 > 


\paper  {
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
  \StaffGroupContext
  \remove "Span_bar_engraver"
}
}}
