\version "2.17.26"

\header {
  texidoc = "Span arpeggios that are not cross-staff do not have
horizontal spacing problems.
"
}


\new Staff
\with
{
  \consists "Span_arpeggio_engraver"
}
\relative c''
{
  \set Staff.connectArpeggios = ##t
  <<
    {r2. <ges aes c ges'>4\arpeggio |}
    \\
    {\repeat unfold 12 aes,16 <ees aes c>4\arpeggio |}
  >>
}
