\version "2.25.35"

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
\relative
{
  \set Staff.connectArpeggios = ##t
  <<
    { r2. <ges' aes c ges'>4\arpeggio | }
    \\
    { \*12 aes,16 <ees aes c>4\arpeggio | }
  >>
}
