\version "2.19.2"
\header {
  texidoc = "Measure repeats may be nested with beat repeats."
}

\paper {
  ragged-right = ##t
}
\relative c'' \context Voice {
  \set Score.skipBars = ##t
  \time 4/4
				% riff
  \repeat "percent" 2 { r8. a16 g8. a16 bes8. a16 f8 d |  a c8 ~ 8 d8 ~ 8 r8 r4 }

  R1*2
  \repeat "percent" 2 { \repeat "percent" 4 { c8 es } }
  R1*2
}

