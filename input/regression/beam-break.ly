
\header {
  texidoc = "Beams can be printed across line breaks, if forced.
"

}
\version "2.19.21"
\layout { ragged-right= ##t }

\relative  {
  \override Score.Beam.breakable = ##t
  \time 3/16 c''16-[ d e \break f-] 
}
