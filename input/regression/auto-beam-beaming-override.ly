
\header {


  texidoc = "Autobeamer remembers @code{subdivideBeams} and other
beaming pattern related functions at the start of an autobeam."

}


\version "2.17.11"
\paper { ragged-right = ##t }
{
  \time 2/4
  b16 b b b
  b16 b b b
  \set subdivideBeams = ##t
  \set Score.baseMoment = #(ly:make-moment 1/8)
  b16 b b b
  b16 b b b
}
