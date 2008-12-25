\version "2.12.0"
\header
{
  texidoc = "Scripts left of a chord avoid accidentals."
}

\paper {
  ragged-right = ##t
}

{
  r4 
  \set fingeringOrientations = #'(left)
  <cis''-3 >
  <cis''-3 e''>
}
