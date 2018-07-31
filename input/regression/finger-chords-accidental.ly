\version "2.16.0"
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
  <cis''!-3 e''> r4
  <e'-1 bis'> <d'-1 bis'!>
  <bis'! e''-5> <bis'! g''-5>
  <g'-1 bes'> <f'-1 bes'!>
  <bes'! f''-5> <bes'! g''-5>
  <e'-1 bis'-3 e''-5> <d'-1 bis'!-3 g''-5>
  <g'-1 bes'-3 f''-5> <f'-1 bes'!-3 g''-5>
}
