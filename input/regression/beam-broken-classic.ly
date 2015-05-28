\version "2.19.21"

\header {
  texidoc="Some classic examples of broken beams, all taken from
Scriabin Op. 11, No. 1.
"
}

\paper {
  ragged-right = ##t
}

music = \relative {
  \override Beam.breakable = ##t
  r2. f''8[ c \break
  e c f,] r8 r4 a'8[ e \break
  g d g,] r8 r4 f'8[ a, \break
  e' g, bes] r8 r4 <a' a,>8 [ d, \break
  <g g,> d g,] r8 r4 <d' d,>8[ a \break
  <c c,> g d] r8 r2
  \clef bass
  r2. d,,8[ d' \break
  a'-4 d a] r8 r4 cis,,8[ cis' \break
  bes' e g] r8 r4 g,,,8[ g' \break
  f' b d ] r8 r2 |
}

\markup { "\override Beam.positions = #beam::place-broken-parts-individually (default)" }
{ \music }

\markup { "\override Beam.positions = #beam::align-with-broken-parts" }
\markup { \justify { Returns y-positions at the ends of the beam such that beams align-across-breaks. } }
{
  \override Beam.positions = #beam::align-with-broken-parts
  \music
}

\markup { "\override Beam.positions = #beam::slope-like-broken-parts" }
\markup { \justify { Approximates broken beam positioning in turn-of-the-century Editions Peters scores. } }
{
  \override Beam.positions = #beam::slope-like-broken-parts
  \music
}
