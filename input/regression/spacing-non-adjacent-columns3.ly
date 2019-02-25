\version "2.21.0"
\header {
  texidoc = "The spacing engine avoids collisions between non-adjacent columns."
}

\paper{ragged-right = ##t }
<<
  \new ChordNames \chordmode {
    \set additionalPitchPrefix = "add"
    f2:13.15 f2
  }
  \new Staff {
    f'4 f'8 f' f'2 \break
    \set fingeringOrientations = #'(left)
    \grace { <d-1-2-3>8[ f a] }
    <fis''-1-2-3-4-5>4 r4
    \set fingeringOrientations = #'(right)
    <g'-1-2-3-4-5>4 \grace { d''8 e'' d''}
    g'4
  }
>>
