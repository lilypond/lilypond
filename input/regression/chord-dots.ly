\version "2.17.13"

\header {
  texidoc =
"Property @code{chord-dots}: If set, remove dots which the
@code{DotColumn} algorithm would vertically position too far away from
note heads."
}

\layout{ ragged-right = ##t }


\relative c'' {
  \override Score.DotColumn.chord-dots = ##f
  << { <d e f g a>4. } \\
     { <a, b c d e>4. } >>

  \override Score.DotColumn.chord-dots = ##t
  << { <d' e f g a>4. } \\
     { <a, b c d e>4. } >>
}
