\version "2.23.6"
\header {
 texidoc = "Test if cautionary accidentals have the same horizontal
            spacing correction as regular accidentals."
}

\layout { ragged-right = ##t }

{
  \override Score.SpacingSpanner.packed-spacing = ##t
  \override Accidental.parenthesized = ##t
  c'8 cis' c' cis' f' fis' g' f' |
}

{
  \override Score.SpacingSpanner.packed-spacing = ##t
  c'8 cis'? c'? cis'? f' fis'? g' f'? |
}
