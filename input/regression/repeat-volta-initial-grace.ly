\version "2.21.6"

\header{
  texidoc="A piece beginning with grace notes followed by a volta
repeat has an opening repeat bar in the expected position."
}

\new Voice {
  \grace f'8 \repeat volta 2 b'1
}
