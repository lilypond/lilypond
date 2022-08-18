\version "2.23.13"

\header {
  texidoc = "@code{\\markup \\rhythm} is not affected by switching
off ragged-right globally."
}

\layout { ragged-right = ##f }

{
  \tempo \markup { \rhythm { 8[ 8] } = \rhythm { \tuplet 3/2 { 4 8 } } }
  \repeat unfold 8 { a'8[ 8] }
}
