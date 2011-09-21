\version "2.15.13"

\header{
  texidoc="
Adding material to a tag in sequential and simultaneous expressions
using @code{\\pushToTag} and @code{\\appendToTag}.  One should get the
equivalent of
@example
@{ c' e' g' <<c' e' g' c''>> <<c'' g' e' c'>> g' e' c' @}
@end example
"
}

\layout { ragged-right = ##t }

test = { \tag #'here { \tag #'here <<c''>> }}

{
  \pushToTag #'here \pushToTag #'here \pushToTag #'here \test g' e' c'
  \appendToTag #'here \appendToTag #'here \appendToTag #'here \test g' e' c'
}
