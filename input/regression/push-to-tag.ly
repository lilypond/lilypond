\version "2.23.3"

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
testb = { \tag #'here { \tag #'here <c''> }}

{
  \pushToTag #'here c'
  \pushToTag #'here e'
  \pushToTag #'here g' \test
  \appendToTag #'here c'
  \appendToTag #'here e'
  \appendToTag #'here g' \testb
}

