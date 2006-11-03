\header {
  texidoc = "Horizontal brackets connect over line breaks."

}
\version "2.9.26"
\paper { ragged-right  = ##t }

\new Voice \with {
  \consists Horizontal_bracket_engraver }
{
  c1\startGroup \break c1 \stopGroup
}
