\version "2.21.0"

\header{
  texidoc = "The @code{Completion_heads_engraver} uses dotted
breve/longa durations if possible."
}

\new Voice \with {
  \remove Note_heads_engraver
  \consists Completion_heads_engraver
} {
  \time 7/1
  c'\maxima d'\breve. e'\breve f'\longa g'\breve a'\maxima b'1
}
