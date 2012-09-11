\header {
  texidoc = "You can put lyrics under completion heads."
}
\version "2.16.0"

mel = \relative c'' {
 c1. c1.
}

lyr = \lyricmode {
 One  two
}

\score {
 <<
   \new Staff <<
     \new Voice = "completion" \with {
       \remove "Note_heads_engraver"
       \remove "Forbid_line_break_engraver"
       \consists "Completion_heads_engraver"
     } \mel
   >>
   \new Lyrics \lyricsto "completion" \lyr
 >>
}
