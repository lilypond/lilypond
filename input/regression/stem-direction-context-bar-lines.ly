\version "2.20.0"

\header {
  texidoc = "The @code{Melody_engraver} decides stem direction for
notes on the middle staff line based on neighboring notes.
Mid-measure repeat bar lines break up the melody as do normal measure
bar lines.  In this test, marcato marks show the expected stem
direction."
}

\new Voice \with {
  \consists "Melody_engraver"
} \relative c'' {
  a2 \repeat volta 2 { b4_^ c | c2 } b4^^ a
}
