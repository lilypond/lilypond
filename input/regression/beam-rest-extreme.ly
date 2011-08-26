\version "2.15.9"

\header {
  texidoc = "Beamed rests are given a pure height approximation
that gets their spacing correct in the majority of circumstances.
"
}

\relative c'' {
  <f b c f>16[ r <f bes c f> <f b c f>]
  <f b c f>16[ r <f'' bes c f> <f b c f>]
  <f b c f>16[ r <f,, bes c f> <f b c f>]
  <f'' b c f>16[ r <f bes c f> <f b c f>]
}
