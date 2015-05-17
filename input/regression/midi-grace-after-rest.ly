\header {
  texidoc = "Grace notes shorten previous notes only if they'd overlap
them. The A should be a full quarter note, but the C should be shortened
to 1/4 - 9/40 * 1/8 = 71/320 (rounded down to 340/384 in MIDI)."
}
\version "2.19.21"
\score {
 \relative {
   a4 r
   \grace b8 c8... r64
   \grace d8 e4
 }
 \midi { }
}
