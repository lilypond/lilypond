\header {
    texidoc = "Tremolos are positioned a fixed distance from the
    end of the beam. Tremolo flags are shortened and made rectangular
    on beamed notes or on stem-up notes with a flag. Tremolo flags are
    tilted extra on stem-down notes with a flag."
}

\version "2.19.21"
\layout {
    ragged-right = ##T
}

\relative {
  c'16:32 e: f: a: c,4:8 c4:32 c8:16 \noBeam c16:32 \noBeam c16:64 |
  c''16:32 e: f: a: c,4:8 c4:32 c8:16 \noBeam c16:32 \noBeam c16:64 |
}
