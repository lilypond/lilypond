\version "2.17.2"

\header {
  texidoc = "Chord tremolos adapt to the presence of accidentals.
"
}

{
  \repeat tremolo 16 { c''32 d'' }
  \repeat tremolo 16 { c''32 <dis''> }
  \repeat tremolo 16 { c''32 <dis'' fis''> }
  \repeat tremolo 8 { c''32 d'' }
  \repeat tremolo 8 { c''32 <dis''> }
  \repeat tremolo 8 { c''32 <dis'' fis''> }
  \repeat tremolo 4 { c''32 d'' }
  \repeat tremolo 4 { c''32 <dis''> }
  \repeat tremolo 16 { b''32 <cis'''> }
}