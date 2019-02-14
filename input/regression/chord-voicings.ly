\version "2.21.0"
\header {
    texidoc = "Chord voicings may be transformed or inverted
automatically through Scheme functions.  These work even when
chord notes are not entered in order (e.g. from the lowest to the
uppermost note), and may also be used in chordmode.
Even when using voicings, chord names remain unchanged."
}

ac = \relative c' {
  <c es g bes>2 <d as' f c'>
  \chordmode {c:maj es:maj11+}
}

transforms = {
  <>^\markup "chords"
  \ac
  \bar "||"
  <>^\markup "drop 2"
  \dropNote 2 \ac
  \bar "||"
  <>^\markup "drop 4"
  \dropNote 4 \ac
  \bar "||"
  <>^\markup "drop 2 and 4"
  \dropNote 2 \dropNote 4 \ac
  \bar "||"
  <>^\markup "raise 1"
  \raiseNote 1 \ac
  \bar "||"
  <>^\markup "raise 3"
  \raiseNote 3 \ac
  \bar "||"
  <>^\markup "2nd inversion"
  \invertChords 2 \ac
  \bar "||"
  <>^\markup "\"down\" inversion"
  \invertChords -1 \ac
  \bar "||"
}

<<
  \new ChordNames \transforms
  \new Staff \transforms
>>
