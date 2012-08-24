\version "2.16.0"

\header {
  texidoc = "Adding ambitus to percussion contexts does not cause
crashes, since the @code{Ambitus_engraver} will only
acknowledge pitched note heads."
}

\new DrumStaff \with { \consists "Ambitus_engraver" } <<
  \new DrumVoice \drummode { \voiceOne cymr8 cymr }
  \new DrumVoice \drummode { \voiceTwo hhp4 }
>>
