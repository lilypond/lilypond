\header{
  texidoc = "Removing Stem_engraver doesn't cause crashes."
}


\version "2.11.51"

\layout{
 \context{
   \TabStaff
   \override TimeSignature #'stencil = ##f
 }
 \context{
   \TabVoice
   \remove Beam_engraver
   \remove Stem_engraver
   \override TupletBracket #'number-visibility = ##f
 }
}

partition = {
    \times 2/3 { f8 g a }
}


\new TabStaff {
    \partition
}
