
\version "2.10.0"

\score {
  \new DrumStaff <<
    \drummode {
      bd4 sn4 bd4 sn4
      <<
	{\voiceOne \repeat unfold 16 hh16 }
	\new DrumVoice { \voiceTwo bd4 sn4 bd4 sn4 }
      >> \oneVoice
    }
  >>
  \layout {}
  \midi {}
}
