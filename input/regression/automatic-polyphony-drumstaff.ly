\version "2.16.0"

\header{ texidoc = "In a DrumStaff, automatic polyphony can be used without
                    explicitly initializing separate voices."
       }

\score {
  \new DrumStaff {
    \drummode {
      bd4 sn4 bd4 sn4
      << { \repeat unfold 16 hh16 } \\ { bd4 sn4 bd4 sn4 } >>
      bd4 sn4 bd4 sn4
    }
  }
}
