\version "2.19.21"

\header {
texidoc="Midi2ly --key works on all staves, this is G major (--key=1)"
options="--key=1"
}

\score {
  <<
    \context Staff = "treble" <<
      \context Voice="one" \relative {
	fis''
      }
    >>
    \context Staff = "bass" <<
      \context Voice="two" \relative {
	\clef bass
	fis
      }
    >>
  >>
  \layout {}
  \midi {}
}
