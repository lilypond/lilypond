\version "2.13.53"

\header {
texidoc="Midi2ly --key works on all staves, this is G major (--key=1)"
options="--key=1"
}

\score {
  <<
    \context Staff = "treble" <<
      \context Voice="one" \relative c'' {
	fis
      }
    >>
    \context Staff = "bass" <<
      \context Voice="two" \relative c {
	\clef bass
	fis
      }
    >>
  >>
  \layout {}
  \midi {}
}
