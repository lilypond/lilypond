\version "2.19.59"

\header {
  texidoc = "@samp{NullVoice} responds to @code{\\change Staff} as a
@samp{Voice} would.  In consequence, in the first shown system
it keeps a single treble-clef staff alive.  In the second
system, it is in a single bass-clef staff."
}

\layout { ragged-right = ##t }

music = \fixed c' {
  c1
  \break
  \change Staff = "After"
  c
}

\markup
\score-lines {
  \new StaffGroup \with \RemoveAllEmptyStaves
  <<
    \new Staff = "Before" \new NullVoice \music
    \new Staff = "After" {
      \clef bass
      $(skip-of-length music)
    }
  >>
}
