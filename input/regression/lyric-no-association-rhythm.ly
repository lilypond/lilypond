
\header {

  texidoc = "When lyrics are not associated with specific voices, the
  lyric placement should follow lyric rhythms.  In particular, the
  second syllable here should not be attached to the first note of the first staff."

}


\version "2.12.0"
\layout { ragged-right = ##t }

\score {
  \context ChoirStaff <<
    \context Staff = soprano <<
      \context Voice = sop \relative c' { e2 e4  }
    >>
    \context Staff = alto <<
      \context Voice = alt \relative c' { e4  e4  e4  }
      \new Lyrics \lyricmode { 
        do4 re4 me4
      }
    >>
  >>
}
