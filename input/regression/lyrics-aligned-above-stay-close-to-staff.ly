\version "2.19.21"

\header {
  title = "Aligned-above lyrics should stay close to their staff"
  texidoc = "Lyrics aligned above a context should stay close to that
context when stretching.  The Bass I lyric line stays with the
Bass staff.
"
}

\paper { ragged-last-bottom = ##f }

tune = \relative { \repeat unfold 2 { c4( e) g2 | \break c1 }
\bar "|."  }

\score {

  \context ChoirStaff <<

    \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = tenori { \voiceOne \tune }
      \new Voice = tenorii { \voiceTwo \tune }
    >>
    \new Staff = basses <<
      \clef "bass"
      \new Voice = bassi { \voiceOne \tune }
      \new Voice = bassii { \voiceTwo \tune }
    >>

    \new Lyrics \with {alignAboveContext=tenors} \lyricsto tenori {
      Te -- nor one!  A -- _ bove!
    }
    \new Lyrics \with {alignBelowContext=tenors} \lyricsto tenorii {
      Te -- nor two!  Be -- _ low!
    }
    \new Lyrics \with {alignAboveContext=basses}  \lyricsto bassi {
      Bas -- ses one!  A -- _ bove!
    }
    \new Lyrics \with {alignBelowContext=basses} \lyricsto bassii {
      Bas -- ses two!  Be -- _ low!
    }
  >>
  \layout {}
 }
