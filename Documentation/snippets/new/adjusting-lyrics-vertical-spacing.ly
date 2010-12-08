\version "2.13.42"

\header {
  lsrtags = "text, vocal-music, spacing"
  texidoc = "
This snippet shows how to bring the lyrics line closer to the staff.

"
  doctitle = "Adjusting lyrics vertical spacing"
}

% Default layout:
<<
  \new Staff \new Voice = melody \relative c' {
    c4 d e f
    g4 f e d
    c1
  }
  \new Lyrics \lyricsto melody { aa aa aa aa aa aa aa aa aa }

  % Reducing the minimum space below the staff and above the lyrics:
  \new Staff {
    \new Voice = melody \relative c' {
      c4 d e f
      g4 f e d
      c1
    }
  }
  \new Lyrics \with {
    \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
  }
  \lyricsto melody { aa aa aa aa aa aa aa aa aa }
>>
