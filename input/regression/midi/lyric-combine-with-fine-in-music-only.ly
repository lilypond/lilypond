\version "2.23.11"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "@code{\\lyricsto} respects @code{\\fine} in the followed
music.  The output should be a whole note with lyric OK and no other
lyric text following."
}

\score {
  <<
    \new Voice = "tune" { c'1 \fine d2 e }
    \new Lyrics \lyricsto "tune" { OK Not good }
  >>
  \midi {}
}
