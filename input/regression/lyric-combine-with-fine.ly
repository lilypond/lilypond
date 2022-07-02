\version "2.23.11"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "@code{\\lyricsto} respects @code{\\fine} when it appears
in the music and also at the corresponding point in the lyrics.  The
output should be a whole note with lyric OK followed by a final bar
line.  No other lyric text should follow."
}

\score {
  <<
    \new Voice = "tune" { c'1 \fine d2 e }
    \new Lyrics \lyricsto "tune" { OK \fine Not good }
  >>
}
