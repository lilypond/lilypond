\version "2.16.0"

\header {
  texidoc = "If @code{ignoreMelismata} is set, lyrics should remain
center-aligned.
"
}

notes = \relative c' {
  \time 2/4
  \slurDashed
  c8 e d( f)
}

lyricsI = \lyricmode {
  \set ignoreMelismata = ##t
  One two three four
}

lyricsII = \lyricmode {
  One two Whee! __
}

<<
  \new Voice = "melody" \notes
  \new Lyrics \lyricsto "melody" \lyricsI
  \new Lyrics \lyricsto "melody" \lyricsII
>>

