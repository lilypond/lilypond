\version "2.25.35"

\header {

  texidoc = "
    NoteNames context should be close to the related notes,
    and should not collide with the tempo markings.
  "
}

\paper {
  system-system-spacing.basic-distance = #10 % increase this value for more space
}

notes = \relative {
  c'4 c c c
}

mylyrics = \lyricmode {
  \tempo "Allegro"
  ly -- ric ly -- ric
}

\score {
  <<
    \new Voice = "voice" {
      \*13 \notes
    }
    \context NoteNames  {
      \*13 \notes
    }
    \new Lyrics \lyricsto "voice" {
      \*13 \mylyrics
    }
  >>
}
