\version "1.9.2"

\header {
    texidoc  = "By putting the output of @code{lilypond-version}
  into a lyric, we can print the version number in a score,
or a lilypond-book document."
    }

\score { \context Lyrics \notes {
    \property Score.RehearsalMark \set #'self-alignment-X = #LEFT
    \mark #(ly:export (string-append "Processed with LilyPond version " (lilypond-version)))
    s2
  }
}


