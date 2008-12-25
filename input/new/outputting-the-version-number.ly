\version "2.12.0"

\header {
  lsrtags = "text"
  texidoc = "
By putting the output of @code{lilypond-version} into lyrics or a
text markup, it is possible to print the version number of LilyPond in
a score, or in a document generated with @code{lilypond-book}.
"
  doctitle = "Outputting the version number"
}

\score {
  \new Lyrics {
    \override Score.RehearsalMark #'self-alignment-X = #LEFT
    \mark #(ly:export (string-append "Processed with LilyPond version "
                       (lilypond-version)))
    s2
  }
}
