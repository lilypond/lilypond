\version "2.25.32"

\header {
  texidoc = "Mid-measure @code{\\polymetric \\time} commands not accompanied by
@code{\\partial} generate warnings."
}

#(ly:set-option 'warning-as-error #t)

testMusic = \new Staff {
  %% MIDI file format supports only a global time signature.  There is no
  %% performer listening for \polymetric \time, so there is no warning.
  \tag "layout" {
    \applyContext
    #(lambda (context)
      (ly:expect-warning (G_ "mid-measure time signature without \\partial")))
  }

  c'2 \context Staff \polymetric \time 4/4 c'2

  \applyContext #(lambda (context) (ly:check-expected-warnings))
}

\score {
  \layout {}
  \keepWithTag "layout" \testMusic
}

\score {
  \midi {}
  \keepWithTag "midi" \testMusic
}
