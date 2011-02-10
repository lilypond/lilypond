\version "2.13.50"

\header {
  texidoc = "
Tempo ranges are supported.  By default, numbers are
printed with an en-dash character, separated by thin-spaces.
"
}

\relative c'' {
  \tempo 4 = 66 ~ 72
  c1 | c
  #(ly:export
    (make-event-chord (list (make-music 'TempoChangeEvent
                                        'tempo-unit (ly:make-duration 2 0 1 1)
                                        'metronome-count (cons 124 132)))))
  c1 | c
}
