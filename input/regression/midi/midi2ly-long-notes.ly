\version "2.25.26"

\header {
  texidoc = "This test creates @code{\\maxima}-equivalent MIDI notes in
different time signatures.  Verifying the MIDI output requires manual
inspection.  After a round-trip through @command{lilypond}, @command{midi2ly},
and @command{lilypond} again, the printed output should show tied notes of the
correct durations."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \fixed c'{
    c1~ | c1~ | c1~ | c1~ | c1~ | c1~ | c1~ | c1 |
    \time 2/1
    d\breve~ | d\breve~ | d\breve~ | d\breve |
    \time 4/1
    e\longa~ | e\longa |
  }

  \midi {}
}
