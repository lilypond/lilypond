\version "2.21.2"

\header {
  texidoc = "LilyPond warns about empty scores.  This test should produce
  neither MIDI nor visual output."
}

#(ly:set-option 'warning-as-error #t)
%% once per \layout or \midi
#(ly:expect-warning (_ "no music found in score"))
#(ly:expect-warning (_ "no music found in score"))
#(ly:expect-warning (_ "no music found in score"))
#(ly:expect-warning (_ "no music found in score"))

\score {
  \new Voice { }
  \layout {}
}

\score {
  \new Voice { }
  \midi {}
}

\score {
  \new Voice { }
  \layout {}
  \midi {}
}
