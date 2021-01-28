\version "2.21.80"

\header {
  texidoc = "LilyPond does not render zero-duration scores.  This test
should produce neither MIDI nor visual output."
}

#(ly:set-option 'warning-as-error #t)
%% once per \layout or \midi
#(ly:expect-warning (_ "skipping zero-duration score"))
#(ly:expect-warning (_ "to suppress this, consider adding a spacer rest"))

#(ly:expect-warning (_ "skipping zero-duration score"))
#(ly:expect-warning (_ "to suppress this, consider adding a spacer rest"))

#(ly:expect-warning (_ "skipping zero-duration score"))
#(ly:expect-warning (_ "to suppress this, consider adding a spacer rest"))

#(ly:expect-warning (_ "skipping zero-duration score"))
#(ly:expect-warning (_ "to suppress this, consider adding a spacer rest"))

\score {
  { s1*0 }
  \layout {}
}

\score {
  << \new Voice {} >>
  \midi {}
}

\score {
  { \new Voice {} }
  \layout {}
  \midi {}
}
