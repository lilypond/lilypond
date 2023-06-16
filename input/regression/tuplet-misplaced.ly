\version "2.25.6"

\header {
  texidoc = "A misplaced tuplet (starting while a note
is playing) is handled robustly."
}

#(ly:set-option 'warning-as-error)

%% TODO: emit a warning in such cases.

{
  \time 2/4
  \tuplet 3/2 { c'2. }
}
