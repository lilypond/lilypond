\version "2.19.80"

\header {
  texidoc = "Attempting to find a Score context by alias before it
exists triggers creation of a Score context.  The output should have a
note on the middle line of the staff."
}

%% Timing is an alias for Score.
\context Timing \with { middleCPosition = 0 } { c'1 }
