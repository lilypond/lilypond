\version "2.25.4"

\header {
  texidoc="A multi-measure rest implicitly creates a bottom context.
The expected output is a repeated section with one whole-measure rest
in the body and one whole-measure rest in one alternative."
}

#(ly:set-option 'warning-as-error #t)

%% In issue 6571, the initial R1 delayed its descent to a bottom
%% context, so the repeat iterator worked in the wrong context, with
%% the visible result being the lack of an end-repeat bar.  It would
%% be nice to test this behavior of multi-measure rests more directly,
%% since repetition is quite complicated.
\repeat volta 2 {
  R1
  \alternative {
    R1
  }
}
