\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="The @code{\\fine} command gives @code{fineBarType}
precedence in the selection of repeat bar types where
@code{underlyingRepeatBarType} or @code{sectionBarType} would normally be
used.  Customizing @code{fineBarType} is effective when appropriate
bar lines are defined.  The output should show two adjacent repeated
sections with doubled dots and thick bar lines, followed by a double
thick bar line without dots."
}

\layout {
  ragged-right = ##t

  \context {
    \Score
    fineBarType = "..-test"
    %% N.B. We don't want the "-..-test" annotation on these.
    %% LilyPond should append it at \fine.
    doubleRepeatBarType = "::..::"
    endRepeatBarType = "::.."
    startRepeatBarType = "..::"
  }
}

%% Notice that we define *only* annotated versions of these.
%%
%% For a score with span-bars, "..::" would need to be defined as
%% well, but we don't need to complicate this test with that.
\defineBarLine "..::-..-test" #'("..::" "..::" "") % start repeat
\defineBarLine "::..-..-test" #'("::.." "::.." "") % end repeat
\defineBarLine "::..::-..-test" #'("::..::" "::..::" "") % double repeat
\defineBarLine "..-test" #'(".." ".." "") % fine

%% It is important that these bars are not aligned to measure
%% boundaries.  It shows that LilyPond chooses fineBarType over
%% underlyingRepeatBarType as well as over sectionBarType.
\new Score \fixed c' {
  r2.
  \repeat volta 2 {
    \fine \section r4 | r2.
  }
  \repeat volta 2 {
    \fine \section
    r4 | r2.
    \fine \section
  }
  r4 | r2.
  \fine \section
}
