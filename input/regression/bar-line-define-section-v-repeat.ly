\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="The @code{\\section} command gives @code{sectionBarType}
precedence in the selection of repeat bar types where
@code{underlyingRepeatBarType} would normally be used.  Customizing
@code{sectionBarType} is effective when appropriate bar lines are
defined.  The output should show two adjacent repeated sections with
dots outside brackets, followed by a single thick bar line."
}

%% Notice that we define *only* annotated versions of these.
%%
%% For a score with span-bars, ";[" would need to be defined as
%% well, but we don't need to complicate this test with that.
\defineBarLine ";[-.-test" #'(";[" ";[" "") % start repeat
\defineBarLine "];-.-test" #'("];" "];" "") % end repeat
\defineBarLine "];[-.-test" #'("];[" "];[" "") % double repeat
\defineBarLine ".-test" #'("." "." "") % section

\layout {
  \context {
    \Score
    %% N.B. We don't want the annotation here.  LilyPond should append
    %% it automatically where \section is used.
    doubleRepeatBarType = "];["
    endRepeatBarType = "];"
    startRepeatBarType = ";["
    sectionBarType = ".-test"
  }
}

testBars = \section

\include "bar-line-define-repeat-test.ily"
