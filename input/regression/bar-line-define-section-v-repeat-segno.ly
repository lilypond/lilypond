\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="The @code{\\section} command gives @code{sectionBarType}
precedence in the selection of repeat bar types where
@code{underlyingRepeatBarType} would normally be used.  Customizing
@code{sectionBarType} is effective when appropriate bar lines are
defined.  The output should show two adjacent repeated sections with
kievan final bar lines outside of the brackets, followed by a kievan
bar line on its own."
}

%% Notice that we define *only* annotated versions of these.
%%
%% For a score with span-bars, "k[" would need to be defined as
%% well, but we don't need to complicate this test with that.
\defineBarLine "k[-." #'("k[" "k[" "") % start repeat
\defineBarLine "]k-." #'("]k" "]k" "") % end repeat
\defineBarLine "]k[-." #'("]k[" "]k[" "") % double repeat
\defineBarLine "k-." #'("k" "k" "") % segno alone

\layout {
  \context {
    \Score
    %% N.B. We don't want the annotation here.  LilyPond should append
    %% it automatically where \section is used.
    doubleRepeatSegnoBarType = "]k["
    endRepeatSegnoBarType = "]k"
    startRepeatSegnoBarType = "k["
    segnoBarType = "k"
    sectionBarType = "."
  }
}

testBars = { \section \inStaffSegno }

\include "bar-line-define-repeat-test.ily"
