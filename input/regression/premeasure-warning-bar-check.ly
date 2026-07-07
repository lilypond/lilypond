\version "2.27.2"

\header {
  texidoc = "There is an implied bar check at the end of
@code{\\premeasure}.  The visual output of this test is not important."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s") 1/2)

%% \partial changes the measure position to force the warning
\premeasure { d'4 \partial 8 e'8 f'2 }
