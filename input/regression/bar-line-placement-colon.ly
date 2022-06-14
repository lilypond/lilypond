\version "2.23.10"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "This test shows the placement of the two-dot bar line
element in various staff configurations."
}

%% \bar ":" isn't predefined
\defineBarLine ":-test" #'(#t #t ":")
testBar = ":-test"
\include "bar-line-placement.ily"
