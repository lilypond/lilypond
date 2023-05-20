\version "2.25.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "A bar check between two volta-style repeat alternatives
which is not aligned with respect to either one produces a warning.
This test should run with expected warnings only."
}

%% TODO: It would be better if the warning for the first alternative
%% told the position within the first alternative rather than the
%% second alternative; but a warning that needs some explanation is
%% better than no warning.
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s") 1/4)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s") 1/4)

\fixed c' {
  \repeat volta 2 {
    c4
    \alternative {
      { d2 | }
      { e2 }
    }
  }
}

\fixed c' {
  \repeat volta 2 {
    c4
    \alternative {
      { d2 }
      { | e2 }
    }
  }
}
