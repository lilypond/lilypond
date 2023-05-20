\version "2.25.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Music between @code{\\cadenzaOn} and @code{\\cadenzaOff}
does not count toward the length of a measure; however when a cadenza
begins at a measure boundary, bar checks during or immediately after
the cadenza do produce warnings.  This test should run with expected
warnings only."
}

%% Except where noted, each of the following scores should warn once.
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s")  0)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s")  0)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s")  0)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s")  0)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s")  0)
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme "bar check failed at: %s")  0)

%% no anacrusis, first measure begins with cadenza
\new Score { \cadenzaOn c'4 \cadenzaOff | d'1 }

%% anacrusis, first measure begins with cadenza
\new Score { \partial 4 r4 | \cadenzaOn c'4 \cadenzaOff | d'1 }

%% second measure begins with cadenza
\new Score { e'1 | \cadenzaOn c'4 \cadenzaOff | d'1 }

%% bar check during cadenza
\new Score { e'1 | \cadenzaOn c'4 | c'4 \cadenzaOff }

%% cadenza starting at a measure boundary arising from restoring
%% timing properties after a partial first alternative
\new Score {
  \partial 4
  \repeat volta 2 {
    e'4
    \alternative {
      \volta 1 { e'2. }
      \volta 2 { \cadenzaOn c'4 \cadenzaOff | d'1 }
    }
  }
}

%% cadenza as the body of a volta-style repeat, with the first
%% alternative not ending on a measure boundary
\new Score {
  \partial 4
  \repeat volta 2 {
    e'4
    \cadenzaOn c'4 \cadenzaOff
    \alternative {
      \volta 1 { e'2. }
      \volta 2 { | d'1 }
    }
  }
}

%% cadenza as the body of a volta-style repeat, with the first
%% alternative ending on a measure boundary
\new Score {
  \repeat volta 2 {
    \cadenzaOn c'4 \cadenzaOff
    \alternative {
      \volta 1 { a'1 }
      %% TODO: A warning would be nice but the current implementation
      %% can't distinguish this from a valid bar check at the end of
      %% the previous alternative.  A false negative in this case is
      %% preferable to a false positive in the other.
      \volta 2 { | b'1 }
    }
  }
}
