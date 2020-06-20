\version "2.21.2"

\header {
  texidoc = "Fingerings don't segfault when their stencil is set to #f.
"
}

%% This regtest compiles without errors.
#(define orig-setting (ly:get-option 'warning-as-error))
#(ly:set-option 'warning-as-error #t)

\markup {
  \score {
    {
      \set fingeringOrientations = #'(right)
      \omit Fingering
      <g'-2 c''-4 >
    }
  }
  \score {
    {
      \set strokeFingerOrientations = #'(right)
      \omit StrokeFinger
      <g'\rightHandFinger #2 c''\rightHandFinger #4 >
    }
  }
  \score {
    {
      \set stringNumberOrientations = #'(right)
      \omit StringNumber
      <g'\2 c''\4 >
    }
  }
}

#(ly:set-option 'warning-as-error orig-setting)
