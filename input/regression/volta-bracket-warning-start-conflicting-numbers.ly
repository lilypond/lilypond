\version "2.25.32"

\header {
  texidoc="A warning is issued when simultaneous @code{\\volta} passages have a
different list of numbers."
}

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "discarding conflicting volta numbers: %s")
  "(1 2)")

<<
  \repeat volta 3 {
    \alternative {
      \volta 2,1 c''1
      \volta 3 c''1
    }
  }
  \\
  \repeat volta 3 {
    \alternative {
      a'1 % implicit \volta 1,2
      a'1 % implicit \volta 3
    }
  }
>>
