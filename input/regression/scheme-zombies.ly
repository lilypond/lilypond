\header {
  texidoc = "Ensures the zombie check actually works. This should print a log message 'object should be dead' "
}

\version "2.23.1"

#(set! debug-gc-object-lifetimes-test-object (ly:make-prob '() '()))


