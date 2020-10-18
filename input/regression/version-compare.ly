\version "2.23.0"

\header {
  texidoc = "The Scheme function @code{ly:version?} checks that the version of
LilyPond being used satisfies a comparison predicate against a given version."
}

#(if (not (ly:version? = (ly:version)))
  (ly:error "Broken regression test: ly:version? says the current version is
not equal to itself: ~a." (ly:version)))

#(if (not (ly:version? > '(2 20)))
  (ly:error "Broken regression test: ly:version? incorrectly compares versions
of different lengths: ~a and ~a." (ly:version) '(2 20)))
