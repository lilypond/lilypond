\version "2.25.11"

\header {
  texidoc = "The function @code{\\withRelativeDir} helps
@code{\\markup} find input files relative to the input file, without
using @command{lilypond}'s command line option @option{-I}.

To actually test this, @command{lilypond} should be called from
another directory, for example

@example
lilypond other/markup-with-relative-dir.ly
@end example

Note: with the current regression testing infrastructure it is not
possible to check whether the command actually does its job."
}

\markup \epsfile #X #20 \withRelativeDir "lilypond.eps"

\markup \verbatim-file \withRelativeDir "verbatim"
