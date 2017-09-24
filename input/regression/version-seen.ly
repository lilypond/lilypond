%% \version "2.19.80"

\header {
  texidoc = "This does not produce typeset output but checks that
@code{\\version} statements in included files do not inhibit the
warning in the main file when a @code{\\version} statement is missing
there."
}

\include "deutsch.ly"

#(ly:expect-warning
  (_ "no \\version statement found, please add~afor future compatibility")
  (format #f "\n\n\\version ~s\n\n" (lilypond-version)))
