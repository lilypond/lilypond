\version "2.25.20"

\header {
  texidoc = "An unterminated manual beam generates a warning.  This test should
run with expected warnings only."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "unterminated beam"))

%% These stem-related warnings are probably actually unexpected, but as they
%% exist, we have to ignore them to verify that the "unterminated beam" warning
%% appears.
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "cyclic dependency: calculation-in-progress encountered for %s.%s")
  "Stem" "direction")
#(ly:expect-warning
  (ly:translate-cpp-warning-scheme
   "direction of grob %s must be UP or DOWN; using UP")
  "Stem")

{
  b8[
}
