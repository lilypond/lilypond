\version "2.16.0"
#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "Cyclic markup detected: %s") 'cycle-markup)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "Cyclic markup detected: %s") 'cycleI-markup)

\header {
  texidoc = "Cyclic markup definitions should cause a warning, but
not crash LilyPond with an endless loop"
}

% A simple markup function that calls itself in a loop.
#(define-markup-command (cycle layout props m)
  (markup?)
  (interpret-markup layout props (make-cycle-markup m)))

% Two simple markup functions that call each other in a loop.
#(define-markup-command (cycleI layout props m)
  (markup?)
  (interpret-markup layout props (make-cycleII-markup m)))
#(define-markup-command (cycleII layout props m)
  (markup?)
  (interpret-markup layout props (make-cycleI-markup m)))


\markup { \cycle "a" }
\markup { \cycleI "a" }
