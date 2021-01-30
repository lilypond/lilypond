\version "2.16.0"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (ly:translate-cpp-warning-scheme "Markup depth exceeds maximal value of %d; Markup: %s") 1024 "recursive-explosion-markup")

\header {
  texidoc = "Markups have a maximum depth to prevent non-termination."

}

% A simple markup function that calls itself and increases its argument, so
% it will grow forever, unless we terminate it.
#(define-markup-command (recursive-explosion layout props nr)
  (number?)
  (interpret-markup layout props (make-recursive-explosion-markup (+ nr 1))))

\markup { Test: \recursive-explosion #1 }
