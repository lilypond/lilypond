\version "2.15.12"
#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "Markups have a maximum depth to prevent non-termination."

}

% A simple markup function that calls itself and increases its argument, so
% it will grow forever, unless we terminate it.
#(define-markup-command (recursive-explosion layout props nr)
  (number?)
  (interpret-markup layout props (make-recursive-explosion-markup (+ nr 1))))

\markup { Test: \recursive-explosion #1 }
