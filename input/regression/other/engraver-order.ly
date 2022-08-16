\version "2.23.13"

\header {
  texidoc = "Engravers are run in the same order as they
are specified (although it is generally preferred that the
order does not matter).

This tests contains several scores, which output ``Engraver 1
ran'' and ``Engraver 2 ran'' in the log file.  The order between
these two lines in each score is what matters."
}

#(define (Engraver_1 context)
   (make-engraver
    ((initialize engraver)
     (ly:message "Engraver 1 ran"))))

#(define (Engraver_2 context)
   (make-engraver
    ((initialize engraver)
     (ly:message "Engraver 2 ran"))))

% Engraver 1, then engraver 2

\score {
  \layout {
    \context {
      \Staff
      \consists #Engraver_1
      \consists #Engraver_2
    }
  }
  { c' }
}

% Same as above, testing this in \with
\new Staff \with {
  \consists #Engraver_1
  \consists #Engraver_2
}
{ c' }

% When there are duplicates, the last \consists is retained.
% Thus, this should be engraver 2, then engraver 1
\new Staff \with {
  \consists #Engraver_1
  \consists #Engraver_2
  \consists #Engraver_1
}
{ c' }

% A more complex case.  Engraver_2 is supposed to come before
% Engraver_1 because the first \remove #Engraver_1 removes
% Engraver_1, and then it's added back after Engraver_2.
\score {
  \layout {
    \context {
      \Staff
      \consists #Engraver_1
    }
  }
  \new Staff \with {
    \remove #Engraver_1
    \consists #Engraver_2
    \consists #Engraver_1
  }
  { c' }
}
