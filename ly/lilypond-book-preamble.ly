
\version "2.10.0"

%% toplevel \book gets output per page,
%% everything else gets output per system/title
#(define default-toplevel-book-handler
  print-book-with-defaults-as-systems )

#(ly:set-option (quote no-point-and-click))
#(define inside-lilypond-book #t)
#(define version-seen #t)
