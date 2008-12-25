
\version "2.12.0"

%% toplevel \book gets output per page,
%% everything else gets output per system/title
#(define default-toplevel-book-handler
  print-book-with-defaults-as-systems )

#(define toplevel-book-handler
  (lambda ( . rest)
  (set! output-empty-score-list #f)
  (apply print-book-with-defaults rest)))

#(define toplevel-music-handler
  (lambda ( . rest)
   (apply collect-music-for-book rest)))

#(define toplevel-score-handler
  (lambda ( . rest)
   (apply collect-scores-for-book rest)))

#(define toplevel-text-handler
  (lambda ( . rest)
   (apply collect-scores-for-book rest)))

#(set! output-empty-score-list #t)


#(ly:set-option (quote no-point-and-click))
#(define inside-lilypond-book #t)
#(define version-seen #t)
