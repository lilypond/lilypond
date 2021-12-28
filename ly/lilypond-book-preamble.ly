%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.16.0"

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
#(ly:set-option 'use-paper-size-for-page #f)
