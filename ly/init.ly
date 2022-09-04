%% Toplevel initialisation file.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%                          Jan Nieuwenhuizen <janneke@gnu.org>
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

\version "2.23.2"

#(use-modules (lily curried-definitions))

#(session-replay)
#(note-names-language default-language)
#(reset-stencil-colors)

#(define toplevel-scores (list))
#(define toplevel-bookparts (list))
#(define $defaultheader #f)
#(define $current-book #f)
#(define $current-bookpart #f)
#(define version-seen #f)
#(define expect-error #f)
#(define output-empty-score-list #f)
#(define output-suffix #f)

#(use-modules (lily clip-region))
#(use-modules (srfi srfi-1))
#(use-modules (ice-9 pretty-print))

$(if (ly:get-option 'include-settings)
  (ly:parser-include-string
    (format #f "\\include \"~a\"" (ly:get-option 'include-settings))))

\maininput
%% there is a problem at the end of the input file

%%
%% Above and below comments compensate for the parser's look-ahead.
%%

#(cond
  ;; FIXME: unclear what this is for --JeanAS
  ((not (defined? 'input-file-name)))

  ((not version-seen)
   (version-not-seen-message input-file-name))

  ((ly:parser-has-error?)
   (suggest-convert-ly-message version-seen)))

#(ly:set-option 'protected-scheme-parsing #f)

#(let ((book-handler (if (defined? 'default-toplevel-book-handler)
                         default-toplevel-book-handler
                         toplevel-book-handler)))
   (cond ((pair? toplevel-bookparts)
          (let ((book (ly:make-book $defaultpaper $defaultheader)))
            (for-each (lambda (part)
                        (ly:book-add-bookpart! book part))
                      (reverse! toplevel-bookparts))
            (set! toplevel-bookparts (list))
            ;; if scores have been defined after the last explicit \bookpart:
            (if (pair? toplevel-scores)
                (for-each (lambda (score)
                            (ly:book-add-score! book score))
                          (reverse! toplevel-scores)))
            (set! toplevel-scores (list))
            (book-handler book)))
         ((or (pair? toplevel-scores) output-empty-score-list)
          (let ((book (apply ly:make-book $defaultpaper
                             $defaultheader toplevel-scores)))
            (set! toplevel-scores (list))
            (book-handler book)))))

#(if (eq? expect-error (ly:parser-has-error?))
  (ly:parser-clear-error)
  (if expect-error
   (ly:parser-error (G_ "expected error, but none found"))))

#(if (ly:get-option 'verbose)
  (begin
   (display "gc time taken: ")
   (display (* 1.0 (/ (cdr (assoc 'gc-time-taken (gc-stats))) internal-time-units-per-second)))
   (display "\n")))
