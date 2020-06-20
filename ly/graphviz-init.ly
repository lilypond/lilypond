%%%% Graphing support.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2007--2020 Joe Neeman <joeneeman@gmail.com>
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

\version "2.19.22"

#(use-modules (scm graphviz))

#(use-modules (ice-9 regex))

#(define last-grob-action '())

#(define sym-blacklist '())
#(define sym-whitelist '())

#(define file-line-blacklist '())
#(define file-line-whitelist '())

#(define grob-blacklist '())
#(define grob-whitelist '())

#(define (blacklist-symbol sym)
  (set! sym-blacklist (cons sym sym-blacklist)))

#(define (whitelist-symbol sym)
  (set! sym-whitelist (cons sym sym-whitelist)))

#(define (whitelist-grob sym)
  (set! grob-whitelist (cons sym grob-whitelist)))

#(define graph (make-empty-graph (ly:parser-output-name)))

% an event is relevant if
% (it is on some whitelist or all whitelists are empty)
% and
% (it isn't on any blacklist)

#(define (relevant? grob file line prop)
  (let ((file-line `(,file . ,line)))
   (and
    (or
     (= 0 (length file-line-whitelist) (length sym-whitelist) (length grob-whitelist))
     (memq prop sym-whitelist)
     (memq (grob::name grob) grob-whitelist)
     (member file-line file-line-whitelist))
    (and
     (not (memq prop sym-blacklist))
     (not (memq (grob::name grob) grob-blacklist))
     (not (member file-line file-line-blacklist))))))

#(define (grob-event-node grob label cluster)
  (let ((node-id (add-node graph label cluster))
        (prev (assv grob last-grob-action)))
   (if (pair? prev)
       (add-edge graph (cdr prev) node-id))
   (set! last-grob-action (assv-set! last-grob-action grob node-id))))

#(define (truncate-value val)
  (let ((val-str (format #f "~a" val)))
   (string-take val-str (min 50 (string-length val-str)))))

#(define (grob-mod grob file line func prop val)
  (let* ((val-str (truncate-value val))
         (label (format #f "~a\\n~a:~a\\n~a <- ~a" (grob::name grob) file line prop val-str))
         ;; to keep escaped "\"" we need to transform it to "\\\""
         ;; otherwise the final pdf-creation will break
         (escaped-label
           (regexp-substitute/global #f "\"" label 'pre "\\\"" 'post)))
   (if (relevant? grob file line prop)
       (grob-event-node grob escaped-label file))))

#(define (grob-cache grob prop callback value)
  (let* ((val-str (truncate-value value))
         (label (format #f "caching ~a.~a\\n~a -> ~a" (grob::name grob) prop callback value)))
   (if (relevant? grob #f #f prop)
       (grob-event-node grob label #f))))

#(define (grob-create grob file line func)
  (let ((label (format #f "~a\\n~a:~a" (grob::name grob) file line)))
   (grob-event-node grob label file)))

#(ly:set-grob-modification-callback grob-mod)
#(ly:set-property-cache-callback grob-cache)
%#(ly:set-grob-creation-callback grob-create)
