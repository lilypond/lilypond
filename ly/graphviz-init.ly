%%%% Graphing support.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2007--2021 Joe Neeman <joeneeman@gmail.com>
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

#(define (escape-label label)
  ;; to keep escaped "\"" we need to transform it to "\\\""
  ;; otherwise the final pdf-creation will break
  (regexp-substitute/global #f "\"" label 'pre "\\\"" 'post))

#(define (discard arg) '())

#(define default-label-formatting
  ;; Store all settings needed for label formatting.
  ;; The alist keys correspond to the callback type,
  ;; the values are lists of the following structure
  ;; (string-template (prepone preptwo prepthree) postp))
  ;; where `prep...` are functions that preprocess the
  ;; arguments before passing them to the `format` function.
  ;; `postp` is a postprocessing function that is applied afterwards.
  ;; Use the `identity` function to leave arguments unprocessed.
  ;; If the final callback function must (for the sake of compatibility
  ;; with the C++ code) accept more arguments than wanted for
  ;; label formatting, the superfluous arguments may be discarded with
  ;; a function that returns the empty list for every input.
  `((mod . ("~a\\n~a:~a\\n~a <- ~a"
            (,grob::name ,identity ,identity ,discard ,identity ,truncate-value)
            ,escape-label))
    (cache . ("caching ~a.~a\\n~a -> ~a"
              (,grob::name ,identity ,identity ,identity)
              ,identity))
    (create . ("~a\\n~a:~a"
               (,grob::name ,identity ,identity ,discard)
               ,identity))))

#(define (make-label-formatter template-string preprocessors postprocessor)
  (lambda args
    (let* ((preprocessed-args
             (remove null? (map
               ;; Call every function in `preprocessors` with the corresponding
               ;; argument in the argument list
               (lambda (proc arg) (proc arg))
               preprocessors args)))
           (formatted-string
             (apply format #f template-string preprocessed-args))
           (postprocessed-string (postprocessor formatted-string)))
      postprocessed-string)))

#(define (make-all-label-formatters label-formatting)
  (map (lambda (format-spec)
         (let* ((callback-type (car format-spec))
                (callback-spec (cdr format-spec))
                (template-string (first callback-spec))
                (preprocessors (second callback-spec))
                (postprocessor (third callback-spec))
                (formatter (make-label-formatter
                            template-string preprocessors postprocessor)))
           (cons callback-type formatter)))
    label-formatting))

#(define default-label-formatters
   (make-all-label-formatters default-label-formatting))

#(define (make-grob-mod-callback formatter)
   (lambda (grob file line func prop value)
     (if (relevant? grob file line prop)
       (let ((formatted-label (formatter grob file line func prop value)))
         (grob-event-node grob formatted-label file)))))

#(define (make-grob-cache-callback formatter)
   (lambda (grob prop callback value)
     (if (relevant? grob #f #f prop)
       (let ((formatted-label (formatter grob prop callback value)))
         (grob-event-node grob formatted-label #f)))))

#(define (make-grob-create-callback formatter)
   (lambda (grob file line func)
     (let ((formatted-label (formatter grob file line func)))
       (grob-event-node grob formatted-label file))))

#(ly:set-grob-modification-callback (make-grob-mod-callback
                                      (assoc-get
                                        'mod
                                         default-label-formatters)))
#(ly:set-property-cache-callback (make-grob-cache-callback
                                   (assoc-get
                                     'cache
                                      default-label-formatters)))
%#(ly:set-grob-creation-callback (make-grob-create-callback
%                                  (assoc-get
%                                    'create
%                                     default-label-formatters)))
