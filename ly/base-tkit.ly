%%%% Template toolkit (common functions).
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2015--2022 Trevor Daniels <t.daniels@treda.co.uk>
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

%\version "2.19.22"

%%% These are the general utility functions and storage
%   used by the built-in templates and the template kits
%   (tkits) supporting them.

% TODO: these may be more sensibly (re)defined as a scm file

#(define (get-id str)
   "Return the identifier with the value str"
   (ly:parser-lookup (string->symbol str)))

#(define (make-id a b)
  "Return the identifier formed from concatenating the
   two strings provided as arguments."
   (get-id (string-append a b)))

#(define (cartesian a b)
  "Return a list formed from concatenating every element
   of list a with every element of list b (the cartesian
   product a X b)."
   (append-map
    (lambda (x)
      (map
       (lambda (y)
         (string-append x y))
       b))
    a))

#(define (define-missing-variables! ids)
  "Check if each of the identifiers listed in the argument is
   known to the parser.  If any are not, define them and set
   their value to #f"
   (for-each
      (lambda (id)
        (define sym (string->symbol id))
          (if (null? (ly:parser-lookup sym))
            (ly:parser-define! sym #f)))
      ids))

% Define the lists used to hold the names and
% component names which form the variable names
% used in the templates.  These are populated by the
% set-music-definitions! procedure
% The variables defined here as empty lists will be provided
% by the template, and may be set to any values there.
#(define voice-prefixes '())   % eg "Soprano"
#(define all-music-names '())  % eg "SopranoMusic"
#(define lyrics-postfixes '())	% eg "Lyrics"
#(define lyrics-names '())     % eg "VerseOne"

% Define the derived variables to be populated
#(define all-music-lyrics-names '())  % eg "SopranoLyrics"
#(define AllMusic (make-music 'SequentialMusic 'void #t))
#(define KeepAlive AllMusic)   % used to ensure voices don't terminate
#(define have-music #f)        % -> #t when at least one music name
                                %    contains music
#(define voice-postfixes
   ;; These names are used verbatim in code, so may not be changed
   '("InstrumentName"
     "MidiInstrument"
     "Music"
     "ShortInstrumentName"))

#(define variable-names
   ;; These names are used verbatim in code, so may not be changed
   '("Key"
     "Layout"
     "PianoDynamics"
     "Time"
     "TwoVoicesPerStaff"))

% Define the predicates used in the tkits and templates
#(define (above-or-below? x)
  (member x '("Above" "Below")))

#(define (up-or-down? x)
   (member x '("Down" "Up" "")))

#(define (voice-prefix? x)
   (member x voice-prefixes))

#(define (vocal-lyrics-or-verses? x)
  (or (member x lyrics-postfixes)
      (member x lyrics-names)))


#(define (set-music-definitions! prefixes lyr-postfixes lyr-names)
  "Populate the name definitions and their derivatives
   with the values provided by the calling template"
   (set! voice-prefixes prefixes)
   (append! variable-names lyr-names)
   (set! all-music-names
         (cartesian voice-prefixes '("Music")))
   (set! lyrics-postfixes lyr-postfixes)
   (set! lyrics-names lyr-names)
   (set! all-music-lyrics-names
     (cartesian voice-prefixes (append
                                voice-postfixes
                                lyrics-postfixes)))
   (define-missing-variables! (append
                                  all-music-lyrics-names
                                  variable-names))
   (set! AllMusic
     (make-simultaneous-music
      (filter ly:music?
              (map
               (lambda (x)
                 (get-id x))
               all-music-names))))
   (set! KeepAlive
         (skip-of-length AllMusic))
   (set! have-music
         (ly:moment<?
          (ly:make-moment 0)
          (ly:music-length KeepAlive))))

