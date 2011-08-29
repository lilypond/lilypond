%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2010--2011 Carl D. Sorensen <c_sorensen@byu.edu>
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

\version "2.14.0"

%%  A stringTuning is a list of pitches ordered by string number
%%  from 1 to N.
%%  Here we define a number of default string tunings.

%% A scheme function for converting a chord to a string tuning
#(define (chord->tuning parser tuning-symbol chord)
  (let* ((ev-chord (car (extract-named-music chord 'EventChord)))
         (pitches (event-chord-pitches ev-chord)))
    (ly:parser-define! parser tuning-symbol (reverse pitches))))

%% A music function for converting a chord to a string tuning.
%% The music argument for \makeStringTuning must be a chord in
%% absolute mode ordered from the highest string number to the
%% lowest string number

makeStringTuning =
#(define-music-function (parser location tuning chord)
   (symbol? ly:music?)
   (_ "Convert @{chord} to a string tuning stored in @code{tuning}.
@{chord} must be in absolute pitches and should have the highest
string number (generally the lowest pitch) first.  @code{tuning}
should be a string that will be converted to a symbol.")
   (begin
     (chord->tuning parser tuning chord)
     (make-music 'SequentialMusic 'void #t)))


%% A music function for converting a chord to a string tuning
%% and setting the current context stringTunings property to
%% the newly-defined-string tuning.

contextStringTuning =
#(define-music-function (parser location tuning chord)
   (symbol? ly:music?)
   (_ "Convert @{chord} to a string tuning stored in @code{tuning},
and set @code{stringTunings} of the current context to the
newly-defined tuning.
@{chord} must be in absolute pitches and should have the highest
string number (generally the lowest pitch) first.  @code{tuning}
should be a string that will be converted to a symbol.")
  (chord->tuning parser tuning chord)
  #{ \set stringTunings #(ly:parser-lookup $parser $tuning)
  #})

%% A music function for converting an alist to string-tunings
makeDefaultStringTunings =
#(define-music-function (parser location default-tuning-alist)
   (cheap-list?)
   (_ "Define default string tunings for each element of
@code{default-tuning-alist}.")
   (begin
     (for-each (lambda (alist-entry)
                 (chord->tuning parser (car alist-entry) (cdr alist-entry)))
	       default-tuning-alist)
     (make-music 'SequentialMusic 'void #t)))

% tuning definitions require default pitchnames
\languageSaveAndChange #default-language

%% Define alist of default string tunings
defaultStringTunings =
#`(
   ;; guitar tunings
   (guitar-tuning . ,#{<e, a, d g b e'>#})
   (guitar-seven-string-tuning . ,#{<b,, e, a, d g b e'>#})
   (guitar-drop-d-tuning . ,#{<d, a, d g b e'>#})
   (guitar-open-g-tuning . ,#{<d, g, d g b d'>#})
   (guitar-open-d-tuning . ,#{<d, a, d fis a d'>#})
   (guitar-dadgad-tuning . ,#{<d, a, d g a d'>#})
   (guitar-lute-tuning . ,#{<e, a, d fis b e'>#})
   (guitar-asus4-tuning . ,#{<e, a, d e a e'>#})

   ;; bass tunings
   (bass-tuning . ,#{<e,, a,, d, g,>#})
   (bass-four-string-tuning . ,#{<e,, a,, d, g,>#})
   (bass-drop-d-tuning . ,#{<d,, a,, d, g,>#})
   (bass-five-string-tuning . ,#{<b,,, e,, a,, d, g,>#})
   (bass-six-string-tuning . ,#{<b,,, e,, a,, d, g, c>#})

   ;; mandolin tunings
   (mandolin-tuning . ,#{<g d' a' e''>#})

   ;; tunings for 5-string banjo
   (banjo-open-g-tuning . ,#{<g' d g b d'>#})
   (banjo-c-tuning . ,#{<g' c g b d'>#})
   (banjo-modal-tuning . ,#{<g' d g c' d'>#})
   (banjo-open-d-tuning . ,#{<a' d fis a d'>#})
   (banjo-open-dm-tuning . ,#{<a' d fis a d'>#})

   ;; ukulele tunings
   (ukulele-tuning . ,#{<g' c' e' a'>#})
   (ukulele-d-tuning . ,#{<a' d' fis' b'>#})
   (tenor-ukulele-tuning . ,#{<a' e' c' g>#})
   (baritone-ukulele-tuning . ,#{<e' b g d>#})

   ;; orchestral strings
   (violin-tuning . ,#{<g d' a' e''>#})
   (viola-tuning . ,#{<c g d' a'>#})
   (cello-tuning . ,#{<c, g, d a>#})
   (double-bass-tuning . ,#{<e,, a,, d, g,>#})
  )

%% convert 5-string banjo tuning to 4-string by removing the 5th string
#(define-public (four-string-banjo tuning)
   (reverse (cdr (reverse tuning))))

%% make all of the default string tunings

\makeDefaultStringTunings #defaultStringTunings

% restore the language
\languageRestore

