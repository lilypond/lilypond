%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2010--2022 Carl D. Sorensen <c_sorensen@byu.edu>
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

%%  A stringTuning is a list of pitches ordered by string number
%%  from 1 to N.
%%  Here we define a number of default string tunings.

%% A music function for converting a chord to a string tuning.
%% The music argument for \makeStringTuning must be a chord in
%% absolute mode ordered from the highest string number to the
%% lowest string number

stringTuning =
#(define-scheme-function (chord)
   (ly:music?)
   (_i "Convert @var{chord} to a string tuning.
@var{chord} must be in absolute pitches and should have the highest
string number (generally the lowest pitch) first.")
   (let* ((ev-chord (car (extract-named-music chord 'EventChord))))
     (reverse! (event-chord-pitches ev-chord))))

defaultStringTunings = #'()

makeDefaultStringTuning =
#(define-void-function (symbol pitches) (symbol? list?)
   (_i "This defines a string tuning @var{symbol} via a list of @var{pitches}.
The @var{symbol} also gets registered in @code{defaultStringTunings}
for documentation purposes.")
   (ly:parser-define! symbol pitches)
   (set! defaultStringTunings (acons symbol pitches defaultStringTunings)))

%% guitar tunings
\makeDefaultStringTuning #'guitar-tuning \stringTuning <e, a, d g b e'>
\makeDefaultStringTuning #'guitar-seven-string-tuning \stringTuning <b,, e, a, d g b e'>
\makeDefaultStringTuning #'guitar-drop-d-tuning \stringTuning <d, a, d g b e'>
\makeDefaultStringTuning #'guitar-drop-c-tuning \stringTuning <c, g, c f a d'>
\makeDefaultStringTuning #'guitar-open-g-tuning \stringTuning <d, g, d g b d'>
\makeDefaultStringTuning #'guitar-open-d-tuning \stringTuning <d, a, d fis a d'>
\makeDefaultStringTuning #'guitar-dadgad-tuning \stringTuning <d, a, d g a d'>
\makeDefaultStringTuning #'guitar-lute-tuning \stringTuning <e, a, d fis b e'>
\makeDefaultStringTuning #'guitar-asus4-tuning \stringTuning <e, a, d e a e'>

%% bass tunings
\makeDefaultStringTuning #'bass-tuning \stringTuning <e,, a,, d, g,>
\makeDefaultStringTuning #'bass-four-string-tuning \stringTuning <e,, a,, d, g,>
\makeDefaultStringTuning #'bass-drop-d-tuning \stringTuning <d,, a,, d, g,>
\makeDefaultStringTuning #'bass-five-string-tuning \stringTuning <b,,, e,, a,, d, g,>
\makeDefaultStringTuning #'bass-six-string-tuning \stringTuning <b,,, e,, a,, d, g, c>

%% mandolin tunings
\makeDefaultStringTuning #'mandolin-tuning \stringTuning <g d' a' e''>

%% tunings for 5-string banjo
\makeDefaultStringTuning #'banjo-open-g-tuning \stringTuning <g' d g b d'>
\makeDefaultStringTuning #'banjo-c-tuning \stringTuning <g' c g b d'>
\makeDefaultStringTuning #'banjo-modal-tuning \stringTuning <g' d g c' d'>
\makeDefaultStringTuning #'banjo-open-d-tuning \stringTuning <a' d fis a d'>
\makeDefaultStringTuning #'banjo-open-dm-tuning \stringTuning <a' d f a d'>
\makeDefaultStringTuning #'banjo-double-c-tuning \stringTuning <g' c g c' d'>
\makeDefaultStringTuning #'banjo-double-d-tuning \stringTuning <a' d g d' e'>

%% ukulele tunings
\makeDefaultStringTuning #'ukulele-tuning \stringTuning <g' c' e' a'>
\makeDefaultStringTuning #'ukulele-d-tuning \stringTuning <a' d' fis' b'>
\makeDefaultStringTuning #'tenor-ukulele-tuning \stringTuning <g c' e' a'>
\makeDefaultStringTuning #'baritone-ukulele-tuning \stringTuning <d g b e'>

%% orchestral strings
\makeDefaultStringTuning #'violin-tuning \stringTuning <g d' a' e''>
\makeDefaultStringTuning #'viola-tuning \stringTuning <c g d' a'>
\makeDefaultStringTuning #'cello-tuning \stringTuning <c, g, d a>
\makeDefaultStringTuning #'double-bass-tuning \stringTuning <e,, a,, d, g,>

defaultStringTunings = #(reverse! defaultStringTunings)

%% convert 5-string banjo tuning to 4-string by removing the 5th string
four-string-banjo = #(lambda (tuning)
                         (take tuning 4))
