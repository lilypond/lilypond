%%%% SATB choir template (lyrics functions).
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

%%% The function used by the built-in templates to
%   build a lyrics context and associate it with a voice.

\include "voice-tkit.ly"

make-one-stanza =
#(define-music-function
  (pos staffPrefix voicePrefix altVoicePrefix lyrics)
  ((above-or-below? #f)
   voice-prefix?
   voice-prefix?
   (voice-prefix?)
   vocal-lyrics-or-verses?)

   "Make a single stanza
           (pos: positioned Above or Below the named staff)
    staffPrefix: voice prefix for the staff to be positioned against
    voicePrefix: voice prefix for the associated voice
(altVoicePrefix: voice prefix for the associated voice
                 if the first is not present)
         lyrics: the words"

   (let* ((lyrics-name (string-append voicePrefix lyrics))
          (staff-name (string-append staffPrefix "Staff"))
          (music-name (make-id voicePrefix "Music"))
          (assoc-voice-name (if music-name
                               voicePrefix
                               altVoicePrefix))
          (with-clause
           (if pos
               #{ \with {
                    #(string-append "align" pos "Context") = #staff-name
                  }
               #}
               (make-music 'SequentialMusic 'void #t)))
          (stanza (if (member lyrics lyrics-postfixes)
                        (make-id voicePrefix lyrics)
                        (get-id lyrics))))
   (if (and stanza  ;we need lyrics and at least one associated voice
            (or music-name
                (make-id altVoicePrefix "Music")))
        #{
          \new Lyrics = #lyrics-name
          \with { #with-clause }
          \lyricsto #(string-append assoc-voice-name "Voice")
          { #stanza }
        #}
        (make-music 'SequentialMusic 'void #t))))
