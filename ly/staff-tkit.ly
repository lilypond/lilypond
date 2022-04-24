%%%% Template toolkit (staff functions).
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

\include "voice-tkit.ly"


%% Staff-oriented functions

% These assume the following lists have been defined:
%   voice-prefixes  eg "Soprano"
%   voice-postfixes  eg "Music"
%   lyrics-postfixes  eg "Lyrics"
%   lyrics-names  eg "VerseOne"
%   variable-names  eg "Time"
%
% The first three lists are used to generate compound
% names such as "SopranoLyrics" and "SopranoInstrumentName"
% The last two lists of names are used as-is.


make-one-voice-staff =
#(define-music-function (show-instrName name clef dynamic-direction)
   ((boolean? #t) voice-prefix? string? (up-or-down? ""))

   "Make a staff with one voice (no lyrics)
    show-instrName: show instrument and short instrument names?
              name: the default prefix for instrument name and music
              clef: the clef for this staff
 dynamic-direction: dynamics are up, down or neither"

   (define music (make-id name "Music"))
   (define instrName (make-id name "InstrumentName"))
   (define shortInstrName (make-id name "ShortInstrumentName"))
   (define midiName (make-id name "MidiInstrument"))
   (define dynUp (equal? dynamic-direction "Up"))
   (define dynDown (equal? dynamic-direction "Down"))
   (if music
     #{
       \new Staff = #(string-append name "Staff")
       \with {
         instrumentName = \markup \smallCaps {
           #(if show-instrName
                (if instrName instrName name)
                "")
         }
         shortInstrumentName = \markup \smallCaps {
           #(if show-instrName
                (cond
                 (shortInstrName shortInstrName)
                 (instrName (substring instrName 0 1))
                 (else (substring name 0 1)))
                "")
         }
         midiInstrument = #(if midiName midiName "clarinet")
         #(cond
           (dynUp dynamicUp)
           (dynDown dynamicDown)
           (else dynamicNeutral))

       }
       {
         #(if Key Key)
         \clef #clef
         \make-voice #name
       }
     #}
     (make-music 'SequentialMusic 'void #t)))


make-two-voice-staff =
#(define-music-function (name clef v1name v2name)
   (voice-prefix? string? voice-prefix? voice-prefix?)

   "Make a vocal staff with two voices
      name: the prefix to the staff name
      clef: the clef to use
    v1name: the prefix to the name of voice one
    v2name: the prefix to the name of voice two "

   (define v1music (make-id v1name "Music"))
   (define v2music (make-id v2name "Music"))
   (define instrName (make-id name "InstrumentName"))
   (define v1InstrName (make-id v1name "InstrumentName"))
   (define v2InstrName (make-id v2name "InstrumentName"))
   (define shortInstrName (make-id name "ShortInstrumentName"))
   (define v1ShortInstrName (make-id v1name "ShortInstrumentName"))
   (define v2ShortInstrName (make-id v2name "ShortInstrumentName"))
   (define v1midiName (make-id v1name "MidiInstrument"))
   (define v2midiName (make-id v2name "MidiInstrument"))
   (if (or v1music v2music)
       #{
         <<
           \new Staff = #(string-append name "Staff")
           \with {
             \remove Staff_performer
             instrumentName =
               #(if instrName
                 #{ \markup \smallCaps #instrName #}
                 #{ \markup \right-column \smallCaps {
                  #(if v1music
                       (if v1InstrName v1InstrName v1name)
                       "")
                  #(if v2music
                       (if v2InstrName v2InstrName v2name)
                       "")
                 } #} )
             shortInstrumentName =
               #(if shortInstrName
                  #{ \markup \smallCaps #shortInstrName #}
                  #{ \markup \right-column \smallCaps {
                    #(if v1music
                         (cond
                          (v1ShortInstrName v1ShortInstrName)
                          (v1InstrName (substring v1InstrName 0 1))
                          (else (substring v1name 0 1)))
                         "")
                    #(if v2music
                         (cond
                          (v2ShortInstrName v2ShortInstrName)
                          (v2InstrName (substring v2InstrName 0 1))
                          (else (substring v2name 0 1)))
                         "")
                  } #} )
           }
           <<
             #(if Key Key)
             \clef #clef

             #(if v1music
               #{
                 \new Voice = #(string-append v1name "Voice")
                 \with {
                   \consists Staff_performer
                   \dynamicUp
                   midiInstrument =
                     #(if v1midiName v1midiName "clarinet")
                 }
                 <<
                   #(if KeepAlive KeepAlive)
                   #(if Time Time)
                   #(if v2music voiceOne oneVoice)
                   #v1music
                 >>
               #} )

             #(if v2music
               #{
                 \new Voice = #(string-append v2name "Voice")
                 \with {
                   \consists Staff_performer
                   \dynamicDown
                   midiInstrument =
                     #(if v2midiName v2midiName "clarinet")
                 }
                 <<
                   #(if KeepAlive KeepAlive)
                   #(if Time Time)
                   #(if v1music voiceTwo oneVoice)
                   #v2music
                 >>
               #} )
           >>
         >>
       #}
        (make-music 'SequentialMusic 'void #t)))
