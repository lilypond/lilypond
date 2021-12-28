%%%% SATB choir template (divided).
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

\version "2.19.25"


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                %%
%%          Accompanied Divided Choir             %%
%%                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  This file may be \include'd in a score to provide the
  context structure for a score arrangement consisting
  of the following staves, although normally only a subset
  of these staves would be used in any one score.

  Solo Staff (outside Choir grouping)
  Descant Staff (within Choir grouping)
  Women Staff (single voice on one staff)
  First and Second Soprano (optionally on two Staves or one Staff)
  Soprano and Alto (optionally on two Staves or one Staff)
  First and Second Alto (optionally on two Staves or one Staff)
  First and Second Tenor (optionally on two Staves or one Staff)
  Tenor and Bass (optionally on two Staves or one Staff)
  First and Second Bass (optionally on two Staves or one Staff)
  Men Staff (single voice on one staff)
  Piano Staff

  It is intended primarily to hide the complexity of the context
  structure from newcomers to LilyPond, but is also useful as a
  shorthand for seasoned users.  It is intended to facilitate the
  setting of pieces in which different groupings of voices appear
  at different times.  All that is necessary is to populate the
  appropriate variables with music for the relevant sections and
  with rests elsewhere, with sections separated by line breaks.

  Usage:

  ssaattbb.ly should be included at the *end* of the input file. Before
  it are placed the required music and lyrics by redefining specific
  variables, like this:

  \paper { ... }
  \header { ... }
  Key = { ... }
  Time = { ... }
  SopranoOneMusic = \relative { ... }
  SopranoOneLyrics = \lyricmode { ... }
  SopranoTwoMusic = \relative { ... }
  SopranoTwoLyrics = \lyricmode { ... }
  AltoOneMusic = \relative { ... }
  AltoOneLyrics = \lyricmode { ... }
  AltoTwoMusic = \relative { ... }
  AltoTwoLyrics = \lyricmode { ... }
  TenorOneMusic = \relative { ... }
  TenorOneLyrics = \lyricmode { ... }
  TenorTwoMusic = \relative { ... }
  TenorTwoLyrics = \lyricmode { ... }
  BassOneMusic = \relative { ... }
  BassOneLyrics = \lyricmode { ... }
  BassTwoMusic = \relative { ... }
  BassTwoLyrics = \lyricmode { ... }
  PianoRHMusic = \relative { ... }
  PianoDynamics = { ... }
  PianoLHMusic = \relative { ... }
  TwoVoicesPerStaff = ##f (applies to all staves)
  \include "ssaattbb.ly"

  In addition, if there are sections in which two or more parts
  are in unison, the following variables may also be defined:

  WomenMusic = \relative { ... }
  WomenLyrics = \lyricmode { ... }
  SopranoMusic = \relative { ... }
  SopranoLyrics = \lyricmode { ... }
  AltoMusic = \relative { ... }
  AltoLyrics = \lyricmode { ... }
  TenorMusic = \relative { ... }
  TenorLyrics = \lyricmode { ... }
  BassMusic = \relative { ... }
  BassLyrics = \lyricmode { ... }
  MenMusic = \relative { ... }
  MenLyrics = \lyricmode { ... }

  All of the definitions are optional. Staves with no music will be
  omitted from the output.

  If TwoVoicesPerStaff is #f, music of the various voices may be
  placed individually on one or two staves according to the
  following variables, all of which default to #f:

  WomenTwoVoicesPerStaff
  SopranoTwoVoicesPerStaff
  AltoTwoVoicesPerStaff
  TenorTwoVoicesPerStaff
  BassTwoVoicesPerStaff
  MenTwoVoicesPerStaff

  Other variables, such as the instrumentName, shortInstrumentName
  and MidiInstrument can also be changed by defining variables like
  AltoInstrumentName, BassMidiInstrument, etc.  The prefixes for staves
  containing two divided voices are WomenDivided and MenDivided, etc
  hence the corresponding variables would be WomenDividedInstrumentName,
  etc.

  The key is defined in the variable Key, and the structure of time
  and repeats in the variable Time, using spacer rests.

  A \layout block may be defined in the variable Layout.  There is
  no default \header block and no default \paper block.

  Music may be tagged with #'print or #'play to be included only in
  the printed score or in the MIDI file respectively.

%}

\include "vocal-tkit.ly"
\include "piano-tkit.ly"

#(define ssaattbb-voice-prefixes
   ;; These define the permitted prefixes to various names.
   ;; They are combined with a fixed set of postfixes to form
   ;; names such as AltoMusic, BassInstrumentName, etc.
   ;; These names may be redefined.
   '("Alto"
     "AltoOne"
     "AltoTwo"
     "AltoDivided"
     "Bass"
     "BassOne"
     "BassTwo"
     "BassDivided"
     "Descant"
     "Men"
     "MenDivided"
     "Piano"
     "PianoLH"
     "PianoRH"
     "Solo"
     "Soprano"
     "SopranoOne"
     "SopranoTwo"
     "SopranoDivided"
     "Tenor"
     "TenorOne"
     "TenorTwo"
     "TenorDivided"
     "Women"
     "WomenDivided"))

#(define ssaattbb-lyrics-postfixes
   ;; These define the permitted postfixes to the names of lyrics.
   ;; They are combined with the prefixes to form names like
   ;; AltoLyrics, etc.
   ;; These names may be redefined or extended.
  '("Lyrics"
    "LyricsOne"
    "LyricsTwo"
    "LyricsThree"
    "LyricsFour"))

#(define ssaattbb-lyrics-variable-names
   ;; These define the names which may be used to specify stanzas
   ;; which go between the two two-voice staves when TwoVoicesPerStaff
   ;; is set to #t.  They may be redefined or extended.
  '("VerseOne"
    "VerseTwo"
    "VerseThree"
    "VerseFour"
    "VerseFive"
    "VerseSix"
    "VerseSeven"
    "VerseEight"
    "VerseNine"))

%% make the above definitions available
#(set-music-definitions!
  ssaattbb-voice-prefixes
  ssaattbb-lyrics-postfixes
  ssaattbb-lyrics-variable-names)

#(define ssaattbb-variable-names
; Define names which are in addition to the base variable names
; of Time, Layout, Key, PianoDynamics and TwoVoicesPerStaff
   '("AltoTwoVoicesPerStaff"
     "BassTwoVoicesPerStaff"
     "MenTwoVoicesPerStaff"
     "SopranoTwoVoicesPerStaff"
     "TenorTwoVoicesPerStaff"
     "WomenTwoVoicesPerStaff"))

%% and make them available
#(define-missing-variables! ssaattbb-variable-names)

#(if TwoVoicesPerStaff
 ; Set all staves to contain two voices
     (begin
      (set! AltoTwoVoicesPerStaff #t)
      (set! BassTwoVoicesPerStaff #t)
      (set! MenTwoVoicesPerStaff #t)
      (set! SopranoTwoVoicesPerStaff #t)
      (set! TenorTwoVoicesPerStaff #t)
      (set! WomenTwoVoicesPerStaff #t)))

#(define (set-default-instr-names! voice)
; If the several instrument names and short instrument
; names have not been defined by the user, set them to
; the defaults "Soprano 1" and "S 1" etc with the same names
; in a column for the divided staves"
   (define (make-sym str1 str2)
     (string->symbol (string-append str1 str2)))

   (if (not (make-id voice "OneInstrumentName"))
       (ly:parser-define!
         (make-sym voice "OneInstrumentName")
          (string-append voice " 1")))
   (if (not (make-id voice "TwoInstrumentName"))
       (ly:parser-define!
         (make-sym voice "TwoInstrumentName")
          (string-append voice " 2")))
   (if (not (make-id voice "DividedInstrumentName"))
       (ly:parser-define!
         (make-sym voice "DividedInstrumentName")
         #{ \markup \right-column \smallCaps {
           #(if (make-id voice "OneMusic")
                  (make-id voice "OneInstrumentName") "")
           #(if (make-id voice "TwoMusic")
                (make-id voice "TwoInstrumentName") "") } #} ))
   (if (not (make-id voice "OneShortInstrumentName"))
       (ly:parser-define!
         (make-sym voice "OneShortInstrumentName")
         (string-append (substring voice 0 1) " 1")))
   (if (not (make-id voice "TwoShortInstrumentName"))
       (ly:parser-define!
         (make-sym voice "TwoShortInstrumentName")
         (string-append (substring voice 0 1) " 2")))
   (if (not (make-id voice "DividedShortInstrumentName"))
       (ly:parser-define!
         (make-sym voice "DividedShortInstrumentName")
         #{ \markup \right-column \smallCaps {
           #(if (make-id voice "OneMusic")
                (make-id voice "OneShortInstrumentName") "")
           #(if (make-id voice "TwoMusic")
                (make-id voice "TwoShortInstrumentName") "") } #} )))

#(set-default-instr-names! "Soprano")
#(set-default-instr-names! "Alto")
#(set-default-instr-names! "Tenor")
#(set-default-instr-names! "Bass")

SSAATTBB =
<<
  \make-one-voice-vocal-staff "Solo" "treble"
  \new ChoirStaff
  <<
    \make-one-voice-vocal-staff "Descant" "treble"
    \make-one-voice-vocal-staff "Women" "treble"
    #(if SopranoTwoVoicesPerStaff
         #{ \make-two-voice-vocal-staff "SopranoDivided" "treble" "SopranoOne" "SopranoTwo" #}
         #{ << \make-one-voice-vocal-staff "SopranoOne" "treble"
               \make-one-voice-vocal-staff "SopranoTwo" "treble" >> #} )
    #(if WomenTwoVoicesPerStaff
         #{ \make-two-voice-vocal-staff "WomenDivided" "treble" "Soprano" "Alto" #}
         #{ << \make-one-voice-vocal-staff "Soprano" "treble"
               \make-one-voice-vocal-staff "Alto" "treble" >> #} )
    #(if AltoTwoVoicesPerStaff
         #{ \make-two-voice-vocal-staff "AltoDivided" "treble" "AltoOne" "AltoTwo" #}
         #{ << \make-one-voice-vocal-staff "AltoOne" "treble"
               \make-one-voice-vocal-staff "AltoTwo" "treble" >> #} )
    #(if TenorTwoVoicesPerStaff
         #{ \make-two-voice-vocal-staff "TenorDivided" "treble_8" "TenorOne" "TenorTwo" #}
         #{ << \make-one-voice-vocal-staff "TenorOne" "treble_8"
               \make-one-voice-vocal-staff "TenorTwo" "treble_8" >> #} )
    #(if MenTwoVoicesPerStaff
         #{ \make-two-voice-vocal-staff "MenDivided" "bass" "Tenor" "Bass" #}
         #{ << \make-one-voice-vocal-staff "Tenor" "treble_8"
               \make-one-voice-vocal-staff "Bass" "bass" >> #} )
    #(if BassTwoVoicesPerStaff
         #{ \make-two-voice-vocal-staff "BassDivided" "bass" "BassOne" "BassTwo" #}
         #{ << \make-one-voice-vocal-staff "BassOne" "bass"
               \make-one-voice-vocal-staff "BassTwo" "bass" >> #} )
    \make-one-voice-vocal-staff "Men" "bass"
  >>
>>

Piano = \make-pianostaff

\tagGroup #'(print play)

\layout {
  \context {
    \Staff
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##t
  }
}

\score {
  \keepWithTag #'print
  #(if have-music
       #{ << \SSAATTBB \Piano >> #}
       #{ { } #} )
  \layout { $(if Layout Layout) }
}


\score {
  \keepWithTag #'play
  #(if have-music
       #{ << \SSAATTBB \Piano >> #}
       #{ { } #} )
  \midi {
    \context {
      \Score
      midiChannelMapping = #'instrument
    }
  }
}
