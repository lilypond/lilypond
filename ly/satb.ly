%%%% SATB choir template.
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

%% FIXME: why does this have to be commented? -vv
%\version "2.19.19"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                %%
%%     Accompanied Choir with Multiple Verses     %%
%%                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  This file may be \include'd in a score to provide the
  context structure for a score arrangement consisting
  of the following staves:

  Solo Staff (outside Choir grouping)
  Descant Staff (within Choir grouping)
  Women Staff (single voice on one staff)
  Soprano and Alto (optionally on two Staves or one Staff each)
  Multiple verses (up to 9)
  Tenor and Bass (optionally on two Staves or one Staff each)
  Men Staff (single voice on one staff)
  Piano Staff

  It is intended primarily to hide the complexity of the context
  structure from newcomers to LilyPond, but is also useful as a
  shorthand for seasoned users.

  Usage:

  satb.ly should be included at the *end* of the input file. Before
  it are placed the required music and lyrics by redefining specific
  variables, like this:

  \paper { ... }
  \header { ... }
  Key = { ... }
  Time = { ... }
  SoloMusic = \relative { ... }
  DescantMusic = \relative { ... }
  DescantLyrics = \lyricmode { ... }
  WomenMusic = \relative { ... }
  WomenLyrics = \lyricmode { ... }
  SopranoMusic = \relative { ... }
  SopranoLyrics = \lyricmode { ... }
  AltoMusic = \relative { ... }
  AltoLyrics = \lyricmode { ... }
  VerseOne = \lyricmode { ... }
  VerseTwo = \lyricmode { ... }
  ...
  VerseNine = \lyricmode { ... }
  TenorMusic = \relative { ... }
  TenorLyrics = \lyricmode { ... }
  BassMusic = \relative { ... }
  BassLyrics = \lyricmode { ... }
  MenMusic = \relative { ... }
  MenLyrics = \lyricmode { ... }
  PianoRHMusic = \relative { ... }
  PianoDynamics = { ... }
  PianoLHMusic = \relative { ... }
  TwoVoicesPerStaff = ##f
  \include "satb.ly"

  All of the definitions are optional. Staves with no music will be
  omitted from the output.

  Other variables, such as the instrumentName, shortInstrumentName
  and MidiInstrument can also be changed by defining variables like
  AltoInstrumentName, BassMidiInstrument, etc.  The prefixes for staves
  containing two divided voices are WomenDivided and MenDivided, hence
  the corresponding variables would be WomenDividedInstrumentName, etc.
  The key is defined in the variable Key, and the structure of time
  and repeats in the variable Time, using spacer rests.

  A \layout block may be defined in the variable Layout.  There is
  no default \header block and no default \paper block.

  Music may be tagged with #'print or #'play to be included only in
  the printed score or in the MIDI file respectively.

%}

\include "vocal-tkit.ly"
\include "piano-tkit.ly"

#(define satb-voice-prefixes
   ;; These define the permitted prefixes to various names.
   ;; They are combined with a fixed set of postfixes to form
   ;; names such as AltoMusic, BassInstrumentName, etc.
   ;; These names may be redefined.
   '("Alto"
     "Bass"
     "Descant"
     "Men"
     "MenDivided"
     "Piano"
     "PianoLH"
     "PianoRH"
     "Solo"
     "Soprano"
     "Tenor"
     "Women"
     "WomenDivided"))

#(define satb-lyrics-postfixes
   ;; These define the permitted postfixes to the names of lyrics.
   ;; They are combined with the prefixes to form names like
   ;; AltoLyrics, etc.
   ;; These names may be redefined or extended.
  '("Lyrics"
    "LyricsOne"
    "LyricsTwo"
    "LyricsThree"
    "LyricsFour"))

#(define satb-lyrics-variable-names
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
  satb-voice-prefixes
  satb-lyrics-postfixes
  satb-lyrics-variable-names)

%% override the usual default value
#(if (not SoloShortInstrumentName)
       (set! SoloShortInstrumentName ""))

SATB =
<<
  \make-one-voice-vocal-staff "Solo" "treble"
  \new ChoirStaff <<
    \make-one-voice-vocal-staff "Descant" "treble"
    \make-one-voice-vocal-staff "Women" "treble"
    #(if TwoVoicesPerStaff
      #{
        \make-two-vocal-staves-with-stanzas
          "WomenDivided" "treble" "MenDivided" "bass"
          "Soprano" "Alto" "Tenor" "Bass"
          #satb-lyrics-variable-names
      #}
      #{
        <<
          \make-one-voice-vocal-staff "Soprano" "treble"
          \make-one-voice-vocal-staff "Alto" "treble"
          \make-one-voice-vocal-staff "Tenor" "treble_8"
          \make-one-voice-vocal-staff "Bass" "bass"
        >>
      #} )
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
       #{ << \SATB \Piano >> #}
       #{ { } #} )
  \layout { $(if Layout Layout) }
}


\score {
  \keepWithTag #'play
  #(if have-music
       #{ << \SATB \Piano >> #}
       #{ { } #} )
  \midi {
    \context {
      \Score
      midiChannelMapping = #'instrument
    }
  }
}
