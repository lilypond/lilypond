%\version "2.19.17"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                %%
%%     Accompanied Choir with Multiple Verses     %%
%%                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%{
  This file may be \include'd in a score to provide the
  context structure for a score arrangement consisting
  of the following staves:

  Descant Staff
  Soprano and Alto (optionally on two Staves or one Staff)
  Multiple verses (up to 9)
  Tenor and Bass (optionally on two Staves or one Staff)
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
  TwoVoicesPerStaff = ##f or ##t
  Key = { ... }
  Time = { ... }
  Layout = \layout { ... }
  DescantMusic = \relative { ... }
  DescantLyrics = \lyricmode { ... }
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
  PianoRHMusic = \relative { ... }
  PianoDynamics = { ... }
  PianoLHMusic = \relative { ... }
  \include "satb.ly"

  All of the definitions are optional. Staves with no music will be
  omitted from the output.

  Other variables, such as the instrumentName, can also be changed by
  defining variables like AltoInstrumentName.  The key is defined in
  the variable Key, and the structure of time and repeats in the
  variable Time, using spacer rests.  A \layout block may be defined in
  the variable Layout.  There is no default \header block and no default
  \paper block.

  Music may be tagged with #'print or #'play to be included only in
  the printed score or in the MIDI file respectively.

%}

#(defmacro satb-defaulting (name . default)
  (if (defined? name) name (if (pair? default) (car default) *unspecified*)))

#(define (satb-sym . strings) (string->symbol (apply string-append strings)))

#(defmacro satb-short-name (part)
  "Use PartShortInstrumentName, or the first letter of
PartInstrumentName or its default."
  (if (defined? (satb-sym part "Music"))
    (let ((sname (satb-sym part "ShortInstrumentName")))
      (if (defined? sname)
        sname
        `(substring (satb-defaulting ,(satb-sym part "InstrumentName") ,part)
                    0 1)))
    ""))

#(defmacro satb-lyrics-if-defined (name voice . optionals)
  (let ((above (and (pair? optionals) (car optionals))))
    (if (defined? name)
      `(make-music 'ContextSpeccedMusic
         'create-new #t
         'context-type 'Lyrics
         'property-operations ',(if above `((assign alignAboveContext ,above)) '())
         'element (make-music 'LyricCombineMusic
                   'associated-context ,voice
                   'element ,name))
      *unspecified*)))

#(defmacro satb-one-voice-staff (name clef)
  `#{ <<
     \new Staff = #,name \with {
       instrumentName = \markup \smallCaps
         #(satb-defaulting ,(satb-sym name "InstrumentName") ,name)
       shortInstrumentName = \markup \smallCaps #(satb-short-name ,name)
       midiInstrument = "clarinet"
     } {
       #(satb-defaulting Key)
       \clef #,clef
       \new Voice = #,name <<
         #(satb-defaulting Time)
         \dynamicUp
         #(satb-defaulting ,(satb-sym name "Music"))
       >>
     }
     #(satb-lyrics-if-defined ,(satb-sym name "Lyrics") ,name)
     #(satb-lyrics-if-defined ,(satb-sym name "LyricsOne") ,name)
     #(satb-lyrics-if-defined ,(satb-sym name "LyricsTwo") ,name)
     #(satb-lyrics-if-defined ,(satb-sym name "LyricsThree") ,name)
   >> #})

#(defmacro satb-two-voice-staff (name clef v1name v2name)
  `#{ <<
    \new Staff = #,name \with {
      instrumentName = \markup \right-column \smallCaps {
        #(satb-defaulting ,(satb-sym v1name "InstrumentName") ,v1name)
        #(satb-defaulting ,(satb-sym v2name "InstrumentName") ,v2name)
      }
      shortInstrumentName = \markup \right-column \smallCaps {
        #(satb-short-name ,v1name)
        #(satb-short-name ,v2name)
      }
      midiInstrument = "clarinet"
    } <<
      #(satb-defaulting Key)
      \clef #,clef
      \new Voice = #,v1name <<
        #(satb-defaulting Time)
        \voiceOne
        \dynamicUp
        #(satb-defaulting ,(satb-sym v1name "Music"))
      >>
      \new Voice = #,v2name <<
        #(satb-defaulting Time)
        \voiceTwo
        #(satb-defaulting ,(satb-sym v2name "Music"))
      >>
    >>
    #(satb-lyrics-if-defined ,(satb-sym v1name "Lyrics") ,v1name ,name)
    #(satb-lyrics-if-defined ,(satb-sym v1name "LyricsOne") ,v1name ,name)
    #(satb-lyrics-if-defined ,(satb-sym v1name "LyricsTwo") ,v1name ,name)
    #(satb-lyrics-if-defined ,(satb-sym v1name "LyricsThree") ,v1name ,name)
    #(satb-lyrics-if-defined ,(satb-sym v2name "Lyrics") ,v2name)
    #(satb-lyrics-if-defined ,(satb-sym v2name "LyricsOne") ,v2name)
    #(satb-lyrics-if-defined ,(satb-sym v2name "LyricsTwo") ,v2name)
    #(satb-lyrics-if-defined ,(satb-sym v2name "LyricsThree") ,v2name)
  >> #})

SATB = <<
  \new ChoirStaff
  \with {
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##t
  }
  <<
    #(satb-one-voice-staff "Descant" "treble")

    #(if (satb-defaulting TwoVoicesPerStaff #f)
      (satb-two-voice-staff "Women" "treble" "Soprano" "Alto")
      (make-simultaneous-music (list (satb-one-voice-staff "Soprano" "treble")
                                     (satb-one-voice-staff "Alto" "treble"))))

    #(satb-lyrics-if-defined VerseOne "Soprano")
    #(satb-lyrics-if-defined VerseTwo "Soprano")
    #(satb-lyrics-if-defined VerseThree "Soprano")
    #(satb-lyrics-if-defined VerseFour "Soprano")
    #(satb-lyrics-if-defined VerseFive "Soprano")
    #(satb-lyrics-if-defined VerseSix "Soprano")
    #(satb-lyrics-if-defined VerseSeven "Soprano")
    #(satb-lyrics-if-defined VerseEight "Soprano")
    #(satb-lyrics-if-defined VerseNine "Soprano")

    #(if (satb-defaulting TwoVoicesPerStaff #f)
      (satb-two-voice-staff "Men" "bass" "Tenor" "Bass")
      (make-simultaneous-music (list (satb-one-voice-staff "Tenor" "treble_8")
                                     (satb-one-voice-staff "Bass" "bass"))))
  >>  % End ChoirStaff

  \new PianoStaff
  \with {
    instrumentName = \markup \smallCaps
                       #(satb-defaulting PianoInstrumentName "Piano" )
    shortInstrumentName = \markup \smallCaps #(satb-short-name "Piano" )
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##t
  }
  <<
    \new Staff {
      \clef "treble"
      #(satb-defaulting Key)
      \new Voice <<
        #(satb-defaulting Time)
        #(satb-defaulting PianoRHMusic)
      >>
    }
    \new Dynamics {
      #(satb-defaulting PianoDynamics)
    }
    \new Staff {
      \clef "bass"
      #(satb-defaulting Key)
      \new Voice <<
        #(satb-defaulting Time)
        #(satb-defaulting PianoLHMusic)
      >>
    }
  >>
>>

\tagGroup #'(print play)

\score {
  \keepWithTag #'print \SATB
  \layout { $(satb-defaulting Layout) }
}

\score {
  \keepWithTag #'play \SATB
  \midi { }
}
