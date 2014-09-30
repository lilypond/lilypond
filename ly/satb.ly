\version "2.18.0"

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
  Key = { ... }
  Time = { ... }
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
  TwoVoicesPerStaff = ##f
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

#(defmacro defaulting (name . default)
  (if (defined? name) name (if (pair? default) (car default) '#{#})))

#(define (sym . strings) (string->symbol (apply string-append strings)))

#(defmacro short-name (part)
  "Use PartShortInstrumentName, or the first letter of
PartInstrumentName or its default."
  (if (defined? (sym part "Music"))
    (let ((sname (sym part "ShortInstrumentName")))
      (if (defined? sname)
        sname
        `(substring (defaulting ,(sym part "InstrumentName") ,part)
	            0 1)))
    ""))

#(defmacro lyrics-if-defined (name voice . optionals)
  (let ((above (if (pair? optionals) (car optionals) #f)))
    (if (defined? name)
      `(make-music 'ContextSpeccedMusic
	 'create-new #t
	 'context-type 'Lyrics
	 'property-operations ',(if above `((assign alignAboveContext ,above)) '())
	 'element (make-music 'LyricCombineMusic
		   'associated-context ,voice
		   'element ,name))
      #{#})))

#(defmacro one-voice-staff (name clef)
  `#{ <<
     \new Staff = #(identity ,name) \with {
       instrumentName = \markup \smallCaps
         #(defaulting ,(sym name "InstrumentName") ,name)
       shortInstrumentName = \markup \smallCaps #(short-name ,name)
       midiInstrument = "clarinet"
     } {
       #(defaulting Key)
       \clef #(identity ,clef)
       \new Voice = #(identity ,name) <<
	 #(defaulting Time)
	 \dynamicUp
	 #(defaulting ,(sym name "Music"))
       >>
     }
     #(lyrics-if-defined ,(sym name "Lyrics") ,name)
     #(lyrics-if-defined ,(sym name "LyricsOne") ,name)
     #(lyrics-if-defined ,(sym name "LyricsTwo") ,name)
     #(lyrics-if-defined ,(sym name "LyricsThree") ,name)
   >> #})

#(defmacro two-voice-staff (name clef v1name v2name)
  `#{ <<
    \new Staff = #(identity ,name) \with {
      instrumentName = \markup \right-column \smallCaps {
        #(defaulting ,(sym v1name "InstrumentName") ,v1name)
        #(defaulting ,(sym v2name "InstrumentName") ,v2name)
      }
      shortInstrumentName = \markup \right-column \smallCaps {
	#(short-name ,v1name)
	#(short-name ,v2name)
      }
      midiInstrument = "clarinet"
    } <<
      #(defaulting Key)
      \clef #(identity ,clef)
      \new Voice = #(identity ,v1name) <<
	#(defaulting Time)
	\voiceOne
	\dynamicUp
	#(defaulting ,(sym v1name "Music"))
      >>
      \new Voice = #(identity ,v2name) <<
	#(defaulting Time)
	\voiceTwo
	#(defaulting ,(sym v2name "Music"))
      >>
    >>
    #(lyrics-if-defined ,(sym v1name "Lyrics") ,v1name ,name)
    #(lyrics-if-defined ,(sym v1name "LyricsOne") ,v1name ,name)
    #(lyrics-if-defined ,(sym v1name "LyricsTwo") ,v1name ,name)
    #(lyrics-if-defined ,(sym v1name "LyricsThree") ,v1name ,name)
    #(lyrics-if-defined ,(sym v2name "Lyrics") ,v2name)
    #(lyrics-if-defined ,(sym v2name "LyricsOne") ,v2name)
    #(lyrics-if-defined ,(sym v2name "LyricsTwo") ,v2name)
    #(lyrics-if-defined ,(sym v2name "LyricsThree") ,v2name)
  >> #})

SATB = <<
  \new ChoirStaff
  \with {
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##t
  }
  <<
    #(one-voice-staff "Descant" "treble")

    #(if (defaulting TwoVoicesPerStaff #f)
      (two-voice-staff "Women" "treble" "Soprano" "Alto")
      (make-simultaneous-music (list (one-voice-staff "Soprano" "treble")
				     (one-voice-staff "Alto" "treble"))))

    #(lyrics-if-defined VerseOne "Soprano")
    #(lyrics-if-defined VerseTwo "Soprano")
    #(lyrics-if-defined VerseThree "Soprano")
    #(lyrics-if-defined VerseFour "Soprano")
    #(lyrics-if-defined VerseFive "Soprano")
    #(lyrics-if-defined VerseSix "Soprano")
    #(lyrics-if-defined VerseSeven "Soprano")
    #(lyrics-if-defined VerseEight "Soprano")
    #(lyrics-if-defined VerseNine "Soprano")

    #(if (defaulting TwoVoicesPerStaff #f)
      (two-voice-staff "Men" "bass" "Tenor" "Bass")
      (make-simultaneous-music (list (one-voice-staff "Tenor" "treble_8")
				     (one-voice-staff "Bass" "bass"))))
  >>  % End ChoirStaff

  \new PianoStaff
  \with {
    instrumentName = \markup \smallCaps
                       #(defaulting PianoInstrumentName "Piano" )
    shortInstrumentName = \markup \smallCaps #(short-name "Piano" )
    \override VerticalAxisGroup.remove-empty = ##t
    \override VerticalAxisGroup.remove-first = ##t
  }
  <<
    \new Staff {
      \clef "treble"
      #(defaulting Key)
      \new Voice <<
	#(defaulting Time)
	#(defaulting PianoRHMusic)
      >>
    }
    \new Dynamics {
      #(defaulting PianoDynamics)
    }
    \new Staff {
      \clef "bass"
      #(defaulting Key)
      \new Voice <<
	#(defaulting Time)
	#(defaulting PianoLHMusic)
      >>
    }
  >>
>>

\tagGroup #'(print play)

\score {
  \keepWithTag #'print \SATB
  \layout { #(defaulting Layout) }
}

\score {
  \keepWithTag #'play \SATB
  \midi { }
}

