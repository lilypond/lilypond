\version "2.27.0"

\header {
  texidoc = "Music unsuitable for a ligature or an out of scope tweak
must not crash lilypond and should preferably produce a helpful message."
}

% #(ly:set-option 'warning-as-error #t)


\paper {
  system-system-spacing.basic-distance = #20
  score-system-spacing.basic-distance = #20
}


\layout {
  ragged-right = ##t
  indent = #0
  \context {
    \Voice
    \override TextScript.padding = #3
  }
  \context {
    \Score
    \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 16 1)
    \override PaperColumn.keep-inside-line = ##f
    \override TextMark.padding = #7
    \override TextMark.font-series = #'bold
    \override TextMark.font-size = #0
  }
}


\context PetrucciStaff \with {
  \override StaffSymbol.line-count = 4
  \omit TimeSignature
} {
  \clef "petrucci-c5"
  \set Staff.printKeyCancellation = ##f
  \cadenzaOn % turn off bar lines
  \accidentalStyle forget
  \textLengthOn

  \textMark "ignored tweaks"
  <>^"BfSuS "  \[ a\breve
                  \applyContext
                  #(lambda (context)
                    (ly:check-expected-warnings)
                    (ly:expect-warning
                     (G_ "flexa cannot have stem in the middle"))
                    (ly:expect-warning
                     (G_ "only longae and maximae may have right stem")))
                  \tweak ligature-flexa ##t d1
                  \tweak right-up-stem ##t e
                  \]

  % ligature warnings are emitted at the end of the timestep,
  % so the check needs to be delayed
  s

  <>^"SfSuB "  \[ a1
                  \applyContext
                  #(lambda (context)
                    (ly:check-expected-warnings)
                    (ly:expect-warning
                     (G_ "this note must form a flexa with the next note,"
                    " not the previous one"))
                    (ly:expect-warning
                     (G_ "only longae and maximae may have right stem")))
                  \tweak ligature-flexa ##t g
                  \tweak right-up-stem ##t f\breve \]

  s

  <>^"dBfSpS " \[ \applyContext
                  #(lambda (context)
                    (ly:check-expected-warnings)
                    (ly:expect-warning
                     (G_ "only longae and maximae may have right stem"))
                    (ly:expect-warning
                     (G_ "flexa cannot have stem in the middle"))
                    (ly:expect-warning
                     (G_ "only a final longa higher at least by a third"
                    " than the previous note can be drawn pes-like")))
                  \tweak right-down-stem ##t c\breve
                  \tweak ligature-flexa ##t d1
                  \tweak ligature-pes ##t g \]

  s

  <>^"lLdB "   \[ \applyContext
                  #(lambda (context)
                    (ly:check-expected-warnings)
                    (ly:expect-warning
                     (G_ "only a breve can have downward left stem"))
                    (ly:expect-warning
                     (G_ "only longae and maximae may have right stem")))
                  \tweak left-down-stem ##t g\longa
                  \tweak right-down-stem ##t f\breve \]

  s

  <>^"LuB "    \[ g\longa
                  \applyContext
                  #(lambda (context)
                    (ly:check-expected-warnings)
                    (ly:expect-warning
                     (G_ "only longae and maximae may have right stem")))
                  \tweak right-up-stem ##t f\breve \]

  s

  <>^"lSM "    \[ \applyContext
                  #(lambda (context)
                    (ly:check-expected-warnings)
                    (ly:expect-warning
                     (G_ "only a breve can have downward left stem")))
                  \tweak left-down-stem ##t d1
                  a\maxima \]

  s

  <>^"BpLL "   \[ d\breve
                  \applyContext
                  #(lambda (context)
                    (ly:check-expected-warnings)
                    (ly:expect-warning
                     (G_ "only a final longa higher at least by a third"
                    " than the previous note can be drawn pes-like")))
                  \tweak ligature-pes ##t a\longa
                  g \]

  s

  <>^"lMlM "   \[ \applyContext
                  #(lambda (context)
                    (ly:check-expected-warnings)
                    (ly:expect-warning
                     (G_ "only a breve can have downward left stem"))
                    (ly:expect-warning
                     (G_ "only a breve can have downward left stem")))
                  \tweak left-down-stem ##t g\maxima
                  \tweak left-down-stem ##t b \]

  s

  \bar "|" \break

% invalid ligatures
  \textMark "invalid ligatures"

% single notes
  <>^"S "    \[ \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single note ligature")))
                a1 \]

  s

  <>^"B "    \[ \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single note ligature")))
                a\breve \]

  s

  <>^"L "    \[ \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single note ligature")))
                a\longa \]

  s

  <>^"M "    \[ \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single note ligature")))
                a\maxima \]

  s

% repeated pitches
  <>^"SS "   \[ a1
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "unison within ligature")))
                a \]

  s

  <>^"BB "   \[ a\breve
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "unison within ligature")))
                a \]

  s

  <>^"BB "   \[ a\breve
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "unison within ligature")))
                \tweak ligature-flexa ##t a \]

  s

  <>^"LL "   \[ a\longa
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "unison within ligature")))
                a \]

  s

% semibrevis can't be followed by a brevis
  <>^"SBB "  \[ a1
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single semibreve must not be followed by a breve")))
                f\breve
                g \]

  s

  <>^"SfBB " \[ a1
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single semibreve must not be followed by a breve"))
                  (ly:expect-warning
                   (G_ "flexa cannot have stem in the middle")))
                \tweak ligature-flexa ##t f\breve
                g \]

  s

% invalid duration
  <>^"B2 "   \[ a\breve
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "mensural ligature: duration none of"
                  " maxima, longa, breve, or semibreve")))
                g2 \]

  s

  <>^"Bd2 "  \[ a\breve
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "mensural ligature: duration none of"
                    " maxima, longa, breve, or semibreve"))
                  (ly:expect-warning
                   (G_ "only longae and maximae may have right stem")))
                \tweak right-down-stem ##t g2 \]

  s

  <>^"B2.B " \[ a\breve
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "mensural ligature: duration none of"
                  " maxima, longa, breve, or semibreve")))
                b2.
                a\breve \]

  \break

% chords
% ligature engraver sees just pitches, so no warnings, crazy look
  <>^"BB "   \[ d\breve <g a> \]
  <>^"BB "   \[ <c g>\breve a \]
  <>^"BB "   \[ <g f>\breve <d a> \]
  <>^"BB "   \[ <e c>\breve <a c'> \]

% skips
% ligature engraver doesn't see them, so no warnings
  <>^"Bs "   \[ \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single note ligature")))
                a\breve s \]

  s

  <>^"BsB "  \[ a\breve s g \]
  <>^"BsBB " \[ a\breve s g f \]
  <>^"Ls "   \[ \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single note ligature")))
                a\longa \tweak right-up-stem ##t s \]

  s

  <>^"sS "   \[ \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "single note ligature")))
                s1 e \]

  s

% pitched rest
  <>^"BBB "  \[ a\breve
                \applyContext
                #(lambda (context)
                  (ly:check-expected-warnings)
                  (ly:expect-warning
                   (G_ "ignoring rest: ligature may not contain rest")))
                g\rest f \]

  s

% clef change in the middle (no warning, crazy look)
  \clef "petrucci-f5"
  <>^"LB "   \[ b,\longa
                \clef "petrucci-c5"
                \tweak flexa-width 8
                c\breve \]
  \clef "petrucci-f5"
  <>^"LL "   \[ b,\longa
                \clef "petrucci-c5"
                c \]
  \clef "petrucci-f4"
  <>^"LB "   \[ a\longa
                \clef "petrucci-c3"
                \tweak flexa-width 8
                g\breve \]
  \clef "petrucci-f4"
  <>^"LL "   \[ a\longa
                \clef "petrucci-c3"
                g \]
}
