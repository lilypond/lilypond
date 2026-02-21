\version "2.27.0"

\header {
  texidoc = "Warnings and error messages from mensural ligatures."
}


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
                  \tweak ligature-flexa ##t d1
                  \tweak right-up-stem ##t e \]
  <>^"SfSuB "  \[ a1 \tweak ligature-flexa ##t g
                  \tweak right-up-stem ##t f\breve \]
  <>^"dBfSpS " \[ \tweak right-down-stem ##t c\breve
                  \tweak ligature-flexa ##t d1
                  \tweak ligature-pes ##t g \]
  <>^"lLdB "   \[ \tweak left-down-stem ##t g\longa
                  \tweak right-down-stem ##t f\breve \]
  <>^"dLuB "   \[ \tweak right-down-stem ##t g\longa
                 \tweak right-up-stem ##t f\breve \]
  <>^"lSpM "   \[ \tweak left-down-stem ##t d1
                  \tweak ligature-pes ##t a\maxima \]
  <>^"BpLL "   \[ d\breve \tweak ligature-pes ##t a\longa g \]
  <>^"ulMlM "  \[ \tweak right-up-stem ##t \tweak left-down-stem ##t g\maxima
                  \tweak left-down-stem ##t b \]
  \bar "|" \break

% invalid ligatures
  \textMark "invalid ligatures"

% single notes
  <>^"S "    \[ a1 \]
  <>^"B "    \[ a\breve \]
  <>^"L "    \[ a\longa \]
  <>^"M "    \[ a\maxima \]

% repeated pitches
  <>^"SS "   \[ a1 a \]
  <>^"BB "   \[ a\breve a \]
  <>^"BB "   \[ a\breve \tweak ligature-flexa ##t a \]
  <>^"LL "   \[ a\longa a \]

% semibrevis can't be followed by a brevis
  <>^"SBB "  \[ a1 f\breve g \]
  <>^"SfBB " \[ a1 \tweak ligature-flexa ##t f\breve g \]

% invalid duration
  <>^"B2 "   \[ a\breve g2 \]
  <>^"Bd2 "  \[ a\breve \tweak right-down-stem ##t g2 \]
  <>^"B2.B " \[ a\breve b2. a\breve \]

  \break

% chords (too many pitches)
  <>^"BB "   \[ d\breve <g a> \]
  <>^"BB "   \[ <c g>\breve a \]
  <>^"BB "   \[ <g f>\breve <d a> \]
  <>^"BB "   \[ <e c>\breve <a c'> \]

% skips (not enough pitches)
  <>^"Bs "   \[ a\breve s \]
  <>^"BsB "  \[ a\breve s g \]
  <>^"BsBB " \[ a\breve s g f \]
  <>^"Ls "   \[ a\longa \tweak right-up-stem ##t s \]
  <>^"sS "   \[ s1 e \]

% clef change in the middle
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
