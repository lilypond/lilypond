\version "2.27.0"

\header {
  texidoc = "Mensural ligatures show different shapes, depending on the
  rhythmical pattern and direction of the melody line."
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
    \remove Bar_number_engraver
  }
}


\context PetrucciVoice {
  \clef "petrucci-c4"
  \omit Score.TimeSignature
  \textLengthOn

% ligaturae binariae
  \textMark "ligaturae binariae"

  <>^"BL "  \[ b\breve g\longa \]
  <>^"BL "  \[ g\breve b\longa \]
  <>^"BL "  \[ e\breve \tweak ligature-pes ##t b\longa \]
  <>^"LL "  \[ b\longa g \]
  <>^"LL "  \[ g\longa b \]
  <>^"LL "  \[ e\longa \tweak ligature-pes ##t b \]
  <>^"BB "  \[ b\breve g \]
  <>^"BB "  \[ g\breve b \]
  <>^"LB "  \[ b\longa g\breve \]
  <>^"LB "  \[ g\longa b\breve \]
  <>^"SS "  \[ b1 g \]
  <>^"SS "  \[ g1 b \]
  <>^"SL "  \[ b1 g\longa \]
  <>^"SL "  \[ g1 b\longa \]
  <>^"SM "  \[ b1 g\maxima \]
  <>^"SM "  \[ g1 b\maxima \]

  % "unofficial", but unambiguous
  <>^"BS " \[ b\breve g1 \]
  <>^"BS " \[ g\breve b1 \]

  \bar "|" \break

% ligaturae ternariae, quaternariae, etc. (sicut in Apel[1])
  \textMark "ligaturae ternariae, quaternariae, etc."

  <>^"BBL "     \[ b\breve a g\longa \]
  <>^"SSL "     \[ e1 f \tweak ligature-pes ##t b\longa \]
  <>^"BBL "     \[ a\breve f \tweak ligature-pes ##t b\longa \]
  <>^"BBL "     \[ f\breve c \tweak ligature-pes ##t b\longa \]
  <>^"BBL "     \[ f\breve c \tweak ligature-pes ##t c'\longa \]
  <>^"BBBB "    \[ a\breve g a b \]
  <>^"SSBBBLB " \[ b1 a g\breve a b a\longa b\breve \]
  <>^"LBMBL "   \[ a\longa g\breve a\maxima b\breve a\longa \]
  <>^"BBBBLL "  \[ d'\breve c' f d' b\longa g \]
  <>^"SSBLLBB " \[ c'1 b g\breve d'\longa a c'\breve b \]
  \bar "|" \break

% examples from "dtv-Atlas zur Musik" [2]
  \textMark "dtv-Atlas"

  <>^"BBL "        \[ d'\breve c' b\longa \]
  <>^"BBBL "       \[ a\breve b c' d'\longa \]
  <>^"L.B.BBLBBB " \[ b\longa. g\breve. a\breve b c'\longa a\breve b a \]
  <>^"SSBB "       \[ c'1 b g\breve a \]
  <>^"LBL "        \[ b\longa a\breve c'\longa \]
  <>^"SSBL "       \[ a1 b d'\breve c'\longa \]
  \bar "|"
}


\context PetrucciStaff \with {
  \override StaffSymbol.line-count = 4
  \omit TimeSignature
} {
  \clef "petrucci-c5"
  \set Staff.printKeyCancellation = ##f
  \accidentalStyle forget
  \textLengthOn

% crazy ligatures
  \textMark "crazy ligatures"

  <>^"BBBBB "     \[ c\breve e f g bes \]
  % TODO: accidentals must be collected and printed before ligature

  <>^"B.B.B.... " \[ a\breve. g b a e g f a g
                     \override NoteHead.ligature-flexa = ##t
                     f a g f d g e g d a e
                     f g e f g b f a e a f b
                     \revert NoteHead.ligature-flexa \]
  <>^"BB "        \[ \tweak style #'semipetrucci b\breve a \]
  <>^"BB "        \[ b\breve \tweak style #'semipetrucci a \]

% final descending breve can be turned to flexa
% only with the "if penultimate note has no right stem" condition
  <>^"MB "   \[ b\maxima g\breve \]
  <>^"LLB "  \[ f\longa g f\breve \]

% "nonexistent" ligatures
  <>^"BfL "   \[ a\breve \tweak ligature-flexa ##t b\longa \]
  <>^"BfL "   \[ b\breve \tweak ligature-flexa ##t a\longa \]
  <>^"BuL "   \[ d\breve
                 \tweak right-up-stem ##t
                 e\longa \]
  <>^"BfuL "  \[ d\breve
                 \tweak ligature-flexa ##t
                 \tweak right-up-stem ##t
                 e\longa \]
  <>^"BfuL "  \[ a\breve
                 \tweak ligature-flexa ##t
                 \tweak right-up-stem ##t
                 d\longa \]
  <>^"dLL "   \[ \tweak right-down-stem ##t a\longa e\longa \]
  <>^"dLdL "  \[ \tweak right-down-stem ##t a\longa
                 \tweak right-down-stem ##t e\longa \]
  <>^"dLuL "  \[ \tweak right-down-stem ##t a\longa
                 \tweak right-up-stem ##t e\longa \]
  <>^"LLpL "  \[ d\longa c \tweak ligature-pes ##t g \]
  <>^"SpuL "  \[ c1 \tweak ligature-pes ##t \tweak right-up-stem ##t c'\longa \]
  <>^"MBpL "  \[ f\maxima c\breve \tweak ligature-pes ##t c'\longa \]
  <>^"MBpL "  \[ g\maxima e\breve \tweak ligature-pes ##t b\longa \]
  <>^"BLBpL " \[ f\breve g\longa c\breve \tweak ligature-pes ##t c'\longa \]
  <>^"BSfLB " \[ f\breve a1
                 \tweak ligature-flexa ##t
                 g\longa a\breve \]
  <>^"BLSS "  \[ a\breve b\longa d1 e \]
  <>^"SfSBpL" \[ g1 \tweak ligature-flexa ##t f d\breve
                 \tweak ligature-pes ##t a\longa \]
  <>^"SSBpL"  \[ g1 f d\breve \tweak ligature-pes ##t a\longa \]
  <>^"BBpM "  \[ e\breve d \tweak ligature-pes ##t a\maxima \]
  <>^"BfBpM " \[ e\breve \tweak ligature-flexa ##t d
                 \tweak ligature-pes ##t a\maxima \]

% ugly accidental placement
  <>^"BSS " \[ b\breve a bes \]
}


% Litterae:
%
% [1] Willi Apel: The Notation of Polyphonic Music. 900-1600.
% [2] Ulrich Michels: dtv-Atlas zur Musik, 1977.
%
