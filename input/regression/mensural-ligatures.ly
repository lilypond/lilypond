\version "2.23.14"

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
    \remove "Ligature_bracket_engraver"
    \consists "Mensural_ligature_engraver"

    \override TextScript.padding = #3
  }
  \context {
    \Score
    \override SpacingSpanner.packed-spacing = ##t
    \override PaperColumn.keep-inside-line = ##f
    \override TextMark.padding = #7
    \override TextMark.font-series = #'bold
    \override TextMark.font-size = #0
  }
}


\context Voice {
  \clef "petrucci-c4"
  \omit Score.TimeSignature
  \cadenzaOn % turn off bar lines
  \textLengthOn

% ligaturae binariae
  \textMark "ligaturae binariae"

  <>^"BL "  \[ b\breve g\longa \]
  <>^"BL "  \[ g\breve b\longa \]
  <>^"LL "  \[ b\longa g \]
  <>^"LL "  \[ g\longa b \]
  <>^"BB "  \[ b\breve g \]
  <>^"BB "  \[ g\breve b \]
  <>^"LB "  \[ b\longa g\breve \]
  <>^"LB "  \[ g\longa b\breve \]
  <>^"SS "  \[ b1 g \]
  <>^"SS "  \[ g1 b \]
  <>^"SL "  \[ b1 g\longa \]
  <>^"SL "  \[ g1 b\longa \]
  <>^"SMx " \[ b1 g\maxima \]
  <>^"SMx " \[ g1 b\maxima \]

  % "unofficial", but unambiguous
  <>^"BS " \[ b\breve g1 \]
  <>^"BS " \[ g\breve b1 \]
  \bar "|" \break

% ligaturae ternariae, quaternariae, etc. (sicut in Apel[1])
  \textMark "ligaturae ternariae, quaternariae, etc."

  <>^"BBL "     \[ b\breve a g\longa \]
  <>^"BBBB "    \[ a\breve g a b \]
  <>^"SSBBBLB " \[ b1 a g\breve a b a\longa b\breve \]
  <>^"LBMxBL "  \[ a\longa g\breve a\maxima b\breve a\longa \]
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
  \bar "|" \break

% some ligatures from Ockeghem: Missa De plus en plus
  \textMark "Ockeghem: Missa De plus en plus"

  <>^"MxMx "    \[ c'\maxima g \]
  <>^"LBBBB "   \[ d\longa c\breve f e d \]
  <>^"MxL "     \[ c'\maxima d'\longa \]
  <>^"BBB "     \[ e'\breve d' c' \]
  <>^"LBBBBB. " \[ \override NoteHead.style = #'blackpetrucci
                   b\longa c'\breve d' g
                   \once \override NoteHead.ligature-flexa = ##t
                   f
                   \revert NoteHead.style
                   g\breve. \]
  <>^"BBBBL "   \[ g\breve b c' e' d'\longa \]
  <>^"SSB "     \[ \override NoteHead.style = #'blackpetrucci
                   e'1 a g\breve
                   \revert NoteHead.style \]
  <>^"LLLL "    \[ g\longa b c' e' \]
  <>^"LBB "     \[ \override NoteHead.style = #'blackpetrucci
                   e'\longa f'\breve
                   \revert NoteHead.style
                   e' \]
  <>^"BBBBBBL " \[ \override NoteHead.style = #'blackpetrucci
                   b\breve g
                   \override NoteHead.ligature-flexa = ##t
                   \override NoteHead.flexa-width = #3
                   f f'
                   \override NoteHead.flexa-width = #5
                   b c'
                   \revert NoteHead.style
                   % though ligature-flexa is still ##t,
                   % this pair must be drawn as recta
                   b\longa
                   \revert NoteHead.flexa-width
                   \revert NoteHead.ligature-flexa \]
  <>^"BBMx "    \[ g\breve b
                   \once \override NoteHead.right-down-stem = ##t
                   c'\maxima \]
  \bar "|" \break

% some from the Requiem
  \textMark "Ockeghem: Requiem"

  <>^"SSBBBBBBBL " \[ a1 d e\breve f d f e f g e\longa \]
  <>^"BBBBL "      \[ c'\breve c d c c'\longa \]
  \bar "|" \break

% rare ligatures from the Caius Choirbook
  \textMark "Fayrfax: Missa O bone Jesu"

  \clef "petrucci-f5"
  <>^"SSSS "       \[ e1 d c b, \]
  <>^"BB "         \[ \once \override NoteHead.left-down-stem = ##t
                      d\breve f \]
  <>^"SSL "        \[ d1 c
                      \once \override NoteHead.right-up-stem = ##t
                      b,\longa \]
  <>^"BBL "        \[ f,\breve e,
                      \once \override NoteHead.right-down-stem = ##t
                      d,\longa \]
  \bar "|" \break
}


\context Staff \with {
  \override StaffSymbol.line-count = 4
  \omit TimeSignature
} {
  \clef "petrucci-c5"
  \set Staff.printKeyCancellation = ##f
  \cadenzaOn % turn off bar lines
  \accidentalStyle forget
  \textLengthOn

  \textMark "ignored tweaks"
  <>^"BSS "                \[ a\breve
                              \once \override NoteHead.ligature-flexa = ##t
                              d1 e \]
  <>^"BSS "                \[ e\breve
                              \once \override NoteHead.ligature-flexa = ##t
                              f1 g \]
  <>^"LB "                 \[ \once \override NoteHead.left-down-stem = ##t
                              g\longa
                              \once \override NoteHead.right-down-stem = ##t
                              f\breve \]
  <>^"BL "                 \[ f\breve
                              \once \override NoteHead.right-up-stem = ##t
                              g\longa \]
  <>^"MxMx "               \[ \override NoteHead.right-up-stem = ##t
                              g\maxima b \]
  \bar "|" \break

% crazy ligatures
  \textMark "crazy ligatures"

  <>^"BBBBB "              \[ c\breve e f g bes \]
  % TODO: accidentals must be collected and printed before ligature
  <>^"BB "                 \[ bes\breve a \]
  <>^"B.B.B.B.B.B.B.B.B. " \[ a\breve. g b a e g f a g \]
  % TODO: the first dot is too high to avoid a non-existent (ledger) line
  <>^"B.B. "               \[ b a \]
  <>^"BSLB "               \[ f\breve a1 g\longa a\breve \]
  <>^"BSS "                \[ d\breve a1 e \]
  <>^"BLSS "               \[ a\breve b\longa d1 e \]
  \bar "|" \break

% invalid ligatures (those commented out are rejected with explanation)
  \textMark "invalid ligatures"

% <>^"SS "   \[ a1 as \]
  <>^"BBB "  \[ a\breve g as \]
% <>^"LLB "  \[ f\longa g f\breve \]
% <>^"BSB "  \[ f\breve a1 g\breve \]
}


% Litterae:
%
% [1] Willi Apel: The Notation of Polyphonic Music. 900-1600.
% [2] Ulrich Michels: dtv-Atlas zur Musik, 1977.
%
