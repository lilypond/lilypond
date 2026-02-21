\version "2.27.0"

\header {
  texidoc = "Mensural ligatures found in 15-16th century manuscripts."
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


\new PetrucciStaff {
  \clef "petrucci-c4"
  \omit Score.TimeSignature
  \textLengthOn

% some ligatures from codex Chigi
% Ockeghem: Missa De plus en plus (f75v-87)
  \textMark "Ockeghem: Missa De plus en plus"

  <>^"LBBBB "     \[ d\longa c\breve f e d \]
  <>^"ML "        \[ c'\maxima g\longa \]
  <>^"dML "       \[ \tweak right-down-stem ##t c'\maxima d'\longa \]
  <>^"BBB "       \[ e'\breve d' c' \]
  <>^"LBBBfBB. "  \[ \override NoteHead.style = #'blackpetrucci
                     b\longa c'\breve d' g
                     \tweak ligature-flexa ##t
                     f
                     \revert NoteHead.style
                     g\breve. \]
  <>^"BBBBL "     \[ g\breve b c' e' d'\longa \]
  <>^"SSB "       \[ \override NoteHead.style = #'blackpetrucci
                     e'1 a g\breve
                     \revert NoteHead.style \]
  <>^"LBB "       \[ \override NoteHead.style = #'blackpetrucci
                     e'\longa f'\breve
                     \revert NoteHead.style
                     e' \]
  <>^"BBfBBfBBL " \[ \override NoteHead.style = #'blackpetrucci
                     b\breve g
                     \override NoteHead.ligature-flexa = ##t
                     \override NoteHead.flexa-width = #3
                     f f'
                     \override NoteHead.flexa-width = #5
                     b c'
                     \revert NoteHead.style
                     \revert NoteHead.flexa-width
                     \revert NoteHead.ligature-flexa
                     b\longa \]
  <>^"BBdM "      \[ g\breve b
                     \tweak right-down-stem ##t
                     c'\maxima \]

% four longae
  <>^"LLLL "      \[ g\longa b c' e' \]

% ascending stemless maximae
  <>^"MM "        \[ d'\maxima e' \]

% without joining it would be the same rhythm, with less ink
  <>^"LB "        \[ d\longa d'\breve \]

% without stem and join it would be the same rhythm, with less ink
  <>^"BBBBB "     \[ g\breve
                     \override NoteHead.style = #'blackpetrucci c e d
                     \revert NoteHead.style c \]
  \bar "|" \break

% some from the Requiem (f125v-136)
  \textMark "Ockeghem: Requiem"

  <>^"BB "         \[ d\breve d' \]
  <>^"SSBBBBBBBL " \[ a1 d e\breve f d f e f g e\longa \]
  <>^"BBBBL "      \[ c'\breve c d c c'\longa \]
  \bar "|" \break

  \textMark "various Vatican codices"

% MS S.Pietro B.80 (Vatican)
% f155r: longae and maximae
  <>^"dMLLML "     \[ \tweak right-down-stem ##t d\maxima f\longa g
                      a\maxima g\longa \]

% f158r: all maximae
  <>^"dM.MMMM "    \[ \tweak right-down-stem ##t d\maxima. f\maxima g a g \]

% f108v: mixed lengths, middle longae
  <>^"SfSLLBfB "   \[ d'1 \tweak ligature-flexa ##t b c'\longa d'
                      e'\breve \tweak ligature-flexa ##t d' \]

% f42r: same rhythm twice
  <>^"BdMBL "      \[ a\breve \tweak right-down-stem ##t c'\maxima
                      d'\breve c'\longa \]
  <>^"BMBL "       \[ b\breve a\maxima g\breve a\longa \]

% 118r: stem on final descending longa
  <>^"SSBuL "      \[ d1 f e\breve \tweak right-up-stem ##t d\longa \]

% 96v-97r, a canon in fifth: different ligatures in the two parts

% f167v (Capp.Sist.15 f227v): dotted middle longa
  <>^"L.BL.L "     \[ d'\longa. c'\breve d'\longa. f'\breve d'\longa \]

% Capp.Sist.35 f114v: pes after longa
  <>^"LpL "        \[ g\longa \tweak ligature-pes ##t c' \]
% f115r: dotted pes
  <>^"BBBpL. "     \[ g\breve d c \tweak ligature-pes ##t g\longa. \]

% f186r: middle longa lower than previous
  <>^"SSLL "       \[ d'1 c' a\longa b \]

% Capp.Sist.14 (after 1470)
% f90v, 91r: pes-like ligatures
  <>^"BpL "        \[ g\breve \tweak ligature-pes ##t c'\longa \]
  \clef "petrucci-f4"
  <>^"LpL "        \[ c\longa \tweak ligature-pes ##t f \]

% f89r: non-final ascending flexa
  <>^"SfSBM "      \[ c1 \tweak ligature-flexa ##t e d\breve c\maxima \]

% f119r: ascending flexa without stem
  <>^"BfB "        \[ a,\breve \tweak ligature-flexa ##t d \]

% Capp.Sist.51
% f63r: a fairly long one with a non-turning flexa
  <>^"BBBfBBBBBB " \[ g,\breve d f \tweak ligature-flexa ##t d c a, e a, d \]

% f11v: dotted breves
  \clef "petrucci-c3"
  <>^"B.B.B.B.B. " \[ e'\breve. g' c' d' g \]

% f12r: non-final ascending flexa
  \clef "petrucci-c4"
  <>^"SfSBBB "     \[ f1 \tweak ligature-flexa ##t g d\breve e c \]

% f128v etc.: several ternary XML ligature with descending end;
% maximae originally drawn without right stem,
  <>^"BML "        \[ a\breve c'\maxima b\longa \]
% but later stems were added,
% so the ending looks like that reads breve in Tr87 f10, f130v-131r (see below)
  <>^"BdML "       \[ a\breve \tweak right-down-stem ##t c'\maxima b\longa \]
% 101r: descending binary ML ligature untouched
  <>^"ML "         \[ a\maxima e\longa \]
% f168v: stemless middle (ascending) maxima
  <>^"BM.L "       \[ a\breve b\maxima. c'\longa \]

% f214v: the same four note motif in different note values;
% maximae and breves are presented unbroken,
% while longae and semibreves are broken into two binaries
  % <>^"M.M.M.M. L.L. L.L. B.B.B.B. S.S. S.S. "
  % \[ a\maxima. b c' b \] \[ a\longa. b \] \[ c' b \]
  % \[ a\breve. b c' b \] \[ a1. b \] \[ c' b \]
  \bar "|" \break

  \textMark "earlier Italian codices"

% Bodleian 213
% f105: pes after flexae
  <>^"BfBBfBpL "       \[ g\breve \tweak ligature-flexa ##t f
                          g \tweak ligature-flexa ##t d
                          \tweak ligature-pes ##t d'\longa \]
% f4v: ascending ligature with pes ending
% (small hollow heads without vertical protrusions
% so even a pes of a third is readable)
\clef "petrucci-c3"
  <>^"BBBBpL "        \[ e\breve f g a \tweak ligature-pes ##t c'\longa \]
% 138v: ascending pes after flexa
  <>^"LfBBpL "         \[ c'\longa \tweak ligature-flexa ##t f\breve a
                          \tweak ligature-pes ##t c'\longa \]
% f116v: pes after longa
  <>^"LpL "            \[ b\longa \tweak ligature-pes ##t e' \]

% f10r
  <>^"LL "            \[ f\longa f' \]

% SL ligatures at end of piece in all parts
% f89v-90 Velut: Un petit oyselet
  % tenor
  <>^"SdL "            \[ f1 \tweak right-down-stem ##t g\longa \]
  % contratenor
  <>^"SuL "            \[ c'1 \tweak right-up-stem ##t g\longa \]
  % unnamed top part
  % <>^"SuL "           \[ a1 \tweak right-up-stem ##t g\longa \]

% f37 Hugo de Lantins: Tra quante regione
% middle longa in mixed ligatures
  <>^"LLBL "           \[ g\longa a d'\breve c'\longa \]
  <>^"SSfBBLB "        \[ a1 d' \tweak ligature-flexa ##t c'\breve
                          d'\longa e'\breve \]
  <>^"BBfBLBL "        \[ g'\breve e' \tweak ligature-flexa ##t d'
                          f'\longa g'\breve e'\longa \]
% long ligature
  <>^"SSBfBB...BfBL "  \[ e'1 f' e'\breve \tweak ligature-flexa ##t d'
                          e' g' e' d'
                          \tweak flexa-width #3
                          \tweak ligature-flexa ##t a e'
                          d' \tweak ligature-flexa ##t c' d'\longa \]

% f56v: descending binary BL with up stem
  \clef "petrucci-c4"
  <>^"BuL "            \[ c'\breve \tweak right-up-stem ##t c\longa \]

% F-Pnm Italien 568, f25: long scale
  <>^"SSBBBB... "      \[ e1 f g\breve a b c' d' e' d' c' b \]

% f94: two ascending flexae
  <>^"SfSBfBBBBfB "   \[ a1 \tweak ligature-flexa ##t b a\breve
                         \tweak flexa-width #3
                         \tweak ligature-flexa ##t e c f d
                         \tweak flexa-width #5
                         \tweak ligature-flexa ##t d' \]

% f86: ascending flexa continued upwards
  <>^"SfSB "          \[ b1 \tweak ligature-flexa ##t c' d'\breve \]

% Aosta 15, f97v: flexa ends in longa
  <>^"BfuL "          \[ g\breve \tweak ligature-flexa ##t
                         \tweak right-up-stem ##t f\longa \]
% f108v:
  <>^"SfuL "          \[ g1 \tweak ligature-flexa ##t
                         \tweak right-up-stem ##t f\longa \]
% f117, f178v:
  <>^"BBfuL "         \[ g\breve a \tweak ligature-flexa ##t
                         \tweak right-up-stem ##t g\longa \]

% f194: ascending longa between breves
  <>^"SSBBLB "        \[ g1 a f\breve e g\longa a\breve \]

% f235v: pes-like LL
  <>^"LpL "           \[ c'\longa \tweak ligature-pes ##t f' \]

% f135: divisi written so that the lower note is joined to the previous,
% while the upper isn't (only to the lower), so looking like
  \clef "petrucci-f3"
  <>^"LBpL "          \[ d\longa g,\breve \tweak ligature-pes ##t d'\longa \]

% f281r: stems on final descending longae
  <>^"BL "            \[ d\breve \tweak right-down-stem ##t c\longa \]
  <>^"BBL "           \[ f\breve e \tweak right-down-stem ##t d\longa \]

% Q15, f135r: longa in the middle
  <>^"LBLBfBBL "      \[ f\longa a\breve c'\longa d'\breve
                         c' \tweak ligature-flexa ##t a b a\longa \]
% f299r: maxima in the middle
  <>^"BfBML "         \[ c'\breve \tweak ligature-flexa ##t b
                         c'\maxima g\longa \]

% f149v-150r: several SL (in a homophonic all-fermata section)
  % <>^"SL "            \[ d'1\fermata a'\longa\fermata \]
  <>^"SL "            \[ b1\fermata c'\longa\fermata \]
  <>^"SL "            \[ f1\fermata d\longa\fermata \]
  \bar "|" \break

% ligatures from the Trent codices
  \textMark "Trent codices"

  \clef "petrucci-c4"
% Tr87 f130r: final descending longa with stem
  <>^"SSBuL "     \[ a1 g f\breve \tweak right-up-stem ##t c\longa \]

% f16v: fermate over all notes, even before pes:
  <>^"BBBfBpL "   \[ a\breve\fermata g\fermata f\fermata
                     \tweak ligature-flexa ##t c\fermata
                     \tweak ligature-pes ##t c'\longa\fermata \]

% f131r: some ligatures with descending middle BL (none turned flexa):
  <>^"SSBLL "     \[ a1 f e\breve d\longa d' \]
  <>^"BLBBL "     \[ d\breve c\longa d\breve f e\longa \]
  <>^"SSBLB "     \[ c'1 d' b\breve a\longa c'\breve \]

% f130v: middle upward right stems:
  <>^"uMB "        \[ \tweak right-up-stem ##t g\maxima a\breve \]
  <>^"uLB "        \[ \tweak right-up-stem ##t c\longa d\breve \]

% f130v-131r: descendens LB written five times as if glueing a longa and a breve
% (and once regularly as a stemless flexa)
  <>^"dLB "        \[ \tweak right-down-stem ##t e'\longa d'\breve \]
  % <>^"dLB "        \[ \tweak right-down-stem ##t c'\longa a\breve \]
  % <>^"dLB "        \[ \tweak right-down-stem ##t g\longa d\breve \]
  % <>^"dLB "        \[ \tweak right-down-stem ##t c'\longa b\breve \]
  % <>^"dLB "        \[ \tweak right-down-stem ##t d\longa c\breve \]
% f10 ditto
  % <>^"dLB "        \[ \tweak right-down-stem ##t b\longa a\breve \]
  % <>^"dLB. "       \[ \tweak right-down-stem ##t a\longa g\breve. \]

% f9v: an incredible left down stem in the middle
  % <>^"BlL "       \[ d'\breve \tweak left-down-stem ##t c\longa \]

% f10 another inexplicable ligature (or just a typo??)
% the middle note is a longa but has no stem, just a signum congruentiae
% without any other part having or needing one, looking like
  % <>^"LBdL. "     \[ f\longa g\breve\signumcongruentiae
  %                    \tweak right-down-stem ##t f\longa. \]

% f241r: 3 flexae:
  <>^"BfBBfBBB "  \[ d\breve \tweak ligature-flexa ##t c
                     c' \tweak ligature-flexa ##t f a g \]
  \break

% Tr92, f126r: mixed lengths
% (in Tr87 f151r the first ligature is the same,
% the second one starts one note later)
  <>^"B.BfBLB.M " \[ c'\breve. a\breve \tweak ligature-flexa ##t g
                     c'\longa d'\breve. c'\maxima \]
  <>^"LMBBBL "    \[ g\longa c'\maxima d'\breve e' d' c'\longa \]

% f147v
  <>^"dMBBM "     \[ \tweak right-down-stem ##t c'\maxima d'\breve b a\maxima \]

% f128r: middle longa higher than the next note
% (ligature at the end of section, so final note is metrically not exact)
  <>^"SSLB "      \[ f1 b a\longa g\breve \]

% f156r: maxima with upward stem
  <>^"dMuM "      \[ \tweak right-down-stem ##t a\maxima
                     \tweak right-up-stem ##t c' \]

% Tr90, f224r (and Tr93, f296v): redundant stems on maximae
  <>^"dMuM "      \[ \tweak right-down-stem ##t f\maxima
                     \tweak right-up-stem ##t e \]

% f14v: initial descending longa with stem
  <>^"dLBB "      \[ \tweak right-down-stem ##t f\longa d\breve f \]

% f336v: mixed lengths
  <>^"MdMBBL "    \[ d\maxima \tweak right-down-stem ##t e
                     a\breve b d'\longa \]

% f336v: penultimate longa higher than final which has a right stem
% to distinguish from those seen in Tr87, f131r
  <>^"LLdL "      \[ a\longa b \tweak right-down-stem ##t a \]
% other sources ligate only the latter two notes: a\longa \[ b a \]

% f356v: large intervals
  <>^"BfBL "      \[ c'\breve \tweak ligature-flexa ##t c c'\longa \]

% f387r: LL descending an octave
  <>^"LL "        \[ c'\longa c \]

% Tr88, f4v: old style LL
  <>^"LpL "       \[ g\longa \tweak ligature-pes ##t d' \]

% f393v: pes-style ML
  <>^"dMpL "      \[ \tweak right-down-stem ##t d\maxima
                     \tweak ligature-pes ##t a\longa \]

% f5r: flexa higher than next note
  <>^"SSfBL "     \[ g1 b \tweak ligature-flexa ##t a\breve d\longa \]

% f326v-327r: inconsistent ligatures, but in a not really metric passage,
% an Amen consisting of 7 fermata chords: in two parts written as all longae
% (LL LL LL L and L LL LL L L), in one part as BL BL BL L.

% Tr89 (from around 1470), f117v: porrectus
  <>^"BBpL "      \[ f\breve c \tweak ligature-pes ##t g\longa \]

% f299v: maximae
  <>^"MMMMMMdM "  \[ f\maxima g a g f e \tweak right-down-stem ##t f \]

% Tr91, f24v, a long one (broken into six in Capp.Sist.51)
  \clef "petrucci-c5"
  <>^"BfBBBB... " \[ f\breve \tweak ligature-flexa ##t d e f d b,
                     a, d c f g d c b, c \]

% f86v: a slightly shorter one with maximae in the middle
% (same notes are broken into two in f89v)
  \clef "petrucci-c4"
  <>^"BfB..MMBBB" \[ a\breve \tweak ligature-flexa ##t g a d e f e
                     d\maxima c e\breve f g \]

% f148r: a very long one
  <>^"BBB... "    \[ a\breve b c' \tweak ligature-flexa ##t g a g
                     \tweak ligature-flexa ##t f g
                     \tweak ligature-flexa ##t f a c' d' f' c' d'
                     \tweak ligature-flexa ##t a b a
                     \tweak ligature-flexa ##t g b a \]

% f178v: divisi on the last note (similar ligatures on f185r)
% currently not handled by LilyPond...
  % <>^"SSBL "      \[ a1 g \tweak ligature-flexa ##t d\breve <g d'>\longa \]
% ... but looks loke a pes ending:
  <>^"SSfBLpL "   \[ a1 g \tweak ligature-flexa ##t d\breve
                     g\longa \tweak ligature-pes ##t d' \]

% f241r: a jumpy one with a middle longa
  <>^"BfBBLB "    \[ g\breve \tweak ligature-flexa ##t c g c\longa g\breve \]
  \bar "|" \break

% ligatures from the Old Hall manuscript
% (featuring black mensural notation, void notes occur only as coloratio)
  \textMark "Old Hall"

% jumping sequence of semibreves
% f65 Patrem
  \clef "petrucci-f3"
  <>^"SSSfS "    \[ a1 b f \tweak ligature-flexa ##t c \]

% six consecutive semibreves
% f65v Pycard: Patrem
  <>^"SSSSSS "   \[ f1 a f a g b \]

% semibreves start in the middle
% f38 Power: Beata progenies
  <>^"BSfS "     \[ g\breve a1 \tweak ligature-flexa ##t g \]

% f103: four semibreves, then something else
  <>^"SSSSB "    \[ f1 g a g f\breve \]

% f46v: ascending flexa with stem
  <>^"lBfB "     \[ \tweak left-down-stem ##t g\breve
                    \tweak ligature-flexa ##t c' \]

% f85: non-standard ternary
  <>^"lBBfB "    \[ \tweak left-down-stem ##t d\breve f
                    \tweak ligature-flexa ##t g \]

% f63: semi-colored flexa
  <>^"BB "       \[ \override NoteHead.style = #'semipetrucci
                    a\breve g \revert NoteHead.style \]

% a long one
% f17 Power: Et in terra
  <>^"LBBBB... " \[ b,\longa c\breve d c b, g f a g f c d c \]
  \bar "|" \break

% rare ligatures from the Caius and Lambeth Choirbooks
% (both written in the 1520s, featuring black mensural notation,
% hollow heads appear only as coloratio)
  \textMark "Caius/Lambeth choirbooks"

% Fayrfax: Missa O bone Jesu
  \clef "petrucci-f5"
  <>^"SSSS "   \[ e1 d c b, \]
  <>^"lBB "    \[ \once \override NoteHead.left-down-stem = ##t
                  d\breve f \]
  <>^"SSuL "   \[ d1 c
                  \once \override NoteHead.right-up-stem = ##t
                  b,\longa \]
  <>^"BBdL "   \[ f,\breve e,
                  \once \override NoteHead.right-down-stem = ##t
                  d,\longa \]

% adding stem to the longa of a binary BL
% Ludford: Missa Christi virgo
  \clef "petrucci-c4"
  <>^"BdL "    \[ e'\breve \tweak right-down-stem ##t c'\longa \]

% dot before and on first note of flexa
% Fayrfax: Missa O quam glorifica as in the Lambeth choirbook
% (the same location in the Caius choirbook
% finishes the ligature on the \breve., separating the \breve)
  <>^"SS.B.B " \[ a1 g1. f\breve. e\breve \]

% Lambeth choirbook f60 (Sturton Ave Maria): final longa drawn pes-style:
% shorter and very thin, since there's just so much place.
% the tenor part has several pes-style longae,
% while final longae in other parts are all regular (virga-like)
  <>^"BBBpL "  \[ d'\breve c' a \tweak ligature-pes ##t b\longa \]
  <>^"BBpL "   \[ a\breve g \tweak ligature-pes ##t a\longa \]

% Lambeth f68: upward flexa in non-binary ligature
  <>^"SSBfB "  \[ g1 f d\breve \tweak ligature-flexa ##t g \]
  \bar "|" \break
}
