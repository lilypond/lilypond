\version "1.7.17"
\header {
    title	= "Puer natus est nobis"
    subtitle	= "Antiphona ad introitum VII"
    texidoc = "This file pretty nicely demonstrates what still does
not work among ligatures: (i) horozontal spacing is totally broken as
soon as raggedright mode is on (say \"raggedright = ##f\" in the paper
block to see); (ii) the clef does not show on each line; (iii) lily
crashes when removing Stem_engraver, but still using Slur_engraver
(this is useful needed for the \addlyrics feature when no slurs are to
be typeset); (iv) episem causes a syntax error when applied inside of
ligatures (but they are useful only inside...); (v) support for
augmentum dots is missing; (vi) accidentals must be placed before the
ligature (not demonstrated in this example)"

}

\include "paper26.ly"
\include "gregorian-init.ly"

cantus = \notes \transpose c c {
  \[ g4
    (
    \pes
    )
    d'
  \]
  d'
  \[ d'
    (
    \pes e' \flexa
    )
    d'
  \]
  c'
  \[ c'
    (
    c'
    )
    c'
  \]
  \[ d'
    (
    \flexa c' e' \flexa
    )
    d' d'
  \]
  \divisioMaior
  \[ g
    (
    \pes
    )
    d'
  \]
  \[ d'
    (
    \pes e' \flexa
    )
    d'
  \]
  \[ c'
    (
    \flexa
    )
    b
  \]
  a
  \[ c'
    (
    c' \pes
    )
    d'
  \]
  c' c'
  \[ c'
    (
    \pes d' \flexa c'
    )
    c'
  \]
  \[ g
    (
    \pes a \flexa
    )
    g
  \]
  \divisioMaxima
  g a c' \[ b
    (
    \pes d' e' \pes
    )
    f'
  \]
  \[ d'
    (
    \flexa
    )
    c'
  \]
  c'
  \divisioMinima
  c' c'
  \[ d'
    (
    \flexa c' e' \flexa
    )
    d'
  \]
  \[ c'
    (
    \flexa
    )
    b
  \]
  \[ c'
    (
    c'
    )
    c'
  \]
  \[ c'
    (
    \flexa a c' \flexa b \virga c'
    \inclinatum \deminutum b \inclinatum \deminutum
    )
    a
  \]
  \[ b
    (
    \flexa
    )
    a
  \]
  \divisioMaxima
  \[ c'
    (
    \flexa
    )
    b
  \]
  c'
  \[ c'
    (
    \pes
    e'
    \flexa
    )
    d'
  \]
  c'
  \[ c'
    (
    c'
    )
    c'
  \]
  c'
  \[ c'
    (
    c'
    )
    c'
  \]
  \[ c'
    (
    \pes
    d' \flexa b \virga c'
    \inclinatum \deminutum b \inclinatum \deminutum
    )
    a
  \]
  \[ b
    (
    \flexa
    )
    a
  \]
  \divisioMaior
  \[ c'
    (
    \pes
    )
    e'
  \]
  d'
  \[ g
    (
    \pes \semivocalis % epiphonus
    )
    c'
  \]
  c'
  \[ c'
    (
    c' c' \flexa
    )
    a
  \]
  a
  \[ %{ FIXME: \episemInitium%} a
    (
    \pes c' \flexa %{ FIXME: \episemFinis%} a \quilisma b
    )
    c'
  \]
  \[ g
    (
    \pes a \flexa
    )
    g
  \]
  g
  \finalis
}

verba = \context Lyrics = verba \lyrics {
  Pu- er na- tus est no- bis,
  et fi- li- us da- tus est no- bis:
  cu- ius im- pe- ri- um
  su- per hu- me- rum e- ius:
  et vo- ca- bi- tur no- men e- ius,
  mag- ni con- si- li- i An- ge- lus.
  Can- ta- te Do- mi- no can- ti- cum no- vum:
  qui- a mi- ra- bi- li- a fe- cit.
}

\score {
  \context StaffGroup <
    \context VaticanaStaff <
      \context VaticanaVoice <
	\addlyrics
	\cantus
	\verba
      >
    >
%    \context Staff <
%      \cantus
%    >
  >
  \paper {
    stafflinethickness = \staffspace / 5.0
    linewidth = 137.0
%   width = 137.0
    indent = 0.0
    raggedright = ##f
    packed = ##t

%    width = 15.0 \cm %%% no effect?
%    gourlay_maxmeasures = 30.
%    gourlay_maxmeasures = 1. %%% no effect?
%    arithmetic_basicspace = 3.8
%    arithmetic_basicspace = 0.0 %%% no effect?
%    arithmetic_multiplier = 8.\pt
%    arithmetic_multiplier = 0.\pt %%% no effect?
%
    \translator {
      \VoiceContext
      \name VaticanaVoice
      \alias Voice

      % Can not `\remove "Slur_engraver"', since \addlyrics needs
      % slurs working.  Hence, set slurs transparent instead.
      Slur \override #'transparent = ##t

      % N.B.: Warnings of the type "Degenerate bow: infinite steepness
      % reqd" arise from the fact that all noteheads of a ligature are
      % in the same paper column.  Therefore, the (transparent) slurs
      % often start and end in the same column, producing the above
      % warning.  TODO: supress this warning; we need slurs only for
      % \addlyrics, not for printing.

      % `\remove "Stem_engraver"' currently produces a crash (see
      % below).  Hence, set stems transparent instead.
      Stem \set #'transparent = ##t

%{

FIXME:

When removing Stem_engraver from VoiceContext (but still using
Slur_engraver), lily will crash while trying to typeset slurs, since
`()' is not a valid stem:

In unknown file:
   ?: 0* [Slur::after_line_breaking #<Grob Slur >]
   ?: 1* [#<procedure #f (slur dir)> #<Grob Slur > -1]
In /home/reuter/project/lilypond-1.7/share/lilypond/scm/slur.scm:
  43: 2* [not ...
  43: 3*  [attached-to-stem #<Grob Slur > -1]
  10: 4   (let* ((note-columns #) (col #) (stem #)) (and (eq? col #) stem ...))
    ...
  16: 5   [ly:get-grob-property () heads]

/home/reuter/project/lilypond-1.7/share/lilypond/scm/slur.scm:16:6: In procedure ly_get_grob_property in expression (ly:get-grob-property stem (quote heads)):
/home/reuter/project/lilypond-1.7/share/lilypond/scm/slur.scm:16:6: Wrong type argument in position 1 (expecting grob): ()

%}

      \remove Ligature_bracket_engraver
      \consists Vaticana_ligature_engraver
      NoteHead \set #'style = #'vaticana_punctum
      Script \set #'padding = #0.0

      % prepare TextSpanner for \episem{Initium|Finis} use
      TextSpanner \set #'type = #'line
      TextSpanner \set #'edge-height = #'(0 . 0)
      TextSpanner \set #'padding = #0.5
      TextSpanner \set #'edge-text = #'("" . "")
    }
    \translator {
      \VoiceContext
      \name GregorianTransciptionVoice
      \alias Voice
      Stem \set #'transparent = ##t
      % \remove "Stem_engraver"
      % Slur \override #'transparent = ##t
      \remove "Slur_engraver"
    }
    \translator {
      \StaffContext
      \name GregorianTranscriptionStaff
      \alias Staff
      \accepts GregorianTranscriptionVoice
      BarLine \override #'transparent = ##t
    }
    \translator {
      \StaffContext
      \name VaticanaStaff
      \alias Staff
      \accepts VaticanaVoice
      \remove Bar_engraver
      \consists Custos_engraver
      clefGlyph = #"clefs-vaticana_do"
      centralCPosition = #0
      clefPosition = #1
      clefOctaviation = #0
      StaffSymbol \set #'line-count = #4
      StaffSymbol \set #'width = #60.0 % FIXME: unit should be \linewidth
      TimeSignature \set #'transparent = ##t
      KeySignature \set #'style = #'vaticana
      Accidental \set #'style = #'vaticana
      Custos \set #'style = #'vaticana
      Custos \set #'neutral-position = #3
      Custos \set #'neutral-direction = #-1
      Custos \set #'adjust-if-on-staffline = ##t
    }
    \translator {
      \HaraKiriStaffContext
      \accepts VaticanaVoice
      \accepts GregorianTranscriptionVoice
    }
    \translator {
      \ScoreContext
      \accepts VaticanaStaff
      \accepts GregorianTranscriptionStaff
      \remove Bar_number_engraver
%      SpacingSpanner \set #'spacing-increment = #0.5
      timing = ##f

      % Don't do this exception for transcription -- it will produce
      % additional space
      barAlways = ##t
%      skipBars = ##t
    }
  }
}
