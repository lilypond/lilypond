\version "1.7.27"
\header {
    title	= "Puer natus est nobis"
    subtitle	= "Antiphona ad introitum VII"
    texidoc = "

Demonstrate gregorian chant notation

This file pretty nicely demonstrates what still does
not work among ligatures: (i) horizontal spacing between ligatures and
lyrics aligment is broken; (ii) the clef does not show on each line;
(iii) lily crashes when removing Stem_engraver, but still using
Slur_engraver (this is useful needed for the \addlyrics feature when
no slurs are to be typeset); (iv) episem causes a parse error (this
used to work a while ago); (v) pitches are typeset half a staff space
too high (the second note is a d, but it appears on the line of the do
clef; this bug must have been introduced only recently); (vi) support
for augmentum dots is missing; (vii) accidentals must be placed before
the ligature (not demonstrated in this example)."

}

\include "paper26.ly"
\include "gregorian-init.ly"

%%% N.B.: Yes, I know, the formatting of the following looks awful,
%%% but this is intentional for editorial purposes (simplifies some
%%% global search/replace operations in emacs).

cantus = \notes \transpose c c {
  \[ g4
    (		%%% Pu-
    \pes
    d'-)
  \]
  d'		%%% er
  \[ d'
    (		%%% na-
    \pes e' \flexa
    d'-)
  \]
  c'		%%% tus
  \[ c'
    (		%%% est
    c'
    c'-)
  \]
  \[ d'
    (		%%% no-
    \flexa c' e' \flexa
    d'-)
  \]
  d'            %%% bis,
  \divisioMaior
  \[ g
    (		%%% et
    \pes \deminutum
    d'-)
  \]
  \[ d'
    (		%%% fi-
    \pes e' \flexa
    d'-)
  \]
  \[ c'
    (		%%% li-
    \flexa
    b-)
  \]
  a		%%% us
  \[ c'
    (		%%% da-
    c' \pes
    d'-)
  \]
  c'		%%% tus-
  c'		%%% est
  \[ c'
    (		%%% no-
    \pes d' \flexa c'
    c'-)
  \]
  \[ g
    (		%%% bis:
    \pes a \flexa
    g-)
  \]
  \divisioMaxima
  g		%%% cu-
  a		%%% us
  c'		%%% im-
  \[ b
    (		%%% pe-
    \pes d' e' \pes
    f'-)
  \]
  \[ d'
    (		%%% ri-
    \flexa
    c'-)
  \]
  c'		%%% um
}

verba = \context Lyrics = verba \lyrics {
  Pu- er na- tus est no- bis,
  et fi- li- us da- tus est no- bis:
  cu- ius im- pe- ri- um
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
  >
  \paper {
    stafflinethickness = \staffspace / 5.0
    linewidth = 137.0\mm
    width = 137.0\mm
    indent = 0.0
    raggedright = ##t
    packed = ##t
%   width = 15.0 \cm %%% no effect?

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
      % \remove "Stem_engraver"

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
      TextSpanner \set #'style = #'line
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
      clefOctavation = #0
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

      % Don't do this except for transcription -- it will produce
      % additional space
      barAlways = ##t
%      skipBars = ##t
    }
  }
}
