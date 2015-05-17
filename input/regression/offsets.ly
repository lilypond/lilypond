\version "2.19.21"

\header {
  texidoc = "The @code{\\offset} command may be used to displace various properties
from the default settings contained in grob descriptions.  Settings which may be
offset are limited to those of type @code{number}, @code{number-pair}, or
@code{number-pair-list}.  Most of the following examples begin with the grob in its
default appearance.  The command is demonstrated as a tweak and as an override."
}

\layout {
  ragged-right = ##t
  indent = 0
}

\relative {

  %% ARPEGGIO %%
  % default
  <c' e g b>1\arpeggio
  <c e g b>1-\offset positions #'(-1 . 1) \arpeggio
  \bar "||"

  %% BREATHING SIGN %%
  % default
  c1 \breathe
  c1
  \once \offset Y-offset 1 BreathingSign
  \breathe
  \bar "||"

  %% DYNAMICS %%
  % default
  c1\f
  \once \offset X-offset #-1 DynamicText
  c1\f
  % DynamicLineSpanner
  \once \offset padding 1 DynamicLineSpanner
  c1\f
  \bar "||"

  %% BEAMS %%
  % default
  c'8 d e f
  \once \offset positions #'(-1 . -1) Voice.Beam
  c8 d e f
  % same effect as an offset of '(-2 . -2)
  \once \offset positions #-2 Beam
  c8 d e f
  \override Beam.breakable = ##t
  c8-\offset positions #'((-1 . -3) (-3 . -1)) [ d e f
  \break
  g8 f e d] c-\offset beam-thickness 0.48 [ d e f]
  \bar "||"

  %% TEXT SPANNERS %%
  c4\startTextSpan d e f\stopTextSpan
  \once \offset dash-fraction #'(0.1 0.3) TextSpanner
  \once \offset staff-padding #'(1.0 2.0) TextSpanner
  c4\startTextSpan d e f
  \break
  c4 d e f\stopTextSpan
  \bar "||"

  %% SLURS %%
  % this duplicates the effect of the \shape command
  \offset control-points #'(
   ((0 . 0) (0 . 1) (0 . 2) (0 . 1))
   ((1 . 0) (0 . 4) (0 . 4) (0 . 0))
   ) Slur
  c4-\offset line-thickness #'(0 10) ( d e f
  \break
  c4 d e f)
  \bar "||"

  %% ACCIDENTAL, STEM %%
  % this illustrates use of \offset as a directed tweak
  cis2
  \offset AccidentalPlacement.right-padding 0.5
  \offset Stem.thickness 4.0
  cis!2
}
