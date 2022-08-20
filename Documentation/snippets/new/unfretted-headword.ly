\version "2.23.10"

\header {
  lsrtags = "headword"

  texidoc = "
Unfretted headword
"

  doctitle = "Unfretted headword"
}


% #!lilypond lcp-extract.ly -*- coding: utf-8; -*-

%%%
%%% les-cinq-pieds:
%%% extract for the lilypond documentaton project
%%%

%% Title:	Les cinq pieds
%% Composer: 	David Séverin
%% Date:	Juillet 2007
%% Instrument:	Violon Solo
%% Dedication:	A mon épouse Lívia De Souza Vidal
%% Additional:	avec l'aide de Krzysztof Wagenaar

%% Statement:

%% Here by, I, the composer, agree that this extract of my composition
%% be in the public domain and can be part of, used and presented in
%% the LilyPond Documention Project.

%% Statement Date: Octber the 9th, 2008


%%%
%%% Abbreviations
%%%

db         = \markup { \musicglyph "scripts.downbow" }
dub        = \markup { \line { \musicglyph "scripts.downbow" " " \musicglyph "scripts.upbow" } }
dubetc     = \markup { \line { \musicglyph "scripts.downbow" " " \musicglyph "scripts.upbow" "..." } }

ub         = \markup { \musicglyph "scripts.upbow" }
udb        = \markup { \line { \musicglyph "scripts.upbow" " " \musicglyph "scripts.downbow" } }
udbetc     = \markup { \line { \musicglyph "scripts.upbow" " " \musicglyph "scripts.downbow" "..." } }

accel   = \markup \tiny \italic \bold "accel..."
ritar   = \markup \tiny \italic \bold "ritar..."

ignore     = \override NoteColumn.ignore-collision = ##t


%%
%% Strings
%%

svib           = \markup \small "s. vib."
pvib           = \markup \small "p. vib."
mvib           = \markup \small "m. vib."
sulp           = \markup \small "s.p."
norm           = \markup \small "n."

quatre         = \markup \teeny "IV"


%%
%% Shifting Notes
%%

shift      = \once \override NoteColumn.force-hshift = #0.9
shifta     = \once \override NoteColumn.force-hshift = #1.2
shiftb     = \once \override NoteColumn.force-hshift = #1.4


%%
%% Hairpin
%%

% aniente        = "a niente"
aniente        = \once \override Hairpin.circled-tip = ##t


%%
%% Tuplets
%%

tupletbp       = \once \override Staff.TupletBracket.padding = #2.25


%%
%% Flag [Note Head - Stem]
%%

noflag         = \once \override Flag.style = #'no-flag

%%%
%%% Functions
%%%

#(define-markup-command (colmark layout props args)
  (markup-list?)
  (let ((entries (cons (list '(baseline-skip . 2.3)) props)
       ))
   (interpret-markup layout entries
    (make-column-markup
     (map (lambda (arg)
	   (markup arg))
      (reverse args))))))


%%%
%%% Instruments
%%%

ViolinSolo = \relative c' {

  \voiceOne

  \set Score.rehearsalMarkFormatter           = #format-mark-box-numbers
  \override Score.VoltaBracket.font-name      = "LilyPond Sans Serif"
  \override Score.VoltaBracket.extra-offset   = #'(0 . 1)
  \override SpacingSpanner.uniform-stretching = ##t


  %% Measure 1
  \time 25/8
  \mark \default
  r2 ^\markup \colmark { \italic "fatigué" " " \bold "lentement"} r4 r r8
  <<
    { \shift d2 \glissando ^\markup \colmark { \quatre \dubetc \svib } \shifta e1 } \\
    { d2 \open \mf \< ~ \aniente d1  \! \> r4 r ^\markup \colmark { " " \fermata } \! }
  >>


  %% Measure 2
  \time 7/4
  \set Score.repeatCommands = #'((volta "1) n.      2) s.p."))
  <<
    { \shift d2 \glissando ^\markup \colmark { \quatre \udbetc } \shifta e1 } \\
    { d2 \open \mf \< ~ d1 \! \> ~ d4 ^\markup \colmark { " " \fermata } \! }
  >>
  \set Score.repeatCommands = #'((volta #f))


  %% Measure 3
  \time 15/4
  <<
    { \shift d2 \glissando ^\markup \colmark { \quatre \dubetc \pvib \norm } \shifta e1 \glissando d2 } \\
    { d2 \open \mf \< ~ d1 ~ 2 \ff  ~ d1 \> ~ d2 ^\markup \colmark { " " " " \svib } ~ d4 \pp}
  >>
  \break

  %% Measure 4
  \time 4/4
  \stemUp
  \tupletDown
  \tuplet 3/2 { d4 ^\markup \colmark { \quatre \db \accel } d d }
  \tuplet 3/2 { d4 ^\markup \colmark { " " \db " " \sulp } d d }



  %% Measure 5
  \time 5/4
  \tupletbp \tuplet 3/2 { d8 \mf \< ^\markup \colmark { \quatre \db \norm } d _\open d }
  \tupletbp \tuplet 3/2 { d8 ^\markup \colmark { " " \db \sulp } d _\open d }
  \tupletbp \tuplet 3/2 { d16 ^\markup \colmark { " " \db \norm } d _\open d d d _\open d }
  d2 \ff ^\markup \colmark { " " \pvib } \>


  %% Measure 6
  \time 5/8
  \once \override Beam.grow-direction = #RIGHT  % \featherDurations 2/3
  { d16 \staccato
    [ d \staccato d \staccato d \staccato d \staccato d \staccato d \staccato d \staccato d \staccato d \staccato]
  }
  \break


  %% Measure 7
  \time 7/4
  \tupletbp \tuplet 3/2 { d16 ^\markup \colmark { \quatre } d _\open d d d _\open d }
  \tupletbp \tuplet 3/2 { d8 ^\markup \colmark { " " \db } d _\open d }
  \tupletbp \tuplet 3/2 { d8 ^\markup \colmark { " " \db " " \sulp } d _\open d }
  \tuplet 3/2 { d4 ^\markup \colmark { \quatre \db \ritar \norm } d d }
  \tuplet 3/2 { d4 ^\markup \colmark { " " \db " " \sulp } d d \ppp ~ }


  %% Measure 8
  d4 ^\markup \colmark { " " " " \pvib \norm }
  deh2 d dih \<


  %% Measure 9
  <<
    { \shift d2 \glissando ^\markup \colmark { \quatre } \shifta e1 } \\
    { d2 \open ~ d1  ^\markup \colmark { " " " " \mvib } }
  >>
  \breathe r4 \!

}


%%%
%%% Score
%%%

\score {

  <<
    \relative c' <<
      \new Staff \ViolinSolo
    >>

    \hide Score.Rest
    \set Score.measureBarType = ""
  >>

  \layout  {
    indent       = 0.0
    \context {
      \Staff
      \remove "Time_signature_engraver"
    }
    \context {
      \Score
      \remove "Bar_number_engraver"
    }
  }
}
