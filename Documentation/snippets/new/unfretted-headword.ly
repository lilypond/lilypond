\version "2.25.18"

\header {
  categories = "Headword"

  texidoc = "
Unfretted headword.
"

  doctitle = "Unfretted headword"
}


% David Séverin
% Les cinq pieds (2007)
% for violon solo
% (this extract is in the public domain)


% Abbreviations
db = \markup { \musicglyph "scripts.udownbow" }
dub = \markup { \musicglyph "scripts.udownbow" " "
                \musicglyph "scripts.uupbow" }
dubetc = \markup { \musicglyph "scripts.udownbow" " "
                   \musicglyph "scripts.uupbow" "..." }

ub = \markup { \musicglyph "scripts.uupbow" }
udb = \markup { \musicglyph "scripts.uupbow" " "
                \musicglyph "scripts.udownbow" }
udbetc = \markup { \musicglyph "scripts.uupbow" " "
                   \musicglyph "scripts.udownbow" "..." }

accel = \markup \tiny \italic \bold "accel..."
ritar = \markup \tiny \italic \bold "ritar..."

% Strings
svib = \markup \small "s. vib."
pvib = \markup \small "p. vib."
mvib = \markup \small "m. vib."
sulp = \markup \small "s.p."
norm = \markup \small "n."

quatre = \markup \teeny "IV"

% Shifting Notes
shift = \once \override NoteColumn.force-hshift = 0.9
shifta = \once \override NoteColumn.force-hshift = 1.2

% Hairpin
aniente = \once \override Hairpin.circled-tip = ##t

% Tuplets
tupletbp = \once \override Staff.TupletBracket.padding = 2.25

% Functions
#(define-markup-command (colmark layout props args) (markup-list?)
   (let ((entries (cons (list '(baseline-skip . 2.3)) props)))
     (interpret-markup layout entries
                       (make-column-markup (map (lambda (arg)
                                                  (markup arg))
                                                (reverse args))))))
% Instruments
ViolinSolo = \relative c' {
  \set Score.rehearsalMarkFormatter = #format-mark-box-numbers
  \override Score.VoltaBracket.font-family = #'sans
  \override Score.VoltaBracket.extra-offset = #'(0 . 1)
  \override SpacingSpanner.uniform-stretching = ##t

  \voiceOne

  % Measure 1
  \time 25/8 \mark \default
  r2^\markup \colmark { \italic "fatigué" " " \bold "lentement"} r4 r r8
  <<
    { \shift d2\glissando^\markup \colmark { \quatre \dubetc \svib }
      \shifta e1 } \\
    { d2\open\mf\< ~ \aniente d1\!\>
      r4 r\!^\markup \colmark { " " \fermata } }
  >>

  % Measure 2
  \time 7/4
  \set Score.repeatCommands = #'((volta "1) n.      2) s.p."))
  <<
    { \shift d2\glissando^\markup \colmark { \quatre \udbetc }
      \shifta e1 } \\
    { d2\open\mf\< ~ d1\!\> ~ d4\!^\markup \colmark { " " \fermata } }
  >>
  \set Score.repeatCommands = #'((volta #f))

  % Measure 3
  \time 15/4
  <<
    { \shift d2\glissando^\markup \colmark { \quatre \dubetc \pvib \norm }
      \shifta e1\glissando d2 } \\
    { d2\open\mf\< ~ d1 ~ d2\ff ~ d1\> ~
      d2^\markup \colmark { " " " " \svib } ~ d4\pp }
  >>
  \break

  % Measure 4
  \time 4/4 \stemUp \tupletDown
  \tuplet 3/2 { d4 ^\markup \colmark { \quatre \db \accel } d d }
  \tuplet 3/2 { d4 ^\markup \colmark { " " \db \sulp } d d }

  % Measure 5
  \time 5/4
  \tupletbp \tuplet 3/2 {
    d8\mf\<^\markup \colmark { \quatre \db \norm } d_\open d }
  \tupletbp \tuplet 3/2 {
    d8^\markup \colmark { " " \db \sulp } d_\open d }
  \tupletbp \tuplet 3/2 {
    d16^\markup \colmark { " " \db \norm } d_\open d d d_\open d }
  d2\ff\>^\markup \colmark { " " \pvib }

  % Measure 6
  \time 5/8
  \once \override Beam.grow-direction = #RIGHT % \featherDurations 2/3
  d16-.[ d-. d-. d-. d-. d-. d-. d-. d-. d-.]
  \break

  % Measure 7
  \time 7/4
  \tupletbp \tuplet 3/2 {
    d16^\markup \colmark { \quatre } d_\open d d d_\open d }
  \tupletbp \tuplet 3/2 {
    d8^\markup \colmark { " " \db } d_\open d }
  \tupletbp \tuplet 3/2 {
    d8^\markup \colmark { " " \db " " \sulp } d_\open d }
  \tuplet 3/2 { d4^\markup \colmark { \quatre \db \ritar \norm } d d }
  \tuplet 3/2 { d4^\markup \colmark { " " \db " " \sulp } d d\ppp ~ }

  % Measure 8
  d4^\markup \colmark { " " " " \pvib \norm } deh2 d dih \<

  % Measure 9
  <<
    { \shift d2\glissando^\markup \colmark { \quatre } \shifta e1 } \\
    { d2\open ~ d1^\markup \colmark { " " " " \mvib } }
  >>
  \breathe r4\!
}

\score {
  <<
    \new Staff \relative c' \ViolinSolo
    \hide Score.Rest
    \set Score.measureBarType = ""
  >>

  \layout {
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

\paper {
  system-system-spacing.padding = 5
}
