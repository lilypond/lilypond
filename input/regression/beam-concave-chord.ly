\version "2.19.21"
\header {
texidoc = "Concave beaming works for chords as well as monophonic
music.
"
}

morgenliedBeam = \relative \new Voice {
  \time 3/8
  % morgenlied
  c''16 b c e g <e b'> |
  \stemUp
  c16 b c e g <e b'> |
}

rachmaninovBeams = \relative \new Voice {
  \voiceOne
  \time 4/4
  \key c \minor
  <c'' e c'>8[ <g c g'> <c f> <ees, ees'>] ~
  <ees ees'>8[ <c c'> <des des'> <d d'>]
}

horizontalBeams = {
  % todo.
}

nonHorizontalBeams = {
  \morgenliedBeam
  \rachmaninovBeams
}
\layout {
  ragged-right = ##t
}

% cut & paste from beam-concave.ly

#(define (<> x y) (not (= x y)))
mustBeHorizontal = {
  \override Staff.Beam.positions = #(check-slope-callbacks =)
}
mustNotBeHorizontal = {
  \override Staff.Beam.positions = #(check-slope-callbacks <>)
}

\new Voice {
  \mustBeHorizontal
  R1
  \horizontalBeams
  \mustNotBeHorizontal
  \nonHorizontalBeams
}
