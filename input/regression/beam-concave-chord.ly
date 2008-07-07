\version "2.11.51"


morgenliedBeam = \relative c'' \new Voice{
  \time 3/8
  % morgenlied
  c16 b c e g <e b'> |
  \stemUp
  c16 b c e g <e b'> |
}

rachmaninovBeams = \relative \new Voice {
  \voiceOne
  \time 4/4
  \key c \minor
  <c' e c'>8 <g c g'> <c f> <ees, ees'>~ <ees ees'> <c c'> <des des'> <d d'>
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

#(define (<> x y) (not (= x  y)))
mustBeHorizontal = 
  \override Staff.Beam #'positions = #(ly:make-simple-closure
				 (ly:make-simple-closure
				  (append
				   (list chain-grob-member-functions `(,cons 0 0))
				   (check-slope-callbacks =))))
mustNotBeHorizontal = 
  \override Staff.Beam #'positions = #(ly:make-simple-closure
				 (ly:make-simple-closure
				  (append
				   (list chain-grob-member-functions `(,cons 0 0))
				   (check-slope-callbacks <>))))


\new Voice {
  \mustBeHorizontal
  r1
  \horizontalBeams
  \mustNotBeHorizontal
  \nonHorizontalBeams
}

