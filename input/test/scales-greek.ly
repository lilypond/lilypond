\version "2.1.7"
\header {
  texidoc = "

Show different greek scales. All these scales are in the key of C
(major, ionian, phrygian, etc.)

"
}

\score  { \notes {

\key c \major c'8 -"major" d'8 e'8 f'8 g'8 a'8 b'8 c''8

\key c \ionian c'8 -"ionian" d'8 e'8 f'8 g'8 a'8 b'8 c''8

\transpose d c { \key d \dorian c'8 -"dorian" d'8 e'8 f'8 g'8 a'8 b'8 c''8 }

\transpose e c { \key e \phrygian c'8 -"phrygian" d'8 e'8 f'8 g'8 a'8 b'8 c''8 }

\transpose f c { \key f \lydian c'8 -"lydian" d'8 e'8 f'8 g'8 a'8 b'8 c''8 }

\transpose g c  { \key g \mixolydian c'8 -"mixolydian" d'8 e'8 f'8 g'8 a'8 b'8 c''8 }

\transpose a c' { \key a \minor c'8 -"minor" d'8 e'8 f'8 g'8 a'8 b'8 c''8 }

\transpose a c' { \key a \aeolian c'8 -"aeolian" d'8 e'8 f'8 g'8 a'8 b'8 c''8 }

\transpose b c' { \key b \locrian c'8 -"locrian" d'8 e'8 f'8 g'8 a'8 b'8 c''8 }

}
	  
	\paper { }
	\midi {}
}

