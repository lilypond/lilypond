\version "2.3.4"
\header {
  texidoc = "

In addition to major and minor keys, the key can be given also in terms 
of greek, modal scales: ionian (= major), dorian, phrygian, lydian, mixolydian,
aeolian (= minor), and locrian. All these scales are in the key of C.

"
}

\score  {  {

\key c \ionian c'8 -"C ionian, major" d'8 e'8 f'8 g'8 a'8 b'8 c''8

\transpose d c { \key d \dorian d'8 -"C dorian" e'8 f'8 g'8 a'8 b'8 c''8 d''8}

\transpose e c { \key e \phrygian e'8 -"C phrygian" f'8 g'8 a'8 b'8 c''8 d''8 e''8}

\transpose f c { \key f \lydian f'8 -"C lydian" g'8 a'8 b'8 c''8 d''8 e''8 f''8}

\transpose g c  { \key g \mixolydian g'8 -"C mixolydian" a'8 b'8 c''8 d''8 e''8 f''8 g''8}

\transpose a c' { \key a \aeolian a8 -"C aeolian, minor" b8 c'8 d'8 e'8 f'8 g'8 a'8 }

\transpose b c' { \key b \locrian b8 -"C locrian" c'8 d'8 e'8 f'8 g'8 a'8 b'8 }

}
	  
	\paper { }
	\midi {}
}

