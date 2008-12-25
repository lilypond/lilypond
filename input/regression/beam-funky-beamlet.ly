
\version "2.12.0"
\header {

  texidoc=" Funky kneed beams with beamlets also work. The beamlets
should be pointing to the note head.
"

}

\layout { ragged-right = ##t}

\relative c' {
  \once \override Stem #'direction = #UP
  c16
  \once \override Stem #'direction = #DOWN

  c''8 c,,16
  \once \override Stem #'direction = #UP
  c16
  \once \override Stem #'direction = #DOWN

  c''8 c16
  \once \override Stem #'direction = #DOWN
  
  c16 c,,8
  \once \override Stem #'direction = #UP

  c16          
}
	  
