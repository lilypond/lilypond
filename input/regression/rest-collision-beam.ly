\header {
  texidoc = "Rests under beams are only moved if necessary."
}

\layout {
  ragged-right = ##t
}

\version "2.7.39"

fig =   \relative c' {
  <a c e>8[ r <c e a> r <e a c> r <a c e>]
	    r |
}

  

  \relative c' \new Staff {
    \fig 
    \transpose c c,  \fig
    \new Voice { \stemUp \transpose c f  \fig }
    <<
      \\
      \transpose f c \fig
    >>
    
    <<
      { \transpose c c' \fig }
      \\
      {} 
    >>
    
    << \transpose c c' \fig \\
       \transpose f c \fig
     >>
  }




