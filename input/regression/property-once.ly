\header {
    texidoc = "Once properties take effect during a single time step only."
}

\score {
      \notes \relative c' {
	  c4
	  \once \property Voice.Stem = #'()
	  c4
	  \once \property Voice.Stem \override #'thickness = #5.0
	  c4
	  c4
  }
}
 
