
\header {

texidoc="Beams are place automatically; the last measure should have a single 
beam."

}

\version "2.3.2"



\paper  { raggedright = ##t } 

\score {
  \notes \relative c'' {
    a\longa a\breve  
    a1 a2 a4 a8 a16 a32 a64 a64 
  }
  \paper {
    \context {
      \Staff
	\remove "Clef_engraver"
%%	\remove "Staff_symbol_engraver"
    }
  }
}

