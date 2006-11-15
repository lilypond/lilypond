
\version "2.10.0"

\header{

  texidoc=" Some scripts must have quantized postions. VErtical
 position descend monotonously for a descending scale.  The staccato
 dot is close to the notehead. If the head is in a space, then the dot
 is in the space next to it.  "

}

\layout {
  ragged-right = ##t
}

\context Voice {
  \relative c' {
    e'4-. f-. g-. d-. c-. b-.
    \stemDown
    e,-. d-. c-. b-. a-. g-.    
  }
  \relative c'' {
    \stemUp		 
    d-> c-> b-> a-> g-> f-> e-> d->
    d'
    d-. c-. b-. a-. g-. f-. e-. d-. 
  }  
}
