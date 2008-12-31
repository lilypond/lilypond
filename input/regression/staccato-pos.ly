
\version "2.12.0"

\header{

  texidoc=" Some scripts must have quantized postions. VErtical
 position descend monotonously for a descending scale.  The staccato
 dot is close to the notehead. If the head is in a space, then the dot
 is in the space next to it.  "

}

\layout {
  ragged-right = ##t
}

{
  \new Voice \relative c'' { 
    \voiceOne
    g8-. a-. b-. c-. 
    a-. b-. c-. d-. 
    b8[-. a-. g b] 
    e,-. f-. g-. a-. 
    e-. g-. b-. d-.
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
}
