\header {

  texidoc = "Pedals.  Run @code{timidity -idvvv file.midi |grep Midi}
  to see midi events."

}

\version "2.19.21"

\score {
  \relative {
    {
      { c'16 e g c }
      
      { c,16\sustainOn e g c\sustainOff }
      { c,16\unaCorda e g c\treCorde }
      { c,16\sostenutoOn e g c\sostenutoOff }
    }
  }
  \midi {}
  \layout{}
}
