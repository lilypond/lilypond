\version "2.23.12"

\header {

texidoc = "Fermatas have an appropriate distance to dots, note
heads and other articulations.

"

}

\relative c''' {
  \tempo 4 = 60
  a4.
  a4.\fermata
  a4.\henzeshortfermata
  a4.\henzelongfermata
  a4.\shortfermata
  a4.\longfermata
  a4.\veryshortfermata
  a4.\verylongfermata
}

\relative c''' {
  \tempo 4 = 60
  a4->
  a4->\fermata
  a4->\henzeshortfermata
  a4->\henzelongfermata
  a4->\shortfermata
  a4->\longfermata
  a4->\veryshortfermata
  a4->\verylongfermata
}


