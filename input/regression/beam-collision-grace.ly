

\version "2.16.0"

\header { texidoc = "
The beaming algorithm handles collisions between beams and
grace notes too.
" }

\relative c' {
 e'8[ f  e \grace { f,16[ a] } e'8]
}

\relative c'{
 d16 e f \grace d'8 g,16
}
