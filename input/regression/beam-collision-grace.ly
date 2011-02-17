

\version "2.13.49"

\header { texidoc = "
The beaming algorithm handles collisions between beams and
grace notes too.
" }

\relative c' {
 e'8[ f  e \grace { f,16[ a] } e'8]
}

