\version "2.1.7"

\header {

texidoc = "Test igatzek inversion and bass notes.
Above the staff: computed chord names. Below staff: entered chord name. 
"

}


bladidbla = \chords { 
    f4:maj7/e_":maj7/e" f:maj7/f_":maj7/f" f2:maj7/g_":maj7/g"
    f4:maj7/+e_":maj7/+e" f:maj7/+f_":maj7/+f" f2:maj7/+g_":maj7/+g"
 }

\score {
<< \context ChordNames \bladidbla
  \context Voice \bladidbla >>
}
