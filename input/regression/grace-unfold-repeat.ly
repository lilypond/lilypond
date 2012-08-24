\version "2.16.0"
\header {
  texidoc = "When grace notes are entered with unfolded repeats,
line breaks take place before  grace  notes.
"
}



\context Voice \relative c'{
  \repeat unfold  10 {\grace d8 c4 d e f}

}


