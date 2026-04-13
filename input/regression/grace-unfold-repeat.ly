\version "2.25.35"

\header {
  texidoc = "When grace notes are entered with unfolded repeats,
line breaks take place before  grace  notes.
"
}



\context Voice \relative c'{
  \*10 { \grace d8 c4 d e f }
}
