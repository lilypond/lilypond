
\version "2.7.39"
\header {

  texidoc="@cindex Grace End
 Grace notes after the last note do not confuse the timing code."


}

\layout {
  ragged-right = ##t
}



\context Voice \relative c' {
  
  c4 \grace {  d16[ d16] }
  
}
  


