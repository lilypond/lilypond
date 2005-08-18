
\version "2.6.0"
\header {

  texidoc="@cindex Grace End
 Grace notes after the last note do not confuse the timing code."


}


\context Voice \relative c' {
  
  c4 \grace {  d16[ d16] }
  
}
\layout {
  raggedright = ##t
}  


