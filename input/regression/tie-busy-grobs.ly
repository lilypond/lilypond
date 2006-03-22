\version "2.8.0"
\header {

  texidoc = "Tie engraver uses @code{busyGrobs} to keep track of
note heads. By throwing many  mixed tuplets on the queue,
one may have collisions between ties and beams. 
" 

}

\layout {
  ragged-right = ##t
}


\context Staff \relative c'' 
<<
  {  \times 2/3 { c'8~  c8~ c8~ c8~ c8~ c8 } }
  \\
  { \voiceTwo \times 2/5 { a,4 ~a4 ~a4~ a4~ a4 }}
  \\
  { \voiceThree  { b,8 ~ b8 ~ b8 ~  b8 }}
>>




