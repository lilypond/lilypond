\version "1.5.68"
\header {

texidoc = "Tie engraver uses @code{busyGrobs} to keep track of
note heads. Test if this queue works by throwing many  mixed tuplets at it." 

}

\score
{
\notes \context Staff \relative c'' 
 <
  \context Voice { \voiceOne \times 2/3 { c'8~  c8~ c8~ c8~ c8~ c8 } }
  \context Voice= VII { \voiceThree  { b,8 ~ b8 ~ b8 ~  b8 }}
  \context Voice = VIII { \voiceTwo \times 2/5 { a,4 ~a4 ~a4~ a4~ a4 }}
 >
}
