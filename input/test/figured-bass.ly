\header {
texidoc = "Test figured bass.

Figured bass is created by the FiguredBass context which eats
note-requests and rest-requests.  You can enter these either using
standard @code{< >} notation, or using the special @code{\figures @{ @}}
mode, which allows you to type numbers, like @code{<4 6+>}.
 
" }

\score { \notes  <
\context FiguredBass \transpose c'' {
   <e! g >
   <f8 ais >
   \figures {
     r8
     <1 3 5>4 <3- 5+ 6!> <5>
   } 
 }
 \context Voice {
   c 
   g8 g 
   f4
   d
   c
  }
 
>
 }
