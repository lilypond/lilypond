\version "1.5.68"
\header {
texidoc = "Test figured bass.

Figured bass is created by the FiguredBass context which eats
figured bass requests and  rest-requests.  You must enter these using
the special @code{\figures @{ @}} mode, which allows you to type
numbers, like @code{<4 6+>}.
 
" }

\score { \notes  <
 \context FiguredBass {
   \figures {
	<_! 3+ 5- _ 7! 9 >4
	< [4 6] >
   }
 }

 \context Voice {
   c 4
   g8
  }
 
>
 }
