\version "2.4.0"
\header {
    texidoc = "
Figured bass is created by the FiguredBass context which responds to
figured bass requests and rest-requests.  You must enter these using
the special @code{\figuremode @{ @}} mode, which allows you to type
numbers, like @code{<4 6+>}.

You can also type letters by entering quoted strings, which is shown in the
last bass figure.

" }
\layout  { raggedright = ##t }  
<<
    \figures { 
	<3 [5 7]>
	\once \override BassFigure  #'direction = #-1
	<3 [5 7]>
	<3 [5] 7 [9 11]>
	<3+ 5- 7!>
	<3 _! 5 _- 7>
	<3 _ 5 _ 7>
	<"V7" ["bla" 6] 7>
	
    }
    \context Voice {
	\clef bass
	c 4
	c c c c c 
	g8
    }
>>


