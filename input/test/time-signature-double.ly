
\version "2.1.26"

\header { texidoc= "

@cindex Double time signature
@cindex markup
@cindex Time signature, double.


Double time
signatures are not supported explicitly, but they can be faked with 
markups and overriding formatting routines. " }

tsMarkup =
\markup  {
    \number { 
    \column < "6" "4" >
    \musicglyph #"scripts-stopped" 
    \bracket \column < "3" "2" >
    }}
	

\score  { \notes \relative c'
	  {
	   \override Staff.TimeSignature  #'print-function = #Text_item::print
	   \override Staff.TimeSignature  #'text = #tsMarkup
	   
	   
	   \time 3/2
	   c2 c c 
	   
   }
	\paper{ raggedright = ##t}
}	  
