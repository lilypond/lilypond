
\version "1.9.8"

\header { texidoc= "

@cindex Double time signature
@cindex markup
@cindex Time signature, double.


Double time
signatures are not supported explicitly, but can be faked with markups
and overriding formatting routines. " }

tsMarkup =
\markup  {
    \number { 
    \column < "6" "4" >
    \musicglyph #"scripts-stopped" 
    \bracket \column < "3" "2" >
    }}
	

\score  { \notes \relative c'
	  {
	   \property Staff.TimeSignature \override #'molecule-callback = #Text_item::brew_molecule
	   \property Staff.TimeSignature \override #'text = #tsMarkup
	   
	   
	   \time 3/2
	   c2 c c 
	   
   }
	\paper{ raggedright = ##t}
}	  
