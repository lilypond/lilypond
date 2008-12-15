\version "2.11.65"

\paper {
	ragged-right =  ##t
}

<<
  \chords {a2 a c c c d}
  
  \new Voice =mel {
    \textLengthOn
    % Set global properties of fret diagram
    \override Voice.TextScript #'size = #'1.2
    \override Voice.TextScript #'(fret-diagram-details finger-code) = #'below-string
    \override Voice.TextScript #'(fret-diagram-details dot-color) = #'black
    
    %% A chord for ukelele
    a'2 ^\markup
    \override #'(fret-diagram-details . (
                  (string-count . 4)
		   (dot-color . white)
		   (finger-code . in-dot))){
		   \fret-diagram #"4-2-2;3-1-1;2-o;1-o;"}
   %% A chord for ukelele, with formatting defined in definition string
   %  1.2 * size, 4 strings, 4 frets, fingerings below string
   %  dot radius .35 of fret spacing, dot position 0.55 of fret spacing
    a'2 ^\markup
    \override #'(fret-diagram-details . (
                  (dot-color . white)
		   (open-string . "o"))){
		   \fret-diagram #"s:1.2;w:4;h:3;f:2;d:0.35;p:0.55;4-2-2;3-1-1;2-o;1-o;"}		   
     %% C major for guitar, barred on third fret
     	% verbose style
	% roman fret label, finger labels below string, straight barre
    c' ^\markup 
    \override #'(size . 1.1) { % 110% of default size
       \override #'(fret-diagram-details . (
                     (number-type . roman-lower)
		     (finger-code . below-string)
		     (barre-type . straight))) {
 	    \fret-diagram-verbose #'((mute 6) 
                                     (place-fret 5 3 1) 
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3)) }}
				     
      %% C major for guitar, barred on third fret
     	% verbose style
    c' ^\markup 
    \override #'(size . 1.1) { % 110% of default size
       \override #'(fret-diagram-details . (
                     (number-type . arabic)
		     (dot-label-font-mag . 0.9)
		     (finger-code . in-dot)
		     (fret-label-font-mag . 0.6)
		     (fret-label-vertical-offset . 0)
		     (label-dir . -1)
		     (mute-string . "M")
		     (orientation . landscape)
		     (xo-font-magnification . 0.4)
		     (xo-padding . 0.3))) {
 	    \fret-diagram-verbose #'((mute 6) 
                                     (place-fret 5 3 1) 
                                     (place-fret 4 5 2)
                                     (place-fret 3 5 3)
                                     (place-fret 2 5 4)
                                     (place-fret 1 3 1)
                                     (barre 5 1 3)) }}

      
      %% C major for guitar, with capo on third fret
     	% verbose style
    c' ^\markup 
    \override #'(size . 1.1) { % 110% of default size
       \override #'(fret-diagram-details . (
		     (dot-label-font-mag . 0.9)
		     (finger-code . in-dot)
		     (xo-font-magnification . 0.4)
		     (xo-padding . 0.3))) {
 	    \fret-diagram-verbose #'((mute 6)
                                     (capo 3)
                                     (open 5) 
                                     (place-fret 4 5 1)
                                     (place-fret 3 5 2)
                                     (place-fret 2 5 3)
                                     (open 1)) }}

      %% simple D chord

    d' ^\markup 
        \override #'(fret-diagram-details . (
                     (finger-code . below-string)
                     (dot-radius . 0.35)
		     (dot-position . 0.5)
		     (fret-count . 3)))		     
		     {
	    \fret-diagram-terse #"x;x;o;2-1;3-2;2-3;" }
  }
>>
