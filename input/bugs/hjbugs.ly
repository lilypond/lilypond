%{

2) Grace-output broken: \grace a8 ~ a2 \grace a8 ~ a2

Expected : a8( )a2 a8( )a2
BUG-here : a8( )a2( a8 )a2

3) Vertical aligns of lines, i.e., line height incorrect:

c^#'(lines (finger "1" ""))  % upper 1,       lower 1
c^#'(lines (finger "1" "1")) % upper 1,       lower 1
\stemDown
c_#'(lines (finger "" "1"))  % upper 1/2 ?!?, lower 1 <- BUG IN UPPER LINE
c_#'(lines (finger "1" "1")) % upper 1,       lower 1

%}


\score { 
     \notes \transpose c''
     \context Voice {
 	<e>~<e>~<c e fis>~<b e f>   % paper & midi broken
 	<e>~<e>~<c e fis>~<b e f>   % here both are ok.
 	c^#'(lines (finger "1" ""))
 	c^#'(lines (finger "1" "1"))
 	\stemDown
 	c_#'(lines (finger "" "1"))  % wrong line height of an empty line
 	c_#'(lines (finger "1" "1")) 
 
 	\grace a8 ~ a2 \grace a8 ~ a2 a8
 	% produces: a8( )a2( a8 )a2   and midi: a1 (a2 \grace a a -> a2)
 	% should be: a8( )a2 a8( )a2  should be: a2 a2 
  }
     \midi{}
     \paper{}
 }
