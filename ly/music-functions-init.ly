applymusic = #(ly:make-music-function
   (list procedure? ly:music?) ; signature
   (lambda (where func music)  ; the function
     (func music)))

\version "2.3.1"



%{

TODO:

remove these from the parser, and softcode here:

 * \addlyrics
 * \tag
 * \appoggiatura, \acciaccatura, \grace
 * \partcombine
 * \autochange
 * \applycontext
 * \applyoutput
 * \breathe


with small syntax changes, we could also do

 * \bar
 *  ?

%}
