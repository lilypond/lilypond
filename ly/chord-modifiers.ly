%{
 chord modifiers
%}

% UGR 
#(eval-string (ly-gulp-file "chord-names.scm"))


% urg!
%
\chordmodifiers #'(
	(m . ( 0 2 -1 ))
	(min . ( 0 2 -1 ))
	(aug . ( 0 4 1 ))
; (dim . ( -100 4 -1 ))	
	(dim . ( -100 2 -1 ))
; urg, not actually a chord-modifier, but it works
; c7 -> <c bes>, c 7+ -> c b
	(maj . ( 0 6 1 ))
; sus4 should delete 2 too...
	(sus . ( 0 3 0 ))
)

