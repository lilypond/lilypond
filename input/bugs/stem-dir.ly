%
% must push 0 to get 'stemboth', popping alone doesn't work
%
\score{
	\notes\relative c''{
	    c4
	    \property Voice.basicStemProperties \push #'direction = #1
	    c
	    %\property Voice.basicStemProperties \pop #'direction 
	    \property basicStemProperties \pop #'direction 
	    c
	    % burp?
	    \property Voice.basicStemProperties \push #'direction = #0
	    c
	}
}
