\version "2.4.0"

one = \relative {
	c' d e f
}

two = \relative {
	\clef "bass"
	c2 g2
}

<<
  \new Staff \one
  \new Staff \two
>>


% A full example with two staves
%
% Type:
%
%     lilypond example-3
%     xpdf example-3     # or your PDF viewer here

%%
%% For learning LilyPond, please read the tutorial, included in the
%% user-manual.
%% 
