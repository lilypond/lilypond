\version "1.0.7";

one = \notes\relative c{
	c'' d e f
}

two = \notes\relative c{
	\clef "bass";
	c'2 g2
}

\score{
	<
		\one
		\two
	>
	\paper{}
	\midi{}
}

% A full-mudela example with two staffs
%
% Type:
%
%     ly2dvi example-3
%     xdvi example-3     # or your dvi viewer here
%
% For more elaborate examples see twinkle.ly, kortjakje.ly and mutopia/*.
%
% A docmument on Mudela is under construction: Documentation/tex/mudela.doc
% (available as .ps from the website too).
