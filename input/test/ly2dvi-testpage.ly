

%{
  Test ly2dvi features.
%}

\header {
copyright = "copyright";
title = "title";
subtitle = "subtitle";
composer = "composer";
arranger = "arranger";
instrument = "instrument";
metre = "metre";
opus = "opus";
piece = "piece";
poet = "poet";
}


\score {
	\notes { c1 c1 c1 c1 }
}

\score {
	\notes { c1 c1 c1 c1 }
	
	\header {

	title = "localtitle";
	subtitle = "localsubtitle";
	composer = "localcomposer";
	arranger = "localarranger";
	instrument = "localinstrument";
	metre = "localmetre";
	opus = "localopus";
	piece = "localpiece";
	poet = "localpoet";
	copyright = "localcopyright";
	}
}
