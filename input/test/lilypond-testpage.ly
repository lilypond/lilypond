
\version "2.3.4"
% MERGE with title.ly -gp

%{
  Test lilypond features.
%}

\header {
copyright = "copyright"
title = "title"
subtitle = "subtitle"
composer = "composer"
arranger = "arranger"
instrument = "instrument"
metre = "metre"
opus = "opus"
piece = "piece"
poet = "poet"
texidoc = "In the generated output for printing, there are several titles which do not appear in the web pages. "

%
% todo: check whether title.ly does the same. --hwn
%
}


\score {
  \relative c'' { c1 c1 c1 c1 }
}

\score {
	 \relative c'' { c1 c1 c1 c1 }
	
	\header {

	title = "localtitle"
	subtitle = "localsubtitle"
	composer = "localcomposer"
	arranger = "localarranger"
	instrument = "localinstrument"
	metre = "localmetre"
	opus = "localopus"
	piece = "localpiece"
	poet = "localpoet"
	copyright = "localcopyright"
	}
}

