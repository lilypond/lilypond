\version "1.7.18"
% MERGE with title.ly -gp

%{
  Test ly2dvi features.
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
texidoc = "This file tests ly2dvi titling. It should be processed with ly2dvi. "

%
% todo: check whether title.ly does the same. --hwn
%
}


\score {
 \notes \relative c'' { c1 c1 c1 c1 }
}

\score {
	\notes \relative c'' { c1 c1 c1 c1 }
	
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

