
\version "2.1.7"
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
texidoc = "This file tests lilypond titling. It should be processed with lilypond. "

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

