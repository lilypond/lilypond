\header{
  title =	 "Konzert Nr. 3 Es dur"
  subtitle = "für Horn und Orchester"
  composer =	 "Wolfgang Amadeus Mozart (1756-1791)"
  enteredby =	 "HWN"
  opus = "KV 447"

  copyright = "public domain"
  instrument = "Horn in F"
  editor = "Henri Kling"
  mutopiatitle = "Horn Concerto 3"
  mutopiacomposer = "W.A.Mozart"
  mutopiaopus = "KV447"
  style = "classical"
  maintainer = "hanwen@cs.uu.nl"
  maintainerEmail = "hanwen@cs.uu.nl"
  maintainerWeb = "http://www.cs.uu.nl/~hanwen/"	
  lastupdated = "2002/May/21"
  source = "Edition Breitkopf 2563"
  footer = "Mutopia-2002/05/21-25"

  textagline = "\\parbox{\hsize}{\\thefooter\\quad\\small This music is part of the Mutopia project, \\texttt{http://sca.uwaterloo.ca/Mutopia/}. It has been typeset and placed in the public domain by " + \maintainer + ". Unrestricted modification and redistribution is permitted and encouraged---copy this music and share it!}"
  
  %% TODO: handle \footer, \head[er] properly
  tagline = \markup { \smaller
      \column <
	   \fill-line < \footer "" >
	   \fill-line < { "This music is part of the Mutopia project, "
			  \typewriter { "http://sca.uwaterloo.ca/Mutopia/" }
			  "." } >
	   \fill-line < { "It has been typeset and placed in the public "
			  "domain by "  \maintainer  "." } >
	   \fill-line < { "Unrestricted modification and redistribution "
			  "is permitted and encouraged---copy this music "
			  "and share it!" } >
	   >
       }
}
%{

This is the Mozart 3 for horn.  It's from an Edition Breitkopf EB
2563, edited by Henri Kling. Henri Kling (1842-1918) was a horn
virtuoso that taught in Geneva. 

%}

\version "2.3.4"

\include "mozart-hrn3-defs.ly"
\include "mozart-hrn3-allegro.ly"
\include "mozart-hrn3-romanze.ly"
\include "mozart-hrn3-rondo.ly"




\book {
\score
{
	 { \transpose c' bes \allegro }
	\paper{ }
	\header { piece = "allegro" opus = "" }	
	\midi{ \tempo 4=90 }
}


\score
{
     { \transpose c' bes \romanze }
	\header { piece = "romanze" opus = "" }	
	\midi{ \tempo 4 = 70  }
	\paper{}
}


\score
{
     { \transpose c' bes	\rondo }
	\header { piece = "rondo" opus = "" }
	\midi{ \tempo 4 = 100 }
	\paper { }
}


}
