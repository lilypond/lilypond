\header{
title= "The Feta font";
subtitle = "proofsheet"; 
enteredby =	 "jcn";
copyright =	 "public domain";
description = "This file tests the Feta music-font in the 20pt version";
TestedFeatures =	 
	 "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\include "font-body.ly"

\score{
	\context Staff { \FontBody}
	\paper{
	    linewidth = 17.5 \cm;
	    gourlay_maxmeasures =5.;
	}
}
\version "1.3.42"; 

