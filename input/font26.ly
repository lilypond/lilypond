\header{
title= "The Feta Font";
subtitle = "proofsheet"; 
enteredby =	 "jcn & hwn";
copyright =	 "public domain";
description = "This file tests the Feta music font";
% "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\include "paper26.ly";
\include "font-body.ly"
\score{
	\FontBody
	\paper{
	    \paper_twentysix
	    linewidth = 17.5 \cm;
	    gourlay_maxmeasures =5.;
	}
}

