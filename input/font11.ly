
\header{
title= "The Feta Font";
subtitle = "proofsheet"; 
enteredby =	 "jcn & hwn";
copyright =	 "public domain";
description = "This file tests the Feta music font, 11pt";
% "(Feta definitively is not an abbreviation of Font-En-TjA)";
}


\include "paper11.ly"
\include "font-body.ly"
\score{
	\FontBody
	\paper{
	    \paper_eleven
	    linewidth = 17.5 \cm;
	    gourlay_maxmeasures =5.;
	}
}

