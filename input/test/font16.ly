\header{
title= "The Feta Font";
subtitle = "proofsheet"; 
enteredby =	 "jcn & hwn";
copyright =	 "public domain";
description = "This file tests the Feta music font";
% "(Feta definitively is not an abbreviation of Font-En-TjA)";
}

\version "1.0.7";
\include "font-body.ly"

\score{
	\FontBody
	\paper{ 
	    % don't change this.
	    % otherwise 16pt and 20pt layouts differ.
	    linewidth = 12.5 \cm;
	    gourlay_maxmeasures =5.;
	}
}

