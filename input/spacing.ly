\header{
title= "Spacing";
subtitle = "proofsheet"; 
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures =	 "This file tests various spacings";
}

\version "0.1.9";

multipart = \melodic{ 
    \multi 3 < 
                { 
                   c4
                   c 
                   c c 
                }
                { 
                   [c8 c] 
                   [c c] 
                   [c c] [c c] 
                }
                { 
                   [2/3 c8 c c]1/1 
                   [2/3 c c c]1/1 
                   [2/3c c c]1/1 [2/3c c c]1/1 
                }
                { 
                   [2/4 c8 c c c]1/1
                   [2/4 c c c c]1/1 
                   [2/4c c c c]1/1 [2/4c c c c]1/1 
                }
                { 
                   [2/5 c8 c c c c]1/1 
                   [2/5 c c c c c]1/1 
                   [2/5c c c c c]1/1 [2/5c c c c c]1/1 
                }
                { 
                   [2/6c8 c c c c c]1/1 
                   [2/6c  c c c c c]1/1 
                   [2/6c c c c c c]1/1 [2/6c c c c c c]1/1 
                }
                { 
                   [2/7c8 c c c c c c]1/1 
                   [2/7c c  c c c c c]1/1 
                   [2/7c c c c c c c]1/1 [2/7c c c c c c c]1/1 
                }
            >
	}

singlepart = \melodic{ 
	c4 d e f
	c4 d e [f8 f]
	c4 d e f8 [f16 f]
	c4 d e f8 f16 [f32 f]
	c4 d e f8 f16 f32 [f64 f]
	c4 c4 c4 c4
	
    }
    
    \score{
    \melodic { 
    	\singlepart 
	\multipart 
    }
    \paper {
%    linewidth= 18.\cm;
%	gourlay_maxmeasures = 4.0
    }
}
