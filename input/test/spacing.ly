\header{
title= "Spacing";
subtitle = "proofsheet"; 
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures =	 "This file tests various spacings";
}

\version "1.0.14";

multipart = \notes{ 
    \type StaffGroup < 
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
                   \times 2/3 { [ c8 c c] } 
                   \times 2/3 { [ c c c] } 
                   \times 2/3 { [c c c] } \times 2/3 { [c c c] } 
                }
                { 
                   \times 2/4 { [ c8 c c c] }
                   \times 2/4 { [ c c c c] } 
                   \times 2/4 { [c c c c] } \times 2/4 { [c c c c] } 
                }
                { 
                   \times 2/5 { [ c8 c c c c] } 
                   \times 2/5 { [ c c c c c] } 
                   \times 2/5 { [c c c c c] } \times 2/5 { [c c c c c] } 
                }
                { 
                   \times 2/6 { [c8 c c c c c] } 
                   \times 2/6 { [c  c c c c c] } 
                   \times 2/6 { [c c c c c c] } \times 2/6 { [c c c c c c] } 
                }
                { 
                   \times 2/7 { [c8 c c c c c c] } 
                   \times 2/7 { [c c  c c c c c] } 
                   \times 2/7 { [c c c c c c c] } \times 2/7 { [c c c c c c c] } 
                }
            >
	}

singlepart = \notes{ 
	c4 d e f
	c4 d e [f8 f]
	c4 d e f8 [f16 f]
	c4 d e f8 f16 [f32 f]
	c4 d e f8 f16 f32 [f64 f]
	c4 c4 c4 c4
	
    }
    
    \score{
    \notes { 
%    	\singlepart 
	\multipart 
    }
    \paper {
%    linewidth= 18.\cm;
%	gourlay_maxmeasures = 4.0
    }
}
