\version "1.3.4";

global = \notes {
  s1 | \mark "A";
  s1*2 | \mark "'12";
}

one = \notes\relative c{
  c'' c c c
  c c c c
  c c c c
}

two = \notes\relative c{
  b' b b b
  b b b b
  b b b b
}

\score{
	< \global \one \two >
	\paper {
		\translator { \OrchestralPartStaffContext 
		        markScriptPadding = "4.0";
			markHangOn  = "Bar";
			markHangDepth = "1";
%			markDirection = \down;
		}
	}
}
