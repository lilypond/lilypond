\version "1.3.5";

global = \notes {
  s1 | \mark "A";
  s1 | \mark ; 
  s1 | \mark "12";
  s1
}

one = \notes \relative c {
  c''1 c c c
}


\score{
\context Staff	< \global \one >
	\paper {
		\translator { \OrchestralPartStaffContext 
		}
	}
}
