\version "1.3.117";

global =  \notes {
  s1 | \mark "A";
  s1 | \mark ; 
  s1 | \mark "12";
  s1 | \mark "A2";
  s1
}

one =  \notes \relative c {
  c''1 c c c c 
}


\score{
\context Staff	< \global \one >
	\paper {
		\translator { \OrchestralPartStaffContext 
		}
	}
}
