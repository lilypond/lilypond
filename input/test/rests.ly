\version "1.5.68"
\header {
    texidoc ="mensural style rests."
    }

\score { 
  \context Voice \notes\relative c {
    r\longa r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128 
	\property Staff.Rest \override #'style = #'mensural
	\emptyText
	r\longa^"restStyle = \"mensural\"" r\breve r1 r2 r4 r8 r16 r32 r64 r128 r128 
	
	
  }
  \paper { }  
  \midi { }
}
