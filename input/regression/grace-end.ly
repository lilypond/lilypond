#(ly:set-option 'old-relative)
\version "1.9.1"
\header {

    texidoc="@cindex Grace End
 Grace notes after the last note do not confuse the timing code."


}

\score { 
  \context Voice \notes\relative c' {
    
	c4 \grace {  d16[ d16] }
	
  }
	\paper {
		raggedright = ##t
	}  
  \midi { }
}

