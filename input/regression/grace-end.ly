
\version "2.3.22"
\header {

    texidoc="@cindex Grace End
 Grace notes after the last note do not confuse the timing code."


}

\score { 
  \context Voice \relative c' {
    
	c4 \grace {  d16[ d16] }
	
  }
	\layout {
		raggedright = ##t
	}  
  \midi { }
}

