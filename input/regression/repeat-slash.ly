#(ly:set-option 'old-relative)
\version "1.9.8"
\header {
texidoc = "Beat repeats are supported."
}

\score { \notes \relative c'
	 \context Voice { \time 4/4
   \repeat "percent" 2 { c2 }

   % the chairman dances
   \repeat "percent" 2 { g'8 g c, c  }   
   \repeat "percent" 4 { b8 b8  }
  }
	 \paper { raggedright = ##t }
}
