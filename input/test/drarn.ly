
\version "2.3.8"
\header{texidoc="@cindex Drarn
You can attach slurs and ties to noteheads.
" }

\score {
  \context Staff  <<
    
     \relative c'' {
\time 3/8	 
      \override Stem  #'direction = #1
      \override Tie  #'direction = #1
      \override Slur  #'direction = #1
      \override Slur  #'attachment = #'(head . head)
      c8~c(c)  
    }\\
     \relative c'' {
      \override Stem  #'direction = #-1
      \override Tie  #'direction = #-1
      \override Slur  #'direction = #-1
      \override Slur  #'attachment = #'(head . head)
      a8(a)~a  
    }
  >>
  \paper { linewidth = 40*\staffspace
		raggedright = ##t } 
}

