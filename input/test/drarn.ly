
\version "2.1.7"
\header{texidoc="@cindex Drarn
You can attach slurs and ties to noteheads.
" }

\score {
  \context Staff \notes <<
    
     \relative c'' {
\time 3/8	 
      \property Voice.Stem \set #'direction = #1
      \property Voice.Tie \set #'direction = #1
      \property Voice.Slur \set #'direction = #1
      \property Voice.Slur \set #'attachment = #'(head . head)
      c8~c(c)  
    }\\
     \relative c'' {
      \property Voice.Stem \set #'direction = #-1
      \property Voice.Tie \set #'direction = #-1
      \property Voice.Slur \set #'direction = #-1
      \property Voice.Slur \set #'attachment = #'(head . head)
      a8(a)~a  
    }
  >>
  \paper { linewidth = 40*\staffspace
		raggedright = ##t } 
}

