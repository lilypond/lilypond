#(ly:set-option 'old-relative)
\version "1.9.0"
\header{texidoc="@cindex Drarn
You can attach slurs and ties to noteheads.
" }

\score {
  \context Staff \notes <
    \time 3/8
    \context Voice=one \relative c'' {
      \property Voice.Stem \set #'direction = #1
      \property Voice.Tie \set #'direction = #1
      \property Voice.Slur \set #'direction = #1
      \property Voice.Slur \set #'attachment = #'(head . head)
      c8~c(c-)  
    }
    \context Voice=two \relative c'' {
      \property Voice.Stem \set #'direction = #-1
      \property Voice.Tie \set #'direction = #-1
      \property Voice.Slur \set #'direction = #-1
      \property Voice.Slur \set #'attachment = #'(head . head)
      a8(a-)-~a  
    }
  >
  \paper { linewidth = 40*\staffspace
		raggedright = ##t } 
}

