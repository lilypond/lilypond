
\version "1.3.110";

t = \notes \relative c' {   <c e g> ~ <c e g> }

	\score { 
\notes \context Voice {
   \t
   \transpose g' \t

   \property Voice.TieColumn \override #'direction = #-1
   \t

  }
}
