t = \notes \relative c' {   <c e g> ~ <c e g> }

	\score { 
\notes \context Voice {
   \t
   \transpose g' \t

   \property Voice.TieColumn \push #'direction = #-1
   \t

  }
}
