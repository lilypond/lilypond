
\version "2.3.17"
\header{
texidoc="
When tying chords, the outer slurs point outwards, the inner slurs
point away from the center of the staff. The behavior can be overridden 
by setting explicitly the @code{direction} of a @code{TieColumn}.
"
}


t =  \relative c' {   <c e g> ~ <c e g> }

	\score { 
 \context Voice {
   \t
   \transpose c g \t

   \override TieColumn  #'direction = #-1
   \t

  }
}

