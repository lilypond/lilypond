
\version "2.1.22"
\header{
texidoc="
When tying chords, the outer slurs point outwards, the inner slurs
point away from the center of the staff.  Override with
@code{tieVerticalDirection}.
"
}


t =  \notes \relative c' {   <c e g> ~ <c e g> }

	\score { 
\notes \context Voice {
   \t
   \transpose c g \t

   \override TieColumn  #'direction = #-1
   \t

  }
}

