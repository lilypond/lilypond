\header{
texidoc="
When tieing chords, the outer slurs point outwards, the inner slurs
point away from the center of the staff.  Override with
@code{tieVerticalDirection}.
";
}
\version "1.3.117";

t =  \notes \relative c' {   <c e g> ~ <c e g> }

	\score { 
\notes \context Voice {
   \t
   \transpose g' \t

   \property Voice.TieColumn \override #'direction = #-1
   \t

  }
}
