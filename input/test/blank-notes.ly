
\version "2.1.22"
% possible rename to invis-notes.ly
% check if other documents call this "blank notes", though.  I think
% invisible notes sounds better, but whatever it is, it should be
% consistent.
\header {
    texidoc = "@cindex Invisible Notes
@cindex Blank Notes
You can suppress printing of LilyPond output.  This example shows you how to
print invisible (or blank) notes.  This can be very useful when you want to
do wierd tricks with LilyPond (especially with slurs, since you can't attach
a slur to a rest or spacer rest).
" }

blanknotes = { \override NoteHead  #'transparent  = ##t
	       \override Stem  #'transparent = ##t }
unblanknotes = { \revert NoteHead #'transparent
		 \revert Stem #'transparent }


\score {
    \notes \relative c'' { c4 d4 
    \blanknotes e4 f4   \unblanknotes
	     g4 a 
	     }
\paper{raggedright = ##t}
}


