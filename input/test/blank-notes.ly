
\version "2.2.0"
% possible rename to invis-notes.ly
% check if other documents call this "blank notes", though.  I think
% invisible notes sounds better, but whatever it is, it should be
% consistent.
\header {
    texidoc = "@cindex Invisible Notes
@cindex Blank Notes
Invisible (or transparent) notes can be useful, when weird tricks are needed; 
especially, a slur cannot be attach to a rest or spacer rest.
" }

blanknotes = { \override NoteHead  #'transparent = ##t
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


