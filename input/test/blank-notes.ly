\version "1.7.18"
% possible rename to invis-notes.ly
% check if other documents call this "blank notes", though.  I think
% invisible notes sounds better, but whatever it is, it should be
% consistent.
\header {
    texidoc = "@cindex Invisible Notes
@cindex Blank Notes
You can suppress printing of Lilypond output.  This example shows you how to
print invisible (or blank) notes.  This can be very useful when you want to
do wierd tricks with Lilypond (especially with slurs, since you can't attach
a slur to a rest or spacer rest).
" }

blanknotes = { \property Voice.NoteHead
	       \override #'transparent  = ##t
	       \property Voice.Stem
	       \override #'transparent = ##t }
unblanknotes = { \property Voice.NoteHead
		 \revert #'transparent
		 \property Voice.Stem
		 \revert #'transparent }


\score {
    \notes { c4 d4 
    \blanknotes e4 f4   \unblanknotes
	     g4 a 
	     }
\paper{raggedright = ##t}
}

%% new-chords-done %%
