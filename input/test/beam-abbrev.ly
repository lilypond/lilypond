\version "1.7.18"
% TODO: delete me?  I think this is covered in the "tremolo
% subdivision" section in refman.
\header {texidoc="DELETE ME
"}
\score { 
  \context Voice \notes\relative c {
    c'1:16 
	\stemUp
	c4:8 c4:16  c8:16-[ c:]  c,8:16-[ c'':]
	\stemBoth
	 c,,8:16-[ c'':]
	
  }
  \paper { }  
}
%% new-chords-done %%
