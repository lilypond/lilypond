\version "1.7.18"
% delete me?
%TODO: what's this for? manual beaming? (the two pairs of 16ths)
%  showing \stemUp?
% all of those things are covered in refman and/or other files
\header{
  texidoc="DELETE ME
" }
\score{
    \notes\relative c''{
	\stemUp
	 b8-[ c]
	 b16-[ c]
	 a'-[ b]
    }
    \paper{
	raggedright = ##t
    }
}%% new-chords-done %%
