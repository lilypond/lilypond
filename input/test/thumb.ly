\version "1.7.6"
\header{
	title="CELLO-THUMBS"
	enteredby="Maarten Storm"
}


% the thumb-script is used in cello music to indicate a note that should
% be played with your thumb. 



\score { \notes \relative c'' {
		[<<a a'-3(>>8_\thumb <<b b'-3>>-)_\thumb
		<<c c'-3(>>_\thumb <<d d'-3>>-)_\thumb]
	}
	\paper{ 
		linewidth = 80.\mm 

	}
}

	
%% new-chords-done %%
