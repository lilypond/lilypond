\header{
	title="CELLO-THUMBS";
	enteredby="Maarten Storm";
}


% the thumb-script is used in cello music to indicate a note that should
% be played with your thumb. 

\version "1.3.59";

\score { \notes \relative c'' {
		[<a8_\thumb a'8-3(> <)b_\thumb b'-3>
		<c_\thumb c'-3(> <)d_\thumb d'-3>]
	}
	\paper{ 
		linewidth = 80.\mm; 
		castingalgorithm = \Wordwrap;
	}
}

	
