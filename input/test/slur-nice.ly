
\version "1.3.93";
\score{
	\notes\relative c''{
		\time 3/4;
		\slurUp
		\stemBoth a ( \stemDown a \stemBoth ) a a( c )a a( e' )a, a( g' )a,
		\stemUp a( e' )a,
		\break
		\slurDown
		\stemBoth c ( \stemUp c \stemBoth ) c c ( a ) c c( d, )c' c( f, )c'
		\stemDown c( f, )c'
	}
	\paper{
		linewidth = 120.\mm;
	}
}
