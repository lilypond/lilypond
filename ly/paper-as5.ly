% paper-as5.ly

\version "1.3.120";

paperAsFive = \paper {
	staffheight = 5.\char;
	%% aiai only have these:
%{	
	mf/as5.af
	mf/as9.af
	mf/as-braces5.af
	mf/as-braces9.af
	mf/as-dummy1.af
	mf/as-dummy.af
	mf/as-number1.af
	mf/as-number4.af
%}
	
	\stylesheet #(make-style-sheet 'paper16)

	\include "params-as.ly";
}

\paper { \paperAsFive }
