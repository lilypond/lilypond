\header{
filename =	 "flauti-part.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.3.59";

\include "global.ly"
\include "flauti.ly"

\score{
	\$flauti_staff
	%\include "coriolan-part-paper.ly"

	\paper {
		textheight = 295.0\mm;
		linewidth = 180.0\mm;

		% slurs are never beautiful (no steep slurs)
		slur_beautiful = 0.0;

		\translator {
			\VoiceContext
			\remove Slur_engraver;
		}
		\translator { 
			\ScoreContext skipBars = ##t 
			%% URG: this changes dynamics too
			%%textStyle = #"italic"
			timeSignatureStyle = #"C"
			instrumentScriptPadding = #60  %% urg, this is in pt
			instrScriptPadding = #40 %% urg, this is in pt
			marginScriptHorizontalAlignment = #1
			maximumRestCount = #1
		}
	}
	\include "coriolan-midi.ly"
}

