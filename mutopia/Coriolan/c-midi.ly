\header{
filename = 	 "c-midi.ly";
title = 	 "Ouverture"; 
subtitle =  "Coriolan";
opus =  "Op. 62";
composer = 	 "Ludwig van Beethoven (1770-1827)";
enteredby = 	 "JCN";
copyright = 	 "public domain";
}


%%
%% Hopefully, this can be junked when part-combiner+midi+dynamics work ok.
%%

\version "1.3.117";

\include "global.ly"

\include "flauto-1.ly"
\include "flauto-2.ly"
\include "oboe-1.ly"
\include "oboe-2.ly"
\include "clarinetto-1.ly"
\include "clarinetto-2.ly"
\include "fagotto-1.ly"
\include "fagotto-2.ly"
\include "corno-1.ly"
\include "corno-2.ly"
\include "trombo-1.ly"
\include "trombo-2.ly"
\include "timpani.ly"
\include "violino-1.ly"
\include "violino-2.ly"
\include "viola-1.ly"
\include "viola-2.ly"
\include "violoncello.ly"
\include "contrabasso.ly"



\score{
	<
	\global;
	\context Staff=flauti <
		\property VoiceCombineStaff.midiInstrument = #"flute"
		\flautoI
		\flautoII
	>
	\context Staff=oboi <
		\property VoiceCombineStaff.midiInstrument = #"oboe"
		\oboeI
		\oboeII
	>
	\context Staff=clarinetti <
		\property VoiceCombineStaff.midiInstrument = #"clarinet"
		\property VoiceCombineStaff.transposing = #-2
		\clarinettoI
		\clarinettoII
	>
	\context Staff=fagotti <
		\property VoiceCombineStaff.midiInstrument = #"bassoon"
		\fagottoI
		\fagottoII
	>
	\context Staff=corni <
		\property VoiceCombineStaff.midiInstrument = #"french horn"
		\property VoiceCombineStaff.transposing = #3
		\cornoI
		\cornoII
	>
	\context Staff=trombe <
		\property VoiceCombineStaff.midiInstrument = #"trumpet"
		\tromboI
		\tromboII
	>
	\context Staff=timpani <
		\property Staff.midiInstrument = #"timpani"
		\timpani
	>
	\context Staff=violini <
		\property Staff.midiInstrument = #"violin"
		\violinoI
		\violinoII
	>
	\context Staff=violi <
		\property VoiceCombineStaff.midiInstrument = #"viola"
		\violaI
		\violaII
	>
	\context Staff=violoncello <
		\property StaffCombineStaff.midiInstrument = #"cello"
		\violoncello
	>
	\context Staff=contrabasso <
		\property StaffCombineStaff.midiInstrument = #"contrabass"
        	\property StaffCombineStaff.transposing = #-12
		\contrabasso
	>
	>
	\include "coriolan-midi.ly"
}

