
%%
%% Hopefully, this can be junked when part-combiner+midi+dynamics work ok.
%%

\version "1.3.117";

\include "header.ly"
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
	\global
	\context Staff=flauti <
		\property Staff.midiInstrument = #"flute"
		\flautoI
		\flautoII
	>
	\context Staff=oboi <
		\property Staff.midiInstrument = #"oboe"
		\oboeI
		\oboeII
	>
	\context Staff=clarinetti <
		\property Staff.midiInstrument = #"clarinet"
		\property Staff.transposing = #-2
		\clarinettoI
		\clarinettoII
	>
	\context Staff=fagotti <
		\property Staff.midiInstrument = #"bassoon"
		\fagottoI
		\fagottoII
	>
	\context Staff=corni <
		\property Staff.midiInstrument = #"french horn"
		\property Staff.transposing = #3
		\cornoI
		\cornoII
	>
	\context Staff=trombe <
		\property Staff.midiInstrument = #"trumpet"
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
		\property Staff.midiInstrument = #"viola"
		\violaI
		\violaII
	>
	\context Staff=violoncello <
		\property Staff.midiInstrument = #"cello"
		\violoncello
	>
	\context Staff=contrabasso <
		\property Staff.midiInstrument = #"contrabass"
        	\property Staff.transposing = #-12
		\contrabasso
	>
	>
	\include "coriolan-midi.ly"
}

