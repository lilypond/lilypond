\header{
filename =	 "flauti.ly";
% %title =	 "Ouvert\\"ure zu Collins Trauerspiel \\"Coriolan\\" Opus 62";
description =	 "";
composer =	 "Ludwig van Beethoven (1770-1827)";
enteredby =	 "JCN";
copyright =	 "public domain";


}

\version "1.3.4";

\include "flauto-1.ly"
\include "flauto-2.ly"

$flauti_staff = \context Staff = flauti <
	\property Staff.midiInstrument = #"flute"
	\property Staff.instrument = #"2 Flauti"
	\property Staff.instr = #"Fl."
	%\notes \context Voice=flauti < 
	\notes \context Staff=flauti < 
		\global
		\context VoiceOne=flautoi 
			\$flauto1
		\context VoiceTwo=flautoii 
			\$flauto2
	>
>

