\header {
 texidoc = " show different naming conventions"
}

\version "1.7.10"

scheme = \chords {
  % major chords
  c
  c:6		% 6 = major triad with added sixth
  c:maj		% triangle = maj
  c:6.9^7	% 6/9 
  c:9^7		% add9

  % minor chords
  c:m		% m = minor triad
  c:m.6		% m6 = minor triad with added sixth
  c:m.7+	% m triangle = minor major seventh chord
  c:3-.6.9^7	% m6/9 
  c:m.7		% m7
  c:3-.9	% m9
  c:3-.9^7	% madd9

  % dominant chords
  c:7		% 7 = dominant
  c:7.5+	% +7 = augmented dominant
  c:7.5-	% 7b5 = hard diminished dominant
  c:9		% 7(9)
  c:9-		% 7(b9)
  c:9+		% 7(#9)
  c:13^9.11 	% 7(13)
  c:13-^9.11 	% 7(b13)
  c:13^11	% 7(9,13)
  c:13.9-^11	% 7(b9,13)
  c:13.9+^11	% 7(#9,13)
  c:13-^11	% 7(9,b13)
  c:13-.9-^11	% 7(b9,b13)
  c:13-.9+^11	% 7(#9,b13)

  % half diminished chords
  c:m5-.7		% slashed o = m7b5
  c:9.3-.5-	% o/7(pure 9)

  % diminished chords
  c:m5-.7-	% o = diminished seventh chord
}

\score {
  \notes <
    \context ChordNames {
	#(set-chord-name-style 'jazz)
	\property ChordNames.instrument = #"Jazz"
	\property ChordNames.instr = #"Jz"
	\scheme }
    \context ChordNames = bn {
	#(set-chord-name-style 'banter)
	\property ChordNames.instrument = # "Banter"
	\property ChordNames.instr = #"Bn"
	\scheme }
    \context ChordNames = am {
	#(set-chord-name-style 'american)
	\property ChordNames.instr = #"Am"
	\property ChordNames.instrument = #"American"
	\scheme }
    \context Staff \transpose c c' \scheme
  >
\paper {
linewidth = 16.0\cm

\translator {
  \ChordNamesContext \consists Instrument_name_engraver }
}
}
%% new-chords-done %%
