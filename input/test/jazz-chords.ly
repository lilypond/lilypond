


%% This should only be necessary if your kpathsea setup is broken
%
% Make sure the correct msamxx.tfm is where lily can find it
% (ie cwd or lily's tfm dir).
%
% For normal (20pt) paper, do
%
%   cp locate `msam9.tfm` LILYPONDPREFIXxtfm
%

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
    \context ChordNames \scheme
    \context Staff \transpose c'' \scheme
  >
  \paper {
    \translator { 
      \ChordNamesContext
      ChordName \override #'word-space = #1 
      ChordName \override #'style = #'jazz
    }
  }
}
