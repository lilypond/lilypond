\header {
    texidoc = "Chord name scheme test -- double-plus-new-chord-name jazz"
}

\version "1.7.11"



%% This should only be necessary if your kpathsea setup is broken
%
% Make sure the correct msamxx.tfm is where lily can find it
% (ie cwd or lily's tfm dir).
%
% For normal (20pt) paper, do
%
%   cp $(locate msam9.tfm) $LILYPONDPREFIX/fonts/tfm
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

efull = \chordnames {

    %% ? what 'bout maj7?
    %% c:7 = \markup { \normal-size-super "maj7" }

    %% Choose your symbol for the fully diminished chord
    %% American:
    %% c:3-.5-.7- = \markup { "dim" }
    %% Jazz:
    c:3-.5-.7- = \markup { \super " o" }

    %% Hmm
    %%    	   ;;Pick your favorite maj7
    %%	   ((0) mathm-markup-object)  ;;a white triangle
    %%	   ;;((0) mathn-markup-object) ;;a black triangle
    %% ;;((0) (make-simple-markup "maj7")) ;;good old maj7

    %% This ok?
    c:7+ = \markup { \normal-size-super \override #'(font-family . math) "N" }
    %%c:3.5.7 = \markup { \override #'(font-family . math) "M" }
    %%c:3.5.7 = \markup { \normal-size-super "maj7" }
}

epartial = \chordnames {
    c:2^3 = \markup { \normal-size-super "2" }
    c:3-  = \markup { "m" }
    c:4   = \markup { \normal-size-super "sus4" }
    c:5^3 = \markup { \normal-size-super "5" }
}

\score {
  \notes <
    \context ChordNames {
	
	%#(set-double-plus-new-chord-name-style 'banter
	%   `((separator . ,(make-simple-markup ":"))
	%     (full-exceptions . ,efull)
	%     (partial-exceptions . ,epartial)))
	
	#(set-double-plus-new-chord-name-style 'jazz
	   `((separator . ,(make-simple-markup ":"))
	     (full-exceptions . ,efull)
	     (partial-exceptions . ,epartial)))
	\scheme }
    \context Staff \transpose c c' \scheme
  >
}
%% new-chords-done %%
