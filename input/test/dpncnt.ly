\header {
1    texidoc = "test file for new-new-chord names, ie, double-plus-new-chord-name"
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


xch = \chords { c:7+.9-^3.5 c:dim }

xch = \chords { c:13-.9+^11 }
ch = \chords { c:7.9- }
ch = \chords { c:7.9+.11+ }
ch = \chords { c:7.9+ }
ch = \chords {  c:3-.9^7 }	% madd9

ch = \chords {  c:3-.6.9^7 }	% m6/9 

ch = \chords { c:dim9 }

ch = \chords { c:1^5 }

ch = \chords { c:m5-.7-	} % o = diminished seventh chord

ch = \chords { c:7-	} 
%ch = \chords { c:3.11-	}

%ch = \chords { c:7.11.13 }

% ch = \chords { c:7.11.15.17.19.21 }
ch = \chords { c c:m c:7 c:7.9 c:7+.9 c:7.9+ c:9^7 c:3.11^7 }

%ch = \chords { c:9^7 c:5^3}

ch = \chords { c:3- c:3 c:2 c:7+ c:3-.5-.7- c:6.9^7 }

\score{
    <
	\context ChordNames {
	% #(set-chord-name-style 'jazz)
	% #(set-chord-name-style 'double-plus-new-banter)
	% #(set-chord-name-style 'double-plus-new-jazz)
	
	#(set-double-plus-new-chord-name-style 'banter
	   `((separator . ,(make-simple-markup ":"))
	     (full-exceptions . ,efull)
	     (partial-exceptions . ,epartial)))
	
	%#(set-double-plus-new-chord-name-style 'jazz
	%   `((separator . ,(make-simple-markup ":"))
	%     (full-exceptions . ,efull)
	%     (partial-exceptions . ,epartial)))
	
	\ch
    }
	\context Staff \notes \transpose c c' \ch
    >
    \paper{
	\translator { 
	    \ChordNamesContext
	    ChordName \override #'word-space = #1 
	}
    }
}
