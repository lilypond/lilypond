
efull = \chordnames {
    c:3-.5-.7- = \markup { \super "didem" }
    c:7+ = \markup { \super "maj7" }
}

epartial = \chordnames {
    c:3- = \markup { "dim" }
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
