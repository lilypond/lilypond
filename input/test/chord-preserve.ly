koorden = \chords{ 
	c1-2/d c-2.3/d 
}


\score{
	<
    	\property Score.chordInversion = "1"
		\type ChordNames {
    			\property Score.chordInversionPreserve = "0"
			\koorden
    			\property Score.chordInversionPreserve = "1"
			\koorden
		}
		\type Staff \notes\transpose c''{
    			\property Score.chordInversionPreserve = "0"
			\koorden
			% preserving doesn't work for staff yet
			% see lily/chord.cc
    			\property Score.chordInversionPreserve = "1"
			\koorden
		}
	>
	\paper{
		linewidth = -1.;
	}
}
