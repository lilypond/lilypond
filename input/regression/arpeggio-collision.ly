\version "2.3.17"

\header  {
texidoc = "Arpeggio stays clear of accidentals and flipped note heads."
}

\score{
     \transpose c c' {
	<fis'' g d a>\arpeggio
	<fis, g d a>\arpeggio
	<fis'' g d a>\arpeggio
	}
    \paper {
        raggedright = ##t
	\context{
	    \Staff
	    connectArpeggios = ##t
	}
	}
}


