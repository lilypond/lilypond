\version "2.3.22"

\header  {
texidoc = "Arpeggio stays clear of accidentals and flipped note heads."
}

\score{
     \transpose c c' {
	<fis'' g d a>\arpeggio
	<fis, g d a>\arpeggio
	<fis'' g d a>\arpeggio
	}
    \layout {
        raggedright = ##t
	\context{
	    \Staff
	    connectArpeggios = ##t
	}
	}
}


