\version "2.3.2"

\header  {
texidoc = "Arpeggio stays clear of accidentals and flipped note heads."
}

\score{
    \notes \transpose c c' {
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


