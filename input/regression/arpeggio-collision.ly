\version "2.1.26"

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
	\translator{
	    \StaffContext
	    connectArpeggios = ##t
	}
	}
}


