\version "1.9.8"

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


