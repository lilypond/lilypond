\version "2.1.36"

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
	    \StaffContext
	    connectArpeggios = ##t
	}
	}
}


