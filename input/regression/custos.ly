\version "2.1.7"
\header {
    texidoc = "Custodes in various styles."
}

\score {
    \notes {
	\property Staff.Custos \set #'neutral-position = #4

	\property Staff.Custos \set #'style = #'hufnagel
	c'1^"hufnagel"
	\break < d' a' f''>1

	\property Staff.Custos \set #'style = #'medicaea
	c'1^"medicaea"
	\break < d' a' f''>1

	\property Staff.Custos \set #'style = #'vaticana
	c'1^"vaticana"
	\break < d' a' f''>1

	\property Staff.Custos \set #'style = #'mensural
	c'1^"mensural"
	\break < d' a' f''>1
    }
    \paper {
	\translator {
	    \StaffContext
	    \consists Custos_engraver
	}
	raggedright = ##t
    }
}

