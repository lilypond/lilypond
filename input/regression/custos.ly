\version "1.9.1"
\header {
    texidoc = "Custodes in various styles."
}

\score {
    \notes {
	\property Staff.Custos \set #'neutral-position = #4
	\property Staff.Custos \set #'neutral-direction = #-1
	\property Staff.Custos \set #'adjust-if-on-staffline = ##t

	\property Staff.Custos \set #'style = #'hufnagel
	c'1^"Custos style = \#'hufnagel"
	\break << d' a' f''>>1

	\property Staff.Custos \set #'style = #'medicaea
	c'1^"Custos style = \#'medicaea"
	\break << d' a' f''>>1

	\property Staff.Custos \set #'style = #'vaticana
	c'1^"Custos style = \#'vaticana"
	\break << d' a' f''>>1

	\property Staff.Custos \set #'style = #'mensural
	c'1^"Custos style = \#'mensural"
	\break << d' a' f''>>1
    }
    \paper {
	\translator {
	    \StaffContext
	    \consists Custos_engraver
	}
	raggedright = ##t
    }
}

