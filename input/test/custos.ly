\version "1.5.68"
\header {
    texidoc = "custodes in various styles."
}

\score {
    \notes {
	\property Staff.Custos \set #'neutral-position = #4
	\property Staff.Custos \set #'neutral-direction = #-1
	\property Staff.Custos \set #'adjust-if-on-staffline = ##t

	\property Staff.Custos \set #'style = #'hufnagel
	c'1^"Custos style = \#'hufnagel"
	\break < d'1 a' f''>

	\property Staff.Custos \set #'style = #'medicaea
	c'1^"Custos style = \#'medicaea"
	\break < d'1 a' f''>

	\property Staff.Custos \set #'style = #'vaticana
	c'1^"Custos style = \#'vaticana"
	\break < d'1 a' f''>

	\property Staff.Custos \set #'style = #'mensural
	c'1^"Custos style = \#'mensural"
	\break < d'1 a' f''>
    }
    \paper {
	\translator {
	    \StaffContext
	    \consists Custos_engraver
	}
    }
}
