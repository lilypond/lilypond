\version "1.5.43"
\header{
    texidoc = "Piano pedal symbols merge stop and start.  The strings are configurable. 
Text style, bracket style, and a mixture of both are supported.  "
}




\score{
    \context Staff \notes\relative c'{

	c4 d e f g
	\sustainDown b c
	c, [d16  c  c c] [e e \sustainUp \sustainDown e e ] f4 \sustainUp 
	g\sustainDown  b \sustainUp c 
	\property Staff.pedalSustainStrings = #'("-" "-P" "P")
	\property Staff.SustainPedal \override #'padding = #-2
	c, \sustainDown d e \sustainUp \sustainDown f
	\sustainUp g b c

	\property Staff.SustainPedal \override #'pedal-type = #'bracket

	c4 d e \sustainDown b c c, \sustainUp \sustainDown [d8 c] [e8 e \sustainUp \sustainDown] f4 d
	\sustainUp g \sustainDown b b, \sustainUp c'

	\property Staff.UnaCordaPedal \override #'pedal-type = #'mixed

	c4 d \unaCorda e f g
	b \treCorde c


    }
    \paper{
    }
    \midi{
	\tempo 4 = 60
    }
}
