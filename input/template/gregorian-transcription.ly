\version "2.1.7"


\header {
texidoc = "Modern  transcriptions  of gregrorian music.
Gregorian music has no measure, no stems; it uses only half and quarter notes,
and two types of barlines, a short one indicating a rest, and a second one
indicating a breath mark."
}


barOne = \notes { \once \property Staff.BarLine \override #'bar-size = #2
	\bar "|" }
barTwo = \notes { \once \property Staff.BarLine \override #'extra-offset = #'(0 . 2)

		\once \property Staff.BarLine \override #'bar-size = #2
	\bar "|" }
\score {

\notes \relative c' {
	\property Score.timing = ##f
	\property Staff.Stem \set #'transparent = ##t
	f4 a2 \barTwo g4 a2  f2 \barOne g4( f) f(
	\bar "empty" 
	\break
	g) a2
}

}
