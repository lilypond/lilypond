\version "2.3.8"


\header {
texidoc = "Modern  transcriptions  of gregrorian music.
Gregorian music has no measure, no stems; it uses only half and quarter notes,
and two types of barlines, a short one indicating a rest, and a second one
indicating a breath mark."
}


barOne =  { \once \override Staff.BarLine  #'bar-size = #2
	\bar "|" }
barTwo =  { \once \override Staff.BarLine  #'extra-offset = #'(0 . 2)

		\once \override Staff.BarLine  #'bar-size = #2
	\bar "|" }
\score {

 \relative c' {
	\set Score.timing = ##f
	\override Staff.Stem  #'transparent = ##t
	f4 a2 \barTwo g4 a2  f2 \barOne g4( f) f(
	\bar "empty" 
	\break
	g) a2
}

}
