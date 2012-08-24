\version "2.16.0"

\header {

  texidoc = "The fill-line markup command should align texts in
  columns. For example, the characters in the center should form one
  column."

}
\paper {
  line-width = 15\cm
  bookTitleMarkup = \markup {
  \column {
   \fill-line {
   		1
   }
   \fill-line {
   		1
   		2
   }
   \fill-line {
   		1
   		2
   		3
   }
   \fill-line {
   		1
   		2
   		3
   		4
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   		7
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   		7
   		8
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   		7
   		8
   		9
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   		7
   		8
   		9
   		10
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   		7
   		8
   		9
   		10
   		11
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   		7
   		8
   		9
   		10
   		11
   		12
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   		7
   		8
   		9
   		10
   		11
   		12
   		13
   }
   \fill-line {
   		1
   		2
   		3
   		4
   		5
   		6
   		7
   		8
   		9
   		10
   		11
   		12
   		13
   		14
   }
   %{
   \fill-line {
   		abcdefghijklmnopqrstuvwxyz0123456789
   		abcdefghijklmnopqrstuvwxyz0123456789
   		abcdefghijklmnopqrstuvwxyz0123456789
   }
   %}
   
   }
  }
}
\book {
	\score { \new Staff \relative c''{
		\repeat unfold 4 c1
	}
	\layout {}
}}
