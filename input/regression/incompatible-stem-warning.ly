\version "2.14.0"

\header {

  texidoc = "
Combine several kinds of stems in parallel voices.
"

}

\new Voice { \time 4/1
	     << c'\breve e'8 >>
	     << c'8 e'\breve >> |
	     << c'\longa e'1 >> |
	     << c'1 e'\longa >> |
	     << c'2 e'1 >>
	     << c'1 e'2 >>
	     << c'2 e'4 >>
	     << c'4 e'2 >>
	     << c'2 e'8 >>
	     << c'8 e'2 >>
}
