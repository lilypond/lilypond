
\version "2.1.26"
% TODO: does this work?  It doesn't do anything with 1.7.20
% Still does not work. 2.1.26 -HJJ
\header {texidoc="@cindex Embedded scm
You can embed scm functions in your scores.
"}

#(begin (newline)(display "hello world")(newline))\score{
	\notes\relative c'{ c }
\paper{raggedright = ##t}
}


