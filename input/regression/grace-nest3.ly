\version "2.4.0"
\header {
    texidoc = "In nested syntax, graces are still properly handled."
    }
    \layout { raggedright= ##t }

\score {  \relative c'' {
	f1
    \grace e8 f1
        << { \grace { e8 } f1 } >>
}
}

