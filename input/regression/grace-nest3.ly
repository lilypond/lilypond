\version "2.3.22"
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

