\version "2.3.17"
\header {
    texidoc = "In nested syntax, graces are still properly handled."
    }
    \paper { raggedright= ##t }

\score {  \relative c'' {
	f1
    \grace e8 f1
        << { \grace { e8 } f1 } >>
}
}

