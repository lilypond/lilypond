\version "2.2.0"
\header {
    texidoc = "In nested syntax, graces are still properly handled."
    }
    \paper { raggedright= ##t }

\score { \notes \relative c'' {
	f1
    \grace e8 f1
        << { \grace { e8 } f1 } >>
}
}

