\version "1.7.18"
\header {
    texidoc = "Another nested grace situation"
    }
    \paper { raggedright= ##t }

\score { \notes \relative c'' {
	f1
    \grace e8 f1
        < { \grace { e8 } f1 } >
}
}

