#(ly:set-option 'old-relative)
\version "1.9.0"
\header {
    texidoc = "Another nested grace situation."
    }
    \paper { raggedright= ##t }

\score { \notes \relative c'' {
	f1
    \grace e8 f1
        < { \grace { e8 } f1 } >
}
}

